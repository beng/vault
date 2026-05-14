#!/usr/bin/env bash
# Claude Code status line -- reads JSON on stdin
# Features: model, dir, branch, subscription usage (5h/7d), pace tracking,
# overuse billing, time-to-reset, auto line wrapping

# --- Catppuccin Mocha (truecolor) ---
# Palette: https://catppuccin.com/palette
FG_TEXT="\033[38;2;205;214;244m"   # text (#cdd6f4)
FG_MODEL="\033[38;2;249;226;175m"  # yellow (#f9e2af)
FG_DIR="\033[38;2;137;180;250m"    # blue (#89b4fa)
FG_BRANCH="\033[38;2;166;227;161m" # green (#a6e3a1)
FG_SEP="\033[38;2;124;130;146m"    # overlay1 (#7f849c)
FG_WARN="\033[38;2;250;179;135m"   # peach (#fab387) -- ahead of pace
FG_CRIT="\033[38;2;243;139;168m"   # red (#f38ba8) -- way ahead / high usage
FG_OK="\033[38;2;148;226;213m"     # teal (#94e2d5) -- on track / low usage
RESET="\033[0m"

# --- Configuration ---
PACE_AHEAD_THRESHOLD=10
PACE_BEHIND_THRESHOLD=25
USAGE_ALWAYS_SHOW=90

# --- Parse input JSON ---
input=$(cat)

MODEL_DISPLAY=$(jq -r '.model.display_name // empty' <<<"$input")
CURRENT_DIR=$(jq -r '.workspace.current_dir // empty' <<<"$input")
total=$(jq -r '.context_window.context_window_size // 200000' <<<"$input")
pct=$(jq -r '.context_window.used_percentage // 0' <<<"$input" | cut -d. -f1)
used=$(( total * pct / 100 ))

# --- Format token counts ---
if [ "$used" -ge 1000000 ]; then
  used_fmt="$(( used / 1000000 ))M"
elif [ "$used" -ge 1000 ]; then
  used_fmt="$(( used / 1000 ))k"
else
  used_fmt="$used"
fi
if [ "$total" -ge 1000000 ]; then
  total_fmt="$(( total / 1000000 ))M"
else
  total_fmt="$(( total / 1000 ))k"
fi

# Context percentage color (Catppuccin mapped)
if [ "$pct" -ge 80 ] 2>/dev/null; then
  pct_color="$FG_CRIT"
elif [ "$pct" -ge 50 ] 2>/dev/null; then
  pct_color="$FG_WARN"
else
  pct_color="$FG_OK"
fi

# --- Helpers ---

# Compact time-to-reset (e.g. "30m", "4.5h", "2.3d")
fmt_reset() {
  local reset_epoch=$1
  if [ -n "$reset_epoch" ] && [ "$reset_epoch" -gt 0 ] 2>/dev/null; then
    local remaining=$(( reset_epoch - now ))
    [ "$remaining" -lt 0 ] && remaining=0
    if [ "$remaining" -lt 3600 ]; then
      echo "$((remaining / 60))m"
    elif [ "$remaining" -lt 86400 ]; then
      local h=$((remaining / 3600))
      local m=$(( (remaining % 3600) / 60 ))
      if [ "$m" -ge 45 ]; then echo "$(( h + 1 ))h"
      elif [ "$m" -ge 15 ]; then echo "${h}.5h"
      else echo "${h}h"; fi
    else
      local d=$((remaining / 86400))
      local h=$(( (remaining % 86400) / 3600 ))
      if [ "$h" -ge 12 ]; then echo "$(( d + 1 ))d"
      else echo "${d}.$(( h * 10 / 24 ))d"; fi
    fi
  fi
}

# Pace calculation: how far ahead/behind expected usage for a window
# Positive = burning fast, negative = on track
# Args: usage_pct reset_epoch window_seconds
# Output: "ahead_pct:color_code"
claude_pace() {
  local usage=$1 reset_epoch=$2 window_secs=$3
  local ahead=0
  if [ "$reset_epoch" -gt 0 ] 2>/dev/null; then
    local remaining=$(( reset_epoch - now ))
    [ "$remaining" -lt 0 ] && remaining=0
    local elapsed=$(( window_secs - remaining ))
    [ "$elapsed" -lt 0 ] && elapsed=0
    local expected=$(( elapsed * 100 / window_secs ))
    ahead=$(( usage - expected ))
  fi
  local color
  if [ "$ahead" -ge 20 ] 2>/dev/null; then color="$FG_CRIT"
  elif [ "$ahead" -ge 10 ] 2>/dev/null; then color="$FG_WARN"
  else color="$FG_OK"; fi
  echo "${ahead}:${color}"
}

# --- Build segments ---
now=$(date +%s)
cols=$(tput cols 2>/dev/null || echo 80)

DIR_DISPLAY=$(basename "$CURRENT_DIR")
BRANCH=$(git -C "$CURRENT_DIR" branch --show-current 2>/dev/null || true)

# Git dirty indicators
git_indicators=""
if [ -n "$BRANCH" ] && [ -d "$CURRENT_DIR/.git" ]; then
  git_status=$(git -C "$CURRENT_DIR" status --porcelain 2>/dev/null | head -20)
  if [ -n "$git_status" ]; then
    echo "$git_status" | grep -q '^.[MD]' && git_indicators="${git_indicators}!"
    echo "$git_status" | grep -q '^??' && git_indicators="${git_indicators}?"
    echo "$git_status" | grep -q '^[MADRC]' && git_indicators="${git_indicators}+"
  fi
fi

# Segment: model
seg_model_plain="[${MODEL_DISPLAY}]"
seg_model_color="${FG_MODEL}[${MODEL_DISPLAY}]${RESET}"

# Segment: context -- expand to show tokens only when it starts mattering
if [ "$pct" -ge 50 ] 2>/dev/null; then
  seg_ctx_plain=" ctx ${used_fmt}/${total_fmt} ${pct}%"
  seg_ctx_color=" ${FG_TEXT}ctx${RESET} ${pct_color}${used_fmt}/${total_fmt} ${pct}%${RESET}"
else
  seg_ctx_plain=" ctx ${pct}%"
  seg_ctx_color=" ${FG_TEXT}ctx${RESET} ${pct_color}${pct}%${RESET}"
fi

# Segment: dir
seg_dir_plain=" ${DIR_DISPLAY}"
seg_dir_color=" ${FG_DIR}${DIR_DISPLAY}${RESET}"

# Segment: branch
seg_branch_plain=""
seg_branch_color=""
if [ -n "$BRANCH" ]; then
  if [ -n "$git_indicators" ]; then
    seg_branch_plain=" [${BRANCH} ${git_indicators}]"
    seg_branch_color=" ${FG_BRANCH}[${BRANCH}${RESET} ${FG_WARN}${git_indicators}${RESET}${FG_BRANCH}]${RESET}"
  else
    seg_branch_plain=" [${BRANCH}]"
    seg_branch_color=" ${FG_BRANCH}[${BRANCH}]${RESET}"
  fi
fi

# --- Claude subscription usage (direct, not proxied) ---
# Cache: five_pct:seven_pct:five_reset_epoch:seven_reset_epoch:extra_used:extra_limit
usage_str=""
usage_str_plain=""
cache_file="/tmp/claude-usage-cache"
cache_max_age=120

if [ -f "$cache_file" ] && [ "$(( now - $(stat -f%m "$cache_file") ))" -lt "$cache_max_age" ]; then
  claude_usage=$(cat "$cache_file")
else
  # Background fetch so we don't block the status line
  (
    token=$(security find-generic-password -s "Claude Code-credentials" -w 2>/dev/null \
      | jq -r '.claudeAiOauth.accessToken // empty' 2>/dev/null)
    if [ -n "$token" ]; then
      resp=$(curl -s --max-time 5 'https://api.anthropic.com/api/oauth/usage' \
        -H "Authorization: Bearer $token" \
        -H "anthropic-beta: oauth-2025-04-20" \
        -H "Content-Type: application/json" 2>/dev/null)
      five=$(echo "$resp" | jq -r '.five_hour.utilization // empty' 2>/dev/null | cut -d. -f1)
      seven=$(echo "$resp" | jq -r '.seven_day.utilization // empty' 2>/dev/null | cut -d. -f1)
      five_reset=$(echo "$resp" | jq -r '.five_hour.resets_at // empty' 2>/dev/null)
      seven_reset=$(echo "$resp" | jq -r '.seven_day.resets_at // empty' 2>/dev/null)
      if [ -n "$five" ] && [ -n "$seven" ]; then
        five_epoch=$(TZ=UTC date -j -f "%Y-%m-%dT%H:%M:%S" "$(echo "$five_reset" | cut -d. -f1)" +%s 2>/dev/null || echo "0")
        seven_epoch=$(TZ=UTC date -j -f "%Y-%m-%dT%H:%M:%S" "$(echo "$seven_reset" | cut -d. -f1)" +%s 2>/dev/null || echo "0")
        extra_used=$(echo "$resp" | jq -r '.extra_usage.used_credits // 0' 2>/dev/null | cut -d. -f1)
        extra_limit=$(echo "$resp" | jq -r '.extra_usage.monthly_limit // 0' 2>/dev/null | cut -d. -f1)
        echo "${five}:${seven}:${five_epoch}:${seven_epoch}:${extra_used}:${extra_limit}" > "$cache_file"
      fi
    fi
  ) &
  [ -f "$cache_file" ] && claude_usage=$(cat "$cache_file")
fi

if [ -n "$claude_usage" ]; then
  five_pct=$(echo "$claude_usage" | cut -d: -f1)
  seven_pct=$(echo "$claude_usage" | cut -d: -f2)
  five_reset_epoch=$(echo "$claude_usage" | cut -d: -f3)
  seven_reset_epoch=$(echo "$claude_usage" | cut -d: -f4)
  extra_used=$(echo "$claude_usage" | cut -d: -f5)
  extra_limit=$(echo "$claude_usage" | cut -d: -f6)
  [ -z "$extra_used" ] && extra_used=0
  [ -z "$extra_limit" ] && extra_limit=0

  # --- 5h sub-field ---
  # Format: "5h N%" always, "5h N% (Xh)" when >=50% used or burning fast.
  # Color encodes pace (teal/peach/red) via claude_pace().
  five_part=""
  five_part_plain=""
  if [ -n "$five_pct" ] && [ "$five_pct" -ge 0 ] 2>/dev/null; then
    five_result=$(claude_pace "$five_pct" "$five_reset_epoch" 18000)
    five_ahead=$(echo "$five_result" | cut -d: -f1)
    fc=$(echo "$five_result" | cut -d: -f2-)
    five_text="5h ${five_pct}%"
    if [ "$five_pct" -ge 50 ] 2>/dev/null || \
       [ "$five_ahead" -ge "$PACE_AHEAD_THRESHOLD" ] 2>/dev/null; then
      five_reset_str=$(fmt_reset "$five_reset_epoch")
      [ -n "$five_reset_str" ] && five_text="${five_text} (${five_reset_str})"
    fi
    five_part="${fc}${five_text}${RESET}"
    five_part_plain="${five_text}"
  fi

  # --- 7d sub-field ---
  seven_part=""
  seven_part_plain=""
  if [ -n "$seven_pct" ] && [ "$seven_pct" -ge 0 ] 2>/dev/null; then
    seven_result=$(claude_pace "$seven_pct" "$seven_reset_epoch" 604800)
    seven_ahead=$(echo "$seven_result" | cut -d: -f1)
    sc=$(echo "$seven_result" | cut -d: -f2-)
    seven_text="7d ${seven_pct}%"
    if [ "$seven_pct" -ge 50 ] 2>/dev/null || \
       [ "$seven_ahead" -ge "$PACE_AHEAD_THRESHOLD" ] 2>/dev/null; then
      seven_reset_str=$(fmt_reset "$seven_reset_epoch")
      [ -n "$seven_reset_str" ] && seven_text="${seven_text} (${seven_reset_str})"
    fi
    seven_part="${sc}${seven_text}${RESET}"
    seven_part_plain="${seven_text}"
  fi

  # --- Compose the burn group: "burn: 5h N%, 7d N%" ---
  # Grouped under one label because both sub-fields measure the same thing
  # (account-wide quota) at different horizons, as opposed to ctx which is
  # per-conversation. Keeps the line parseable as three groups, not five fields.
  if [ -n "$five_part" ] || [ -n "$seven_part" ]; then
    if [ -n "$five_part" ] && [ -n "$seven_part" ]; then
      inner="${five_part}${FG_SEP},${RESET} ${seven_part}"
      inner_plain="${five_part_plain}, ${seven_part_plain}"
    elif [ -n "$five_part" ]; then
      inner="${five_part}"
      inner_plain="${five_part_plain}"
    else
      inner="${seven_part}"
      inner_plain="${seven_part_plain}"
    fi
    usage_str="${usage_str} ${FG_SEP}|${RESET} ${FG_TEXT}burn:${RESET} ${inner}"
    usage_str_plain="${usage_str_plain} | burn: ${inner_plain}"
  fi

  # --- Overage billing: only when 5h quota is exhausted and extra is enabled ---
  if [ "$five_pct" -ge 100 ] 2>/dev/null && [ "$extra_limit" -gt 0 ] 2>/dev/null; then
    if [ "$extra_used" -ge 100 ]; then
      extra_used_fmt="\$$(( extra_used / 100 ))"
    else
      extra_used_fmt="\$0"
    fi
    extra_limit_fmt="\$$(( extra_limit / 100 ))"
    extra_pct=$(( extra_used * 100 / extra_limit ))
    if   [ "$extra_pct" -ge 80 ] 2>/dev/null; then ec="$FG_CRIT"
    elif [ "$extra_pct" -ge 50 ] 2>/dev/null; then ec="$FG_WARN"
    else                                            ec="$FG_OK"; fi
    usage_str="${usage_str} ${FG_SEP}|${RESET} ${ec}over ${extra_used_fmt}/${extra_limit_fmt}${RESET}"
    usage_str_plain="${usage_str_plain} | over ${extra_used_fmt}/${extra_limit_fmt}"
  fi
fi

# --- Compose and auto-wrap ---
sep=" ${FG_SEP}|${RESET} "
sep_plain=" | "

line1_plain="${seg_model_plain}${sep_plain}${seg_dir_plain}${seg_branch_plain}${sep_plain}${seg_ctx_plain}${usage_str_plain}"

if [ ${#line1_plain} -le "$cols" ]; then
  printf "%b" "${seg_model_color}${sep}${seg_dir_color}${seg_branch_color}${sep}${seg_ctx_color}${usage_str}"
else
  # Line 1: model | dir [branch]
  printf "%b\n" "${seg_model_color}${sep}${seg_dir_color}${seg_branch_color}"
  # Line 2: context + usage
  printf "%b" "${seg_ctx_color}${usage_str}"
fi
