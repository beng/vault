#!/bin/bash
# cleanup-worktrees.sh - Remove worktrees and branches after merge

set -e

WORKTREE_DIR="../worktrees"

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo -e "${YELLOW}=== Cleaning Up Worktrees ===${NC}"
echo ""

# List current worktrees
echo "Current worktrees:"
git worktree list
echo ""

# Check which branches are merged
echo "Merged task branches (safe to remove):"
for branch in $(git branch | grep "workspace/agent-" | sed 's/^[ *]*//'); do
  if git branch --merged main | grep -q "$branch"; then
    echo -e "  ${GREEN}✓ $branch${NC}"
  fi
done
echo ""

echo "Unmerged branches (will skip):"
for branch in $(git branch | grep "workspace/agent-" | sed 's/^[ *]*//'); do
  if ! git branch --merged main | grep -q "$branch"; then
    echo -e "  ${YELLOW}⚠ $branch${NC}"
  fi
done
echo ""

read -p "Remove all merged worktrees? (y/n) " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
  echo "Cancelled."
  exit 0
fi

echo ""

# Remove worktrees for merged branches only
for dir in $WORKTREE_DIR/agent-*; do
  if [ -d "$dir" ]; then
    agent=$(basename $dir)
    branch="workspace/$agent"
    
    # Check if merged
    if git branch --merged main | grep -q "$branch"; then
      echo -n "Removing $agent... "
      git worktree remove "$dir" --force 2>/dev/null || true
      git branch -d "$branch" 2>/dev/null || true
      echo -e "${GREEN}done${NC}"
    else
      echo -e "${YELLOW}Skipping $agent (branch not merged)${NC}"
    fi
  fi
done

# Prune any stale worktree references
git worktree prune

echo ""
echo -e "${GREEN}=== Cleanup Complete ===${NC}"
echo ""
echo "Remaining worktrees:"
git worktree list

# Clean up empty worktree directory
if [ -d "$WORKTREE_DIR" ] && [ -z "$(ls -A $WORKTREE_DIR)" ]; then
  rmdir "$WORKTREE_DIR"
  echo ""
  echo "Removed empty worktree directory."
fi
