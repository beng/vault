#!/bin/bash
# setup-worktrees.sh - Create isolated worktrees for parallel agents

set -e

# Configuration
AGENT_COUNT=${1:-10}
WORKTREE_DIR="../worktrees"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${GREEN}=== Setting up $AGENT_COUNT agent worktrees ===${NC}"
echo ""

# Check we're in a git repo
if ! git rev-parse --git-dir > /dev/null 2>&1; then
  echo -e "${RED}Error: Not in a git repository${NC}"
  exit 1
fi

# Check for uncommitted changes
if ! git diff-index --quiet HEAD --; then
  echo -e "${YELLOW}Warning: You have uncommitted changes${NC}"
  read -p "Continue anyway? (y/n) " -n 1 -r
  echo
  if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    exit 1
  fi
fi

# Create worktree directory
mkdir -p "$WORKTREE_DIR"

# Get current branch (usually main)
BASE_BRANCH=$(git branch --show-current)
echo "Base branch: $BASE_BRANCH"
echo ""

# Create worktrees
for i in $(seq -f "%02g" 1 $AGENT_COUNT); do
  AGENT_DIR="$WORKTREE_DIR/agent-$i"
  
  if [ -d "$AGENT_DIR" ]; then
    echo -e "${YELLOW}Skipping agent-$i (already exists)${NC}"
  else
    echo -n "Creating agent-$i... "
    git worktree add "$AGENT_DIR" -b "workspace/agent-$i" "$BASE_BRANCH" --quiet
    echo -e "${GREEN}done${NC}"
  fi
done

echo ""
echo -e "${GREEN}=== Setup Complete ===${NC}"
echo ""
echo "Worktrees created in: $WORKTREE_DIR"
echo ""
git worktree list
echo ""
echo "Next steps:"
echo "  1. Create your spec file (spec.md)"
echo "  2. Run: /project:decompose-spec @spec.md"
echo "  3. Review and approve task breakdown"
echo "  4. Run: /project:spawn-workers"
echo "  5. Monitor: /project:check-status"
