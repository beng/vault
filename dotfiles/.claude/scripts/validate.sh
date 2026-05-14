#!/bin/bash
# validate.sh - Run all validation checks
# Workers MUST run this before every commit

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo "=== Running Validation ==="
echo ""

FAILED=0

# Detect project type and run appropriate checks
# Customize this for your stack

# --- Node.js / TypeScript ---
if [ -f "package.json" ]; then
  
  # Lint
  if grep -q '"lint"' package.json; then
    echo -n "Linting... "
    if npm run lint --silent 2>/dev/null; then
      echo -e "${GREEN}✓${NC}"
    else
      echo -e "${RED}✗ Lint failed${NC}"
      FAILED=1
    fi
  fi
  
  # TypeScript
  if grep -q '"typecheck"' package.json; then
    echo -n "Type checking... "
    if npm run typecheck --silent 2>/dev/null; then
      echo -e "${GREEN}✓${NC}"
    else
      echo -e "${RED}✗ TypeScript errors${NC}"
      FAILED=1
    fi
  elif [ -f "tsconfig.json" ]; then
    echo -n "Type checking... "
    if npx tsc --noEmit 2>/dev/null; then
      echo -e "${GREEN}✓${NC}"
    else
      echo -e "${RED}✗ TypeScript errors${NC}"
      FAILED=1
    fi
  fi
  
  # Tests
  if grep -q '"test"' package.json; then
    echo -n "Running tests... "
    if npm test --silent 2>/dev/null; then
      echo -e "${GREEN}✓${NC}"
    else
      echo -e "${RED}✗ Tests failed${NC}"
      FAILED=1
    fi
  fi
  
  # Build (optional - uncomment if you want build validation)
  # if grep -q '"build"' package.json; then
  #   echo -n "Building... "
  #   if npm run build --silent 2>/dev/null; then
  #     echo -e "${GREEN}✓${NC}"
  #   else
  #     echo -e "${RED}✗ Build failed${NC}"
  #     FAILED=1
  #   fi
  # fi

fi

# --- Python ---
if [ -f "pyproject.toml" ] || [ -f "setup.py" ] || [ -f "requirements.txt" ]; then
  
  # Ruff or flake8
  if command -v ruff &> /dev/null; then
    echo -n "Linting (ruff)... "
    if ruff check . 2>/dev/null; then
      echo -e "${GREEN}✓${NC}"
    else
      echo -e "${RED}✗ Lint errors${NC}"
      FAILED=1
    fi
  elif command -v flake8 &> /dev/null; then
    echo -n "Linting (flake8)... "
    if flake8 . 2>/dev/null; then
      echo -e "${GREEN}✓${NC}"
    else
      echo -e "${RED}✗ Lint errors${NC}"
      FAILED=1
    fi
  fi
  
  # Type checking
  if command -v mypy &> /dev/null && [ -f "mypy.ini" ]; then
    echo -n "Type checking (mypy)... "
    if mypy . 2>/dev/null; then
      echo -e "${GREEN}✓${NC}"
    else
      echo -e "${RED}✗ Type errors${NC}"
      FAILED=1
    fi
  fi
  
  # Tests
  if command -v pytest &> /dev/null; then
    echo -n "Running tests (pytest)... "
    if pytest --quiet 2>/dev/null; then
      echo -e "${GREEN}✓${NC}"
    else
      echo -e "${RED}✗ Tests failed${NC}"
      FAILED=1
    fi
  fi
  
fi

# --- Rust ---
if [ -f "Cargo.toml" ]; then
  echo -n "Checking (cargo)... "
  if cargo check --quiet 2>/dev/null; then
    echo -e "${GREEN}✓${NC}"
  else
    echo -e "${RED}✗ Cargo check failed${NC}"
    FAILED=1
  fi
  
  echo -n "Running tests... "
  if cargo test --quiet 2>/dev/null; then
    echo -e "${GREEN}✓${NC}"
  else
    echo -e "${RED}✗ Tests failed${NC}"
    FAILED=1
  fi
fi

# --- Go ---
if [ -f "go.mod" ]; then
  echo -n "Vetting... "
  if go vet ./... 2>/dev/null; then
    echo -e "${GREEN}✓${NC}"
  else
    echo -e "${RED}✗ Go vet failed${NC}"
    FAILED=1
  fi
  
  echo -n "Running tests... "
  if go test ./... 2>/dev/null; then
    echo -e "${GREEN}✓${NC}"
  else
    echo -e "${RED}✗ Tests failed${NC}"
    FAILED=1
  fi
fi

echo ""

if [ $FAILED -eq 0 ]; then
  echo -e "${GREEN}=== All checks passed ✓ ===${NC}"
  exit 0
else
  echo -e "${RED}=== Validation failed ✗ ===${NC}"
  echo "Fix errors before committing."
  exit 1
fi
