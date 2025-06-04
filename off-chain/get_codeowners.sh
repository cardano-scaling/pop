#!/bin/sh

# Usage: ./script.sh <git-repo-url> <commit-hash>

set -e

REPO_URL="$1"
COMMIT_HASH="$2"

if [ -z "$REPO_URL" ] || [ -z "$COMMIT_HASH" ]; then
  echo "Usage: $0 <git-repo-url> <commit-hash>"
  exit 1
fi

# Extract repo name from URL for local clone folder
REPO_NAME=$(basename -s .git "$REPO_URL")

# Clone repo if not already present
if [ ! -d "$REPO_NAME" ]; then
  git clone "$REPO_URL"
fi

cd "$REPO_NAME"

# Fetch all commits to ensure the commit hash is available
git fetch --all

# Checkout the specified commit in detached HEAD mode
git checkout "$COMMIT_HASH"

# Look for CODEOWNERS file in common locations
if [ -f "CODEOWNERS" ]; then
  CODEOWNERS_PATH="CODEOWNERS"
elif [ -f ".github/CODEOWNERS" ]; then
  CODEOWNERS_PATH=".github/CODEOWNERS"
elif [ -f "docs/CODEOWNERS" ]; then
  CODEOWNERS_PATH="docs/CODEOWNERS"
else
  echo "CODEOWNERS file not found in the repository at commit $COMMIT_HASH."
  cd ..
  rm -rf "$REPO_NAME"
  exit 2
fi

echo "Displaying CODEOWNERS file from commit $COMMIT_HASH:"
echo "--------------------------------------------------"
cat "$CODEOWNERS_PATH"

# Go back to parent directory and delete the cloned repo
cd ..
rm -rf "$REPO_NAME"

