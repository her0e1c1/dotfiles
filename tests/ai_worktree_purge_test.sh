#!/usr/bin/env bash
set -euo pipefail

DOTFILES_ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
PROFILE_FILE="${DOTFILES_ROOT}/.profile"

fail() {
  echo "FAIL: $*" >&2
  exit 1
}

assert_contains() {
  local haystack="$1"
  local needle="$2"
  if [[ "$haystack" != *"$needle"* ]]; then
    fail "expected output to contain '$needle'"
  fi
}

assert_not_exists() {
  local path="$1"
  if [ -e "$path" ]; then
    fail "expected path to be removed: $path"
  fi
}

assert_ref_exists() {
  local repo="$1"
  local ref="$2"
  if ! git -C "$repo" show-ref --verify --quiet "$ref"; then
    fail "expected git ref to exist: $ref"
  fi
}

assert_worktree_count() {
  local repo="$1"
  local expected="$2"
  local count
  count=$(git -C "$repo" worktree list --porcelain | grep -c '^worktree ')
  if [ "$count" -ne "$expected" ]; then
    fail "expected $expected worktrees, got $count"
  fi
}

make_repo() {
  local repo
  repo=$(mktemp -d)
  git init -b master "$repo" >/dev/null
  git -C "$repo" config user.name "Test User"
  git -C "$repo" config user.email "test@example.com"
  echo "base" >"${repo}/tracked.txt"
  git -C "$repo" add tracked.txt
  git -C "$repo" commit -m "init" >/dev/null
  printf '%s\n' "$repo"
}

run_profile_command() {
  local repo="$1"
  local script="$2"
  bash --noprofile --norc -c '
    set -eo pipefail
    repo="$1"
    profile_file="$2"
    script="$3"
    export PATH_ADDITIONAL=""
    cd "$repo"
    source "$profile_file"
    set -u
    eval "$script"
  ' bash "$repo" "$PROFILE_FILE" "$script"
}

test_no_secondary_worktrees() {
  local repo
  repo=$(make_repo)
  trap 'rm -rf "$repo"' RETURN

  local output
  output=$(run_profile_command "$repo" "ai_worktree_purge")

  assert_contains "$output" "No secondary worktrees to purge."
  assert_worktree_count "$repo" 1

  trap - RETURN
  rm -rf "$repo"
}

test_purge_removes_secondary_worktrees_only() {
  local repo
  repo=$(make_repo)
  trap 'rm -rf "$repo"' RETURN

  mkdir -p "${repo}/.worktrees"
  git -C "$repo" worktree add -b feature-one "${repo}/.worktrees/one" HEAD >/dev/null
  git -C "$repo" worktree add -b feature-two "${repo}/.worktrees/two" HEAD >/dev/null

  local output
  output=$(run_profile_command "$repo" "printf 'y\n' | ai_worktree_purge")

  assert_contains "$output" "${repo}/.worktrees/one"
  assert_contains "$output" "${repo}/.worktrees/two"
  assert_worktree_count "$repo" 1
  assert_not_exists "${repo}/.worktrees/one"
  assert_not_exists "${repo}/.worktrees/two"
  assert_ref_exists "$repo" "refs/heads/feature-one"
  assert_ref_exists "$repo" "refs/heads/feature-two"

  trap - RETURN
  rm -rf "$repo"
}

test_cancel_keeps_secondary_worktrees() {
  local repo
  repo=$(make_repo)
  trap 'rm -rf "$repo"' RETURN

  mkdir -p "${repo}/.worktrees"
  git -C "$repo" worktree add -b feature-keep "${repo}/.worktrees/keep" HEAD >/dev/null

  if run_profile_command "$repo" "printf 'n\n' | ai_worktree_purge" >/dev/null; then
    fail "expected cancellation to return non-zero"
  fi

  assert_worktree_count "$repo" 2
  assert_ref_exists "$repo" "refs/heads/feature-keep"

  trap - RETURN
  rm -rf "$repo"
}

test_no_secondary_worktrees
test_purge_removes_secondary_worktrees_only
test_cancel_keeps_secondary_worktrees

echo "PASS: ai_worktree_purge"
