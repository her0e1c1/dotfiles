# Commit Prompt

Review the current staged git diff and create a concise commit message, then run `git commit`.

Requirements:
- Summarize the change based only on the staged diff
- Prefer an imperative commit subject
- Keep the subject within 72 characters
- Mention notable user-facing or maintenance-impacting changes in the body only when needed
- Do not invent changes that are not present in the diff
- Ignore unstaged changes completely; they are out of scope for the commit
- Execute `git commit` without asking the user for additional confirmation
- Execute `git commit` with the generated message instead of only printing it
