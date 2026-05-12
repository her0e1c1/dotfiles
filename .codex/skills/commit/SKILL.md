---
name: commit
description: 現在 staged されている git diff を確認し、staged 変更だけに基づく簡潔なコミットメッセージを作成して git commit を実行する。ユーザーが staged 変更のコミット、または staged diff 用のコミットメッセージ作成を依頼したときに使う。
---

# Commit

現在の staged diff からコミットメッセージを作成し、通常は `git commit` も実行するタスクでこの skill を使う。

## ワークフロー

1. staged 変更だけを確認する。
   - `git status --short` を実行して、何が staged されているか確認する。
   - `git diff --cached --stat` を実行して、簡潔な要約を見る。
   - `git diff --cached` を実行して、実際の staged patch を読む。
2. ユーザーが明示的に触れるよう依頼しない限り、unstaged 変更と untracked 変更は完全に無視する。
3. staged diff だけを反映したコミットメッセージを書く。
   - subject は命令形を優先する。
   - subject は 72 文字以内に収める。
   - ユーザー影響や保守上の影響がある詳細を補足する必要がある場合だけ body を追加する。
4. ユーザーがメッセージ案だけを依頼していない限り、生成したメッセージで `git commit` を実行する。

## ガードレール

- staged diff に存在しない変更を作り上げない。
- コミットメッセージで unstaged 変更に言及しない。
- staged されているものが何もない場合は、そのことを明確に報告し、`git commit` は実行しない。
- sandbox や権限で `git commit` がブロックされた場合は、メッセージ案で止まらず、必要な escalation で再試行する。
- hooks や Git エラーでコミットがブロックされた場合は、正確な blocker を報告し、失敗理由として必要な場合を除いてメッセージを書き換えない。
