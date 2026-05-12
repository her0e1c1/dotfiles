---
name: create-pr
description: 現在の branch から GitHub pull request を作成し、公開に必要な最小限の `git` と `gh` 操作だけに作業範囲を限定すべきときに使う。
---

# Create PR

## 概要

現在の branch を、範囲を絞った GitHub pull request にする。`git` と `gh` だけを使い、branch を公開するために必要な作業だけを行う。完了までの許可された経路は `git add`、`git commit`、`git push`、`gh pr create` とする。

## ガードレール

- `git` と `gh` だけを使う。
- タスク範囲を PR 作成に限定する。tests、refactors、formatting passes、dependency changes、無関係な cleanup は実行しない。
- PR 公開に必要な場合は、`git add`、`git commit`、`git push` を許可する。
- ユーザーが明示的に依頼しない限り、既存履歴の書き換え、branch の restack、無関係なローカル作業の整理はしない。
- ローカル変更の扱いが曖昧な場合は、気を利かせようとせず、止まって確認する。

## ワークフロー

1. 現在の Git context を読む。
   - branch 名と tracking branch
   - staged、unstaged、untracked 変更
   - remote の有無と `gh` の有無
2. base branch を決める。
   - まず `git config branch.<current-branch>.base` を確認し、設定されている場合は優先する。
   - `main`、`master`、`develop` などの有力候補を比較する。
   - merge-base と fork-point の証拠を優先する。
   - upstream と最近の branch context は補助的な signal としてだけ使う。
   - 強い候補が 1 つに絞れない場合だけユーザーに確認する。
3. 未コミット変更を確認する。
   - この PR に明らかに属する作業だけを含める。
   - 無関係なローカル変更は触らず残す。
   - 境界が曖昧な場合は止まってユーザーに確認する。
4. 必要な commit を作る。
   - この PR に明らかに属する変更にだけ `git add` を使う。
   - 既存履歴を保つ。
   - branch 公開に必要な新規 commit だけを追加する。
   - commit cleanup より、必要最小限の commit 作業を優先する。
5. `base...HEAD` を要約し、branch の主目的を特定する。
6. PR metadata を生成する。
   - repository の PR template があれば合わせる。
   - なければ `## Summary` と `## Testing` を持つ簡潔な body にする。
7. 必要なら upstream を設定しつつ、現在の branch を push する。
8. `gh pr create` を実行する。
9. 選択した base branch、新規 commit の有無、PR title、PR URL を含む簡潔な結果 summary を返す。

## 判断ルール

### Base Branch

- `git config branch.<current-branch>.base` が存在する場合は優先する。
- 一般的な integration branch との merge-base と fork-point 関係を優先する。
- 設定済み upstream や remote default branch は、その証拠が commit graph と一致する場合だけ使う。
- 最近の branch history や naming は primary proof ではなく tie-breaker として扱う。
- 複数候補が同程度に妥当なまま残る場合はユーザーに確認する。

### 未コミット変更

- 何かを commit する前に、staged、unstaged、untracked 変更を確認する。
- ファイル状態だけでなく、branch の目的に基づいて判断する。
- PR に明らかに属する変更を含める。
- 無関係な変更は working tree に残す。
- ある変更がこの PR に属する可能性も別 follow-up に属する可能性もある場合は、ユーザーに確認する。

### Commit 作成

- この PR に明らかに属する変更だけを `git add` する。
- 現在の branch history を保つ。
- PR-ready な履歴に必要な新規 commit だけを追加する。
- 実用的な commit message を生成し、不要な commit 分割は避ける。
- ユーザーが別途 history cleanup を依頼しない限り、過去 commit を書き換えない。

### Scope Discipline

- Git state を確認し、関連変更を add し、必要なら commit し、push して PR を作成する狭い経路に留まる。
- 役に立ちそうというだけで追加の repository maintenance をしない。
- タスクを validation、cleanup、follow-up implementation work に広げない。

## 停止条件

次のいずれかに該当する場合は停止し、blocker を説明する。

- detached `HEAD`
- push に適した remote がない
- `gh` が利用できない
- 権限、認証、remote state により push が失敗する
- base branch の推定が曖昧なまま残る
- ローカル変更に、安全に推定できない曖昧な scope が含まれている

title と body を生成した後に PR 作成が失敗した場合は、選択した base branch、生成した title、生成した body、次に手動で実行する command を返す。
