---
name: plan-new-feature
description: リポジトリを広く調査し、利用可能なローカル情報を統合したうえで、実装はせずに、絞り込んだ案と広めの案に分けて新機能アイデアを提案する必要があるときに使う。
---

# Plan New Feature

## 概要

リポジトリを広く調査し、現在すでに何をしているかを理解したうえで、現状に合う新機能アイデアを提案する。実現しやすさとリポジトリへの適合度で絞り込んだ section と、より広い可能性の section の 2 つを返す。

リポジトリに複数の subsystem がある場合、または documentation が薄い場合は、開始前に `references/checklist.md` を読む。

## ワークフロー

1. 見つけられる有用な local signal をすべて読む。
   - README files
   - docs
   - config files
   - scripts
   - key entry points
   - recent commits
2. リポジトリの現在の目的、workflow、強み、gap を推定する。
3. local context に合う新機能アイデアを作る。
4. 出力を次の 2 section に整理する。
   - `Narrowed Recommendations`
   - `Broader Possibilities`
5. 後で意思決定できるだけの十分な説明を各機能案に付ける。
6. この skill はアイデアを提案するだけで、実装しないことを明記する。

## 出力

次を返す。

- `Narrowed Recommendations`: 実現しやすさとリポジトリへの適合度で絞り込んだ新機能アイデア
- `Broader Possibilities`: 即時性や確度がやや低い可能性も含む、より広いアイデア

各新機能アイデアには、簡潔だが十分な説明を付ける。詳細な implementation plan は含めない。

## ガードレール

- 汎用的な product advice ではなく、リポジトリ内の evidence を使う。
- 機能を提案する前に広く読む。
- 曖昧な strategy statement より、具体的な機能提案を優先する。
- 確度の高いアイデアと、広い探索的アイデアを分ける。
- ファイル編集、code 実装、test 実行はしない。
