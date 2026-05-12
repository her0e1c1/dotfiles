---
name: summarize-repository
description: ユーザーが repository の構造、workflow、API、data model、runtime、configuration、features、tests について、調査、要約、diagram 化、documentation 生成を依頼したときに使う。
---

# Summarize Repository

## 概要

repository を広く調査し、local evidence から現在の構造を推定し、codebase の clean snapshot として `docs/summary/` を再生成する。

summary files を書く前に [references/outputs.md](references/outputs.md) を読む。

## ワークフロー

1. `README`、`docs/`、contribution guides などの既存 docs から、repository の documentation language を検出する。
2. dominant documentation language が不明な場合は、output 生成前にユーザーへ確認する。
3. 次を理解できるだけ repository を広く読む。
   - purpose と主要 workflows
   - entry points と runtime boundaries
   - data stores と schema sources
   - API surfaces
   - 主要 features と representative use cases
4. 古い summary files を incremental に保持するのではなく、現在の repository state から `docs/summary/` を再作成する。
5. repository overview document は必ず生成する。
6. supported output artifacts の source of truth として `references/outputs.md` を使い、repository にそれを支える十分な evidence がある場合だけ追加 document を生成する。
7. 不確実性は明示する。local evidence に基づかない endpoint、entity、component、flow を作り上げない。

## 出力ルール

- `docs/summary/` に書く。
- 生成した files は、分析時点の repository の fresh snapshot として扱う。
- `references/outputs.md` にある stable filenames と generation conditions を使う。
- diagram は Mermaid を優先し、output を text-native で reviewable に保つ。
- prose は簡潔にし、repository evidence へ trace できるようにする。
- output を skipped する場合は、content を捏造するのではなく、overview または assumptions file で理由を書く。

## ガードレール

- stack への一般的な期待ではなく、repository-local evidence に基づいて summary を作る。
- 完成して見えるが speculative な package より、小さくても正確な artifact set を優先する。
- 既存の `docs/summary/` に手動編集が含まれる場合でも、ユーザーが別方針を明示しない限り置き換える。
- 生成 artifact が曖昧な architecture や domain behavior に依存する場合は、停止して焦点を絞った follow-up question をする。
