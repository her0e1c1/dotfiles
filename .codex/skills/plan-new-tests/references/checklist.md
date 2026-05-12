# Plan New Tests チェックリスト

## 実装を読む

- まず target feature、module、または command path を特定する。
- tests を確認する前に implementation code を読む。
- 主要な success paths、branching behavior、edge conditions、invalid inputs、failure handling を記録する。

## 既存 tests を読む

- 同じ feature または module に最も近い existing test files を探す。
- 新しいものを提案する前に、どの behavior がすでに covered されているか確認する。
- local coverage を判断するときは、遠い shared examples より近くの tests を優先する。

## Missing cases を特定する

- implementation behavior を current coverage と比較する。
- uncovered branches、boundary values、error paths、重要な input variants を探す。
- 重複をまとめ、tests を読んだ後でも plausibly uncovered に見える case だけを残す。
- list は actionable な短さに保つ。

## Plan を作る

- missing test cases の簡潔な candidate list を返す。
- 後で test-writing work に取り組む方法を説明する短い follow-up plan を追加する。
- tests は追加も実行もしていないことを明記する。

## 避けること

- この skill では implementation や fixture の変更を提案しない。
- coverage が曖昧な場合、case が definitely missing だと主張しない。
- tests を実行せず、fabricated verification status も残さない。
