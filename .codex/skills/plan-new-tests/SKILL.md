---
name: plan-new-tests
description: 現在の実装 code と既存 tests を読み、欠けていそうな test case を特定し、編集や test 実行はせずに簡潔な test 追加 plan を作る必要があるときに使う。
---

# Plan New Tests

## 概要

まず実装を読み、それから既存 tests と比較して、欠けていそうな箇所を見つける。出力は、missing test-case candidates の簡潔な list と、後で追加するための follow-up plan だけにする。

codebase に複数の testing style がある場合、または feature area が広い場合は、開始前に `references/checklist.md` を読む。

## ワークフロー

1. 関連する implementation code を読み、主要 behavior、branch、boundary condition、failure path を抽出する。
2. 最も近い既存 tests を読み、すでに covered されている behavior を特定する。
3. implementation behavior と test coverage を比較し、欠けていそうな test case を list 化する。
4. 重複を取り除き、candidate list を簡潔に保つ。
5. follow-up の test 追加に向けた短い plan を作る。
6. test は追加も実行もしていないことを明記する。

## 出力

次を返す。

- 欠けていそうな test case の簡潔な list
- 後でそれらの test を追加するための短い follow-up plan

implementation edit、fixture edit、test execution result は含めない。

## ガードレール

- implementation code を読むことは必須。
- 既存 tests を確認するまでは、case が missing だと断定しない。
- 長い説明より、簡潔な candidate list を優先する。
- 不確かな case は facts ではなく candidates として扱う。
- tests の追加、code 編集、tests を実行する command は行わない。
