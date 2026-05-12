# Plan New Feature チェックリスト

## Signal を集める

- まず main README と近くの docs を読む。
- configuration、scripts、entry-point files を確認し、リポジトリが実際にどう使われているかを見る。
- recent commits を確認し、project がすでにどの方向へ進んでいるかを見る。
- 散らばっていたり偏りがあったりしても、存在する local information を使う。

## 現在のリポジトリを理解する

- リポジトリがすでにユーザーの何を助けているかを特定する。
- 繰り返しの workflows、manual steps、rough edges、足りない conveniences を記録する。
- 作りかけの方向性、隣接 capability、自然な extension を探す。

## アイデアを生成する

- リポジトリの現在の purpose に明確に合う機能を優先する。
- fit の高い practical ideas と、より広い exploratory ideas を区別する。
- どのリポジトリにも当てはまる generic ideas は提案しない。

## 出力を書く

- section は次の 2 つだけにする。
  - `Narrowed Recommendations`
  - `Broader Possibilities`
- 追加説明なしで評価できるよう、各アイデアに十分な説明を付ける。
- 説明は具体的にする。機能が何を追加するか、誰を助けるか、どの gap に対応するかを書く。

## 避けること

- 詳細な implementation plans は含めない。
- repository files を変更しない。
- repo evidence が弱い場合、不確かな inference を facts として提示しない。
