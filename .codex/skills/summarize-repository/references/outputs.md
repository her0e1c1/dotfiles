# Summary Outputs

`docs/summary/` 配下に何を出力するか決めるときは、この file を使う。

## 基本出力

必ず生成する。

- `docs/summary/repository-overview.md`
  - リポジトリの purpose
  - main subsystems または packages
  - 見える場合は runtime または deployment shape
  - 明確な場合は primary developer workflows
- evidence が弱いため意図的に省略したものについての短い note

リポジトリを広く調査した後も重要な uncertainty が残る場合は、`docs/summary/assumptions-and-gaps.md` を生成する。

## 任意出力

リポジトリに十分な direct evidence がある場合だけ optional artifacts を生成する。

### アーキテクチャ図

service boundaries、packages、processes、major integrations が見える場合は `docs/summary/architecture.md` を生成する。

- Mermaid flowcharts または graphs を使う。
- 価値の低い file trees より system-level diagram を優先する。
- clients、services、workers、databases、queues、third-party systems など、意味のある boundaries だけを示す。
- internal layering が重要な場合は、module relationships 用の 2 つ目の diagram を追加する。量を増やすためだけに diagrams を追加しない。

### ER 図

schema definitions、ORM models、migrations、SQL files が entities と relationships を明確に定義している場合は、`docs/summary/er-diagram.md` を生成する。

- Mermaid `erDiagram` を使う。
- repository に evidence がある entities だけを含める。
- cardinality が不確かな場合は、relation を省略するか、近くに uncertainty を明記する。
- 少数の field names から full ER model を reverse-engineer しない。

### OpenAPI 仕様

HTTP routes、request shapes、response structures が code または既存 docs で十分明示されている場合は、`docs/summary/openapi.yaml` を生成する。

- リポジトリが別 version に標準化していない限り、OpenAPI 3.1 を優先する。
- local evidence に支えられた routes と schemas だけを含める。
- auth、errors、shared components が部分的にしか見えない場合は最小限にし、overview に limitations を注記する。
- undocumented fields の examples や schema details を捏造しない。

### API Surfaces

リポジトリが OpenAPI ではうまく表せない API を公開している場合、または複数 API style を簡潔に map する必要がある場合は、`docs/summary/api-surfaces.md` を生成する。

- GraphQL schemas、gRPC または Protobuf definitions、RPC handlers、public SDK/library exports、webhook contracts、event topics など、direct local evidence がある APIs だけを扱う。
- 見える場合は entry points、major operations、request または message shapes、既知の consumers または producers を含める。
- event-driven API に明示的な channels、messages、payloads があり、推測なしで書ける場合は、この document の代わりに、または併せて `docs/summary/asyncapi.yaml` を生成する。
- internal helper functions だけから public API を推定しない。

### Runtime と Deployment

runtime または deployment evidence が見える場合は、`docs/summary/runtime-and-deployment.md` を生成する。

- Dockerfiles、compose files、Kubernetes manifests、Terraform、serverless config、process managers、package scripts、Procfiles、CI deployment jobs、documented release steps から evidence を使う。
- 見える場合は processes、runtime dependencies、ports、jobs、scheduled tasks、deployment boundaries を説明する。
- secrets、credentials、account IDs、environment-specific values は summary に含めない。

### Configuration

configuration sources または environment variables が見える場合は、`docs/summary/configuration.md` を生成する。

- `.env.example`、config schemas、typed settings、default config files、package scripts、deployment manifests、docs から evidence を使う。
- 明確な場合は variable または setting names、purpose、defaults、requiredness、source files を含める。
- requiredness や defaults は names から推定せず、unknown として mark する。
- real secret values は含めない。

### CLI と Commands

command-line entry points または task runners が見える場合は、`docs/summary/cli-and-commands.md` を生成する。

- CLIs、Make targets、package scripts、task runner files、shell entry points、documented operational commands を扱う。
- 見える場合は command purpose、entry point、important arguments、side effects を含める。
- repository workflows の中心でない限り、tests や one-off scripts 内だけで使われる incidental commands は document 化しない。

### Module Map

package、workspace、module boundaries が意味を持つ場合は、`docs/summary/module-map.md` を生成する。

- monorepo workspaces、package manifests、build config、imports、module exports、source layout から evidence を使う。
- major packages または modules、その responsibilities、重要な dependencies を示す。
- full file tree より、簡潔な dependency diagram または table を優先する。
- understanding を実質的に改善しない限り、小さな single-module repositories ではこの artifact を作らない。

### Feature Inventory

code、docs、menus、routes、commands、tests から user-visible capabilities を推定できる場合は、`docs/summary/features.md` を生成する。

各 feature には次を含める。

- 簡潔な title
- feature が何をするか
- key entry points または files
- 見える場合は major constraints または integration points

### Use Cases と Sequence Diagrams

リポジトリが meaningful な end-to-end workflows を示している場合は、`docs/summary/use-cases.md` を生成する。

- すべての path を網羅しようとせず、最も representative な use cases を list 化する。
- actor、system interactions、major steps が明確な場合は、強い use case ごとに Mermaid sequence diagram を添える。
- deep edge-case sequencing より core flows の breadth を優先する。
- flow が hidden business logic に依存する場合は、sequence diagram を過度に具体化せず prose で要約する。

### Testing

tests、fixtures、test commands が meaningful な verification structure を示している場合は、`docs/summary/testing.md` を生成する。

- test suites、test types、key fixtures、helper utilities、tests の実行方法を説明する。
- major coverage boundaries と notable gaps は、local evidence から見える場合だけ含める。
- リポジトリが区別している場合は、unit、integration、end-to-end、contract、snapshot、manual checks を区別する。
- repository evidence なしに coverage や quality を推定しない。

## 生成 heuristics

各 artifact を追加する前に、次を確認する。

1. 1 つの弱い hint ではなく、複数の local signals で artifact を支えられるか。
2. その artifact は future reader がリポジトリを実質的に早く理解する助けになるか。
3. missing details を捏造せずに artifact を書けるか。

いずれかの答えが `no` なら、その artifact は skip する。

## 文体

- リポジトリの dominant documentation language に合わせる。
- promotional wording より direct statements を優先する。
- 短い sections と stable filenames を使う。
- Mermaid syntax は simple で portable に保つ。
