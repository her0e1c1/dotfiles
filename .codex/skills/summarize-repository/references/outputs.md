# Summary Outputs

Use this file when deciding what to emit under `docs/summary/`.

## Baseline Outputs

Always generate:

- `docs/summary/repository-overview.md`
  - purpose of the repository
  - main subsystems or packages
  - runtime or deployment shape when visible
  - primary developer workflows if clear
  - short note on what was intentionally omitted because evidence was weak

Generate `docs/summary/assumptions-and-gaps.md` when important uncertainty remains after broad repository inspection.

## Optional Outputs

Generate optional artifacts only when the repository contains enough direct evidence.

### Architecture Diagram

Generate `docs/summary/architecture.md` when service boundaries, packages, processes, or major integrations are visible.

- Use Mermaid flowcharts or graphs.
- Prefer a system-level diagram over low-value file trees.
- Show only meaningful boundaries such as clients, services, workers, databases, queues, and third-party systems.
- If internal layering matters, add one second diagram for module relationships. Do not add diagrams just to increase volume.

### ER Diagram

Generate `docs/summary/er-diagram.md` when schema definitions, ORM models, migrations, or SQL files clearly define entities and relationships.

- Use Mermaid `erDiagram`.
- Include only entities with evidence in the repository.
- If cardinality is uncertain, either omit the relation or state the uncertainty nearby.
- Do not reverse-engineer a full ER model from a handful of field names.

### OpenAPI Specification

Generate `docs/summary/openapi.yaml` when HTTP routes, request shapes, and response structures are sufficiently explicit in code or existing docs.

- Prefer OpenAPI 3.1 unless the repository already standardizes on another version.
- Include only routes and schemas supported by local evidence.
- If auth, errors, or shared components are only partly visible, keep them minimal and annotate limitations in the overview.
- Do not fabricate examples or schema details for undocumented fields.

### API Surfaces

Generate `docs/summary/api-surfaces.md` when the repository exposes APIs that are not well represented by OpenAPI, or when multiple API styles need a concise map.

- Cover only APIs with direct local evidence, such as GraphQL schemas, gRPC or Protobuf definitions, RPC handlers, public SDK/library exports, webhook contracts, or event topics.
- Include entry points, major operations, request or message shapes when visible, and known consumers or producers.
- If an event-driven API has explicit channels, messages, and payloads, generate `docs/summary/asyncapi.yaml` instead of or alongside this document when it can be written without guessing.
- Do not infer a public API from internal helper functions alone.

### Runtime And Deployment

Generate `docs/summary/runtime-and-deployment.md` when runtime or deployment evidence is visible.

- Use evidence from Dockerfiles, compose files, Kubernetes manifests, Terraform, serverless config, process managers, package scripts, Procfiles, CI deployment jobs, or documented release steps.
- Describe processes, runtime dependencies, ports, jobs, scheduled tasks, and deployment boundaries when visible.
- Keep secrets, credentials, account IDs, and environment-specific values out of the summary.

### Configuration

Generate `docs/summary/configuration.md` when configuration sources or environment variables are visible.

- Use evidence from `.env.example`, config schemas, typed settings, default config files, package scripts, deployment manifests, or docs.
- Include variable or setting names, purpose, defaults, requiredness, and source files when clear.
- Mark requiredness or defaults as unknown rather than inferring them from names.
- Do not include real secret values.

### CLI And Commands

Generate `docs/summary/cli-and-commands.md` when command-line entry points or task runners are visible.

- Cover CLIs, Make targets, package scripts, task runner files, shell entry points, and documented operational commands.
- Include command purpose, entry point, important arguments, and side effects when visible.
- Do not document incidental commands that are only used inside tests or one-off scripts unless they are central to repository workflows.

### Module Map

Generate `docs/summary/module-map.md` when package, workspace, or module boundaries are meaningful.

- Use evidence from monorepo workspaces, package manifests, build config, imports, module exports, or source layout.
- Show major packages or modules, their responsibilities, and important dependencies between them.
- Prefer a concise dependency diagram or table over a full file tree.
- Do not create this artifact for small single-module repositories unless it materially improves understanding.

### Feature Inventory

Generate `docs/summary/features.md` when user-visible capabilities can be inferred from code, docs, menus, routes, commands, or tests.

For each feature include:

- concise title
- what the feature does
- key entry points or files
- major constraints or integration points when visible

### Use Cases And Sequence Diagrams

Generate `docs/summary/use-cases.md` when the repository reveals meaningful end-to-end workflows.

- List the most representative use cases instead of trying to cover every path.
- Pair each strong use case with a Mermaid sequence diagram when the actor, system interactions, and major steps are clear.
- Favor breadth across core flows over deep edge-case sequencing.
- If a flow depends on hidden business logic, summarize it in prose instead of over-specifying a sequence diagram.

### Testing

Generate `docs/summary/testing.md` when tests, fixtures, or test commands reveal meaningful verification structure.

- Describe test suites, test types, key fixtures, helper utilities, and how tests are run.
- Include major coverage boundaries and notable gaps only when they are visible from local evidence.
- Distinguish unit, integration, end-to-end, contract, snapshot, and manual checks when the repository makes that distinction.
- Do not estimate coverage or quality without repository evidence.

## Generation Heuristics

Use these checks before adding each artifact:

1. Can the artifact be supported by multiple local signals, not one weak hint?
2. Would the artifact help a future reader understand the repository materially faster?
3. Can the artifact be written without inventing missing details?

If any answer is `no`, skip that artifact.

## Style

- Match the repository's dominant documentation language.
- Prefer direct statements over promotional wording.
- Use short sections and stable filenames.
- Keep Mermaid syntax simple and portable.
