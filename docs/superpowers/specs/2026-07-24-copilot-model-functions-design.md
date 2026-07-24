# Copilot model functions design

## Goal

Add short Bash functions to `.profile` for starting GitHub Copilot CLI with a
specific model while preserving the existing `copilot` auto-selection function.

## Interface

| Function | Copilot model ID |
| --- | --- |
| `copilot_sonnet5` | `claude-sonnet-5` |
| `copilot_sonnet46` | `claude-sonnet-4.6` |
| `copilot_haiku` | `claude-haiku-4.5` |
| `copilot_gpt55` | `gpt-5.5` |
| `copilot_gpt54` | `gpt-5.4` |
| `copilot_gpt53` | `gpt-5.3-codex` |
| `copilot_opus48` | `claude-opus-4.8` |
| `copilot_opus46` | `claude-opus-4.6` |

Each function forwards all additional arguments unchanged to Copilot CLI.

## Implementation

Use one direct wrapper per model:

```bash
copilot_sonnet5() {
  command copilot --model "claude-sonnet-5" "$@"
}
```

Direct wrappers keep each mapping visible and avoid adding a helper function for
only eight mappings. The existing `copilot()` wrapper will also use
`command copilot` so it invokes the executable instead of recursively invoking
itself. The staged `copilot_high()` and incomplete `copilot_mid()` functions will
be replaced by the model-specific wrappers.

No new dependency or external-tool setting is introduced.

## Verification

- Run Bash's syntax checker against `.profile`.
- Source `.profile` in an isolated Bash process and verify that all nine
  functions are defined.
- Inspect each function definition to verify the expected model ID and argument
  forwarding.

The locally installed Copilot CLI is version `0.0.403`, which predates some
requested models. The wrappers can be verified independently, but using every
model requires an up-to-date Copilot CLI and model access on the user's plan.
