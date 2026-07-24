# Codex model functions design

## Goal

Add model-specific Bash functions to `.profile` that start the Codex CLI with
an optional reasoning/thinking effort. The effort defaults to `medium`.

## Interface

| Function | Codex model ID |
| --- | --- |
| `codex_54` | `gpt-5.4` |
| `codex_53` | `gpt-5.3-codex` |
| `codex_52` | `gpt-5.2-codex` |
| `codex_51` | `gpt-5.1-codex` |
| `codex_51mini` | `gpt-5.1-codex-mini` |

Each function accepts the optional first argument `minimal`, `low`, `medium`,
`high`, or `xhigh`. If omitted, it uses `medium`. Remaining arguments are
forwarded unchanged to Codex.

Examples:

```bash
codex_53
codex_53 high
codex_54 low -- full command arguments
```

## Implementation

Use a private helper to avoid repeating argument handling while keeping one
small public wrapper per model:

```bash
codex_model() {
  local model="$1"
  local effort="medium"
  if [[ $# -gt 1 ]]; then
    effort="$2"
    shift 2
  else
    shift
  fi
  command codex --model "$model" \
    --config "model_reasoning_effort=\"$effort\"" \
    "$@"
}
```

The helper validates the first optional argument against the supported effort
values. An invalid value prints usage information and returns a non-zero
status without starting Codex. This prevents an arbitrary first CLI argument
from being mistaken for a thinking level.

Each public wrapper passes its fixed model ID and the caller's arguments to the
helper. No dependency or external-tool setting is introduced.

## Verification

- Run `bash -n .profile`.
- Source `.profile` in an isolated Bash process and verify all five public
  functions exist.
- Use a fake `codex` executable to verify the default and explicit effort are
  passed as `--config` and that subsequent arguments are preserved.
- Verify invalid effort values fail before invoking the executable.
