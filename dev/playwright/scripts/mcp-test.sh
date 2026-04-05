printf '%s\n%s\n' \
'{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"manual-test","version":"0.1.0"}}}' \
'{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}' \
| npx playwright run-test-mcp-server \
| jq .
