# Commit Prompt

現在 staged されている git diff を確認し、簡潔な commit message を作成してから `git commit` を実行する。

要件:
- staged diff だけに基づいて変更を要約する。
- commit subject は命令形を優先する。
- subject は 72 文字以内に収める。
- ユーザー影響または保守上の影響がある変更は、必要な場合だけ body で触れる。
- diff に存在しない変更を作り上げない。
- unstaged 変更は完全に無視する。commit の scope 外である。
- ユーザーに追加確認せず `git commit` を実行する。
- メッセージを表示するだけでなく、生成した message で `git commit` を実行する。
