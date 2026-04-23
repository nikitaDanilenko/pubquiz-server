# Project Guidelines

## GitHub Workflows and Actions

- Use `main` as the default branch name, never `master`
- Always use hash-pinned action versions with a version comment: `uses: actions/checkout@<sha> # v1.2.3`
- Always use the latest stable versions of actions
- Name steps with verb phrases (imperative mood, like Git commits):
  - "Check out" not "Checkout"
  - "Set up Haskell" not "Setup Haskell"
  - "Build project" not "Building"
- Prefer YAML dash syntax for arrays over JSON bracket syntax:
  ```yaml
  # Good
  branches:
    - main
  
  # Avoid
  branches: [main]
  ```
