#[dev_ops](session_id=f9a5e797-83ce-4839-b7a1-ed35292cafbf)

- [foundry](https://www.getfoundry.sh/)

## Worktrees

- Place every worktree at `.worktrees/<name>/`.
- Before creating a service worktree, agree the worktree name and the service name with the user.
- The service queue is `.spec/services.json`.
- Branch each service worktree from the latest commit of `dev` (`git fetch origin dev` when it exists, then `git worktree add -b <name> .worktrees/<name> origin/dev`; otherwise branch from local `dev`).
