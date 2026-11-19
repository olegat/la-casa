---
name: wt-desks
description: Hot-desk worktree policy shared by every agent on this machine — allocate ~/worktrees/wtN (smallest free N), branch from the base, keep the desk until the branch merges, inspect before freeing. Also how to serialise a shared machine resource (e.g. an e2e server or a Docker container) between agents with a lock. Use when creating, choosing, freeing or cleaning up a worktree, or when several agents contend for one resource.
---

# Worktree desks

Many agents, from this session and others, share `~/worktrees/`. The scheme only works if everyone follows it.

## Allocate
1. `git fetch origin <base>`.
2. Pick the **smallest free N** for `~/worktrees/wtN`. "Free" means no directory exists AND no entry in
   `git worktree list`. Re-check immediately before creating, because other agents allocate too.
3. Create the desk on a new branch, from the main checkout, quoting every path (zsh word-splitting once
   created a worktree called `wt1 a`):
   ```sh
   git worktree add -b "<branch>" "$HOME/worktrees/wtN" "origin/<base>"
   ```
   Scratch desks for verification use `--detach` and no branch.
4. Run the repo's setup in the new desk (dependency install, tooling init) before any gates.
5. An orchestrator allocates desks *before* spawning agents, so parallel agents can't race for one.

Desk creation is the only HEAD-related action an agent may take without asking. Everything else follows
`human-signed-git`.

## Keep
A desk stays allocated **until its branch has merged into `<base>`**, not just pushed. Review feedback needs it.
Agents never remove their own desk or branch.

## Free (only after the merge)
1. Confirm the merge: the PR state is MERGED, or `git merge-base --is-ancestor <tip> origin/<base>`.
2. **Inspect before deleting.** Run `git status --short --ignored` and look past generated output (deps,
   build caches, test reports and snapshots). Any untracked or ignored file that looks hand-made (notes,
   editor autosaves, scratch files) is the human's: ask first. Real incident: an Emacs autosave was destroyed
   by a `--force` removal.
3. Delete `commit-message.txt` and other agent scratch files in the desk.
4. `git worktree remove "$HOME/worktrees/wtN"` (`--force` only after step 2), then `git branch -D <branch>`
   for the local branch.
5. Scratch (detached) desks can be freed as soon as their job is done, after the same inspection.

## Shared resources (locks)
Some resources can't be shared by concurrent runs. For example, an e2e harness that kills other containers or
binds a fixed port. Serialise them with `scripts/resource-lock.sh`:
```sh
LOCK_NAME=e2e BUSY_CMD="docker ps --format '{{.Names}}' | grep -m1 '^playwright-'" \
  resource-lock.sh acquire <owner-id>     # blocks; run it in the background or under a monitor
… run …
resource-lock.sh release <owner-id>       # ALWAYS, even on failure
resource-lock.sh status
```
- `BUSY_CMD` (optional) means "the resource is in use even without the lock". It covers sessions that don't
  know about the lock.
- **Stale holders are the main failure mode**: an agent that dies or idles while holding the lock. A watcher
  should flag "held, but `BUSY_CMD` and the owner's process are both absent for N minutes". Release a stale
  lock only after checking that the holder's process is really gone.
- Known flake to document per repo: e.g. "the first e2e run in a fresh desk fails with a Docker
  `chown …/reports: permission denied` error; retry once".
