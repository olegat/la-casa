# Rebase <PR/branch> onto <base>

Prepared against `origin/<base>` = `<BASE_SHA>`.
Run in `<worktree>` (branch `<branch>`, clean, HEAD `<HEAD_SHA>`).
Paste one block at a time. Guards only print OK or STOP: if any prints STOP, do not continue.

## 1. Guards

```sh
cd <worktree>
git fetch origin
[ "$(git rev-parse origin/<base>)" = <BASE_SHA> ] && echo "OK base unchanged" || echo "STOP: base moved - kit not verified against it"
[ "$(git rev-parse HEAD)" = <HEAD_SHA> ] && echo "OK HEAD" || echo "STOP: HEAD is not <HEAD_SHA>"
[ "$(git branch --show-current)" = <branch> ] && echo "OK branch" || echo "STOP: wrong branch"
[ -z "$(git status --porcelain)" ] && echo "OK clean" || echo "STOP: worktree not clean"
```

## 2. Rebase (stops on the conflict)

```sh
git rebase origin/<base>
```

## 3. Check the conflict set, copy the resolved files, stage

```sh
[ "$(git diff --name-only --diff-filter=U | sort | tr '\n' ' ')" = "<sorted conflicted paths, space-separated, trailing space> " ] && echo "OK conflict set" || echo "STOP: unexpected conflict set"

R=<kit-dir>/files
cp "$R/<path1>" "<path1>"
git add "<path1>"

[ "$(git write-tree)" = <TREE_SHA> ] && echo "OK tree matches the verified tree" || echo "STOP: index differs from the verified tree"
```

## 4. Continue (re-signs), verify, push

```sh
git rebase --continue
git log -1 --format='%G? %s'   # expect: G <subject>
git push --force-with-lease origin <branch>
```

## Resolutions

- **<path1>**: what <base> changed, what the branch changed, and how they were combined, including why
  nothing was ambiguous.

Files that merged cleanly, and how they were checked. Gates run against the resolved tree, and the results.
