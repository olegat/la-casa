---
description: Paranoid correctness-only code review of a worktree or branch, written to review.txt in GCC diagnostic format
argument-hint: [worktree-dir | branch-name]
---
Perform an adversarial, correctness-only code review of a git branch. Anything that can be wrong
is wrong until you have checked it.

The target is: $ARGUMENTS

# 1. Resolve the target FIRST

Do this before reading any code, and do not create anything until resolution succeeds.

Run `git fetch` first (a target may exist only on the remote), then resolve the argument:

- **Empty** — list plausible branches (recent local branches, and remote branches whose last commit
  is mine) with a one-line summary each, ASK which to review, and wait for my reply. Then treat the
  answer as the branch case.
- **An existing worktree** — a bare name matching a directory in `~/worktrees/`, or any path
  (absolute, relative, or `~`-prefixed) that `git worktree list` knows. Review it as-is, on whatever
  branch it currently has checked out.
- **An existing branch** — local or remote-only. If a worktree already has that branch checked out,
  use that worktree (it is the worktree case). Otherwise create one; see below.

Resolve both interpretations and only ASK me to disambiguate when BOTH genuinely match — i.e. the
same string is both a directory in `~/worktrees/` and an existing branch. A name containing `/` and
matching no directory is a branch. Do not guess on a real collision.

**Abort on anything wrong.** A named worktree directory that does not exist, a branch that exists
neither locally nor on any remote, a detached or corrupt worktree, a dirty worktree with uncommitted
changes that would confuse the diff — stop, say precisely what is wrong, and do not proceed. Never
invent a target that resembles what I typed.

# 2. Create the worktree (branch case only)

Pick `~/worktrees/wt1`, else `wt2`, `wt3`, … — the lowest suffix whose path neither exists on disk
nor appears in `git worktree list`. Another session may take the same number concurrently, so if
`git worktree add` fails on a collision, move to the next number and retry.

- Local branch: `git worktree add <path> <branch>`
- Remote-only branch: `git worktree add -b <branch> <path> origin/<branch>`

Then, in the new worktree, run the repo's install step (`yarn install` unless the repo documents
otherwise). This is not optional: without it the build, lint and test gates cannot run at all, and
in some repos the agent tooling itself is installed by it. It may take several minutes.

For an existing worktree, check whether dependencies are installed; if not, tell me the gates are
unavailable rather than installing without asking.

If dependencies are missing and cannot be installed, still do the review — but say plainly in the
report that it is static-only and which gates never ran. Never imply you verified something you did not.

# 3. Establish the review range — do not trust "the base"

Determine the base branch from the repo's documented default (its CLAUDE.md or equivalent);
otherwise infer it and tell me what you inferred. Then:

- Compute `git merge-base <base> <head>` and diff with three dots (`<base>...<head>`) so the range
  is the branch's own work and not whatever the base merged since.
- **Sanity-check the range before reviewing it.** If it contains commits that are plainly not part
  of this change — unrelated tickets, duplicates of work already on the base under different SHAs,
  a merge-base far behind the base tip — the branch was cut from a stale base. Say so, state the
  range you are actually reviewing, and narrow to the branch's own commits.
- Report the diffstat and commit list up front. If the diff is very large, tell me the size and ask
  whether to narrow before spending the effort.

# 4. Review

Be extremely thorough, picky and paranoid. Focus ONLY on correctness: bugs, edge cases, logic
errors, race conditions, off-by-ones, error paths, unhandled states, broken invariants, API and
behaviour changes, cache-key and identity mistakes, resource and lifetime bugs. Ignore code style
entirely — naming, whitespace, formatting, import order.

Do not be sycophantic. Tell me what I need to hear, not what I want to hear. A review that finds
nothing is a review that did not look hard enough — but never pad it with invented problems either.

- **Verify, do not speculate.** Read the definition of every symbol whose behaviour your finding
  depends on. Where a claim is cheap to test, test it: run the code, the rule, the query. State for
  each finding whether it is CONFIRMED (reproduced) or reasoned from source.
- Prefer a small number of real defects, each with a concrete failure path, over a long list of
  vague worries.
- Check that new tests actually exercise the risky path, and name the gaps.
- Run whatever review gates the repo documents in its own instructions. Do not assume any particular
  tool exists — this command runs in many repos.
- Read the change as if reviewing a stranger's PR, then check the claims the change makes about
  itself: comments, docs and commit messages that no longer match the code are findings.

**Leave no trace.** The review must not modify the branch: no commits, no pushes, no edits to
source. If you write a scratch or probe file anywhere to test a hypothesis — including in another
checkout — delete it and confirm both trees are clean before you finish.

# 5. Write the report

Write `review.txt` to the reviewed project's root. If it exists, append a number: `review2.txt`,
`review3.txt`, and so on. Never overwrite an existing report.

Use GCC diagnostic format so it works with Emacs compile and next-error:

```
packages/ag-foobar-example/foo.c:1:2: error: 'my example error' [-Werror-#errors]
    1 | #error 'my example error'
      |  ^
      | Add your natural-language explanation here.
      | Multiple lines are allowed.

packages/ag-foobar-example/foo.c:2:2: warning: 'my example warning' [-W#warnings]
    2 | #warning 'my example warning'
      |  ^
      | Do NOT use filename:linenumber patterns in explanations.
      | Avoid patterns like example.ts:2 or example.ts(2), as they break parsing.
      | Use alternatives like "example.ts (Line 2)".
```

- Paths relative to the project root. `error` for defects, `warning` for risks and behaviour changes.
- Order most severe first.
- In explanation bodies, never write anything that parses as a location — no `file.ts:12`,
  no `file.ts(12)`, no `:905:37`. Write "file.ts (Line 12)". This applies to quoted tool output and
  stack traces too, which is the easiest place to slip.
- **Before finishing, verify every diagnostic**: the quoted source line must match the file at that
  line exactly, and the column must be within it. Check them mechanically, not by eye.
- Open with a header stating: the worktree path, branch, base, the exact reviewed range, the
  diffstat, and which gates ran and which did not.
- Close with coverage gaps and any state that needs my attention (a branch needing a rebase, say).

# 6. Finish

Tell me the absolute path of the report, and warn me it is untracked and will be destroyed if the
worktree is removed — copy it somewhere durable if I want to keep it. Summarise the most serious
findings in your reply; do not make me open the file to learn whether anything is wrong.
