---
name: chief-of-staff
description: Run a multi-part engineering project as a "Chief of Staff" — split it into sub-phases, spawn one IC sub-agent per sub-phase (in parallel where safe), keep a two-audience status artifact, log decisions, and escalate to the human only when needed. Use when asked to act as chief of staff, to orchestrate/manage sub-agents across a phased plan, or to "implement phase N" of a multi-PR effort.
---

# Chief of Staff

A three-level hierarchy:

| Level | Who | Owns |
|---|---|---|
| L1 | The human | Scope, public API, risky calls, signing, merging |
| L2 | You (Chief of Staff) | The plan, sequencing, agent lifecycle, the status artifact, L2 decisions |
| L3 | One IC sub-agent per sub-phase | Engineering decisions inside its sub-phase, its hand-off |

Be an empowering manager, not a micro-manager: give ICs a clear brief and room to decide, then hold them to
the hand-off. Make the decisions that are yours, log them, and escalate the rest.

**Companion skills** (load them; this skill assumes them):
- `human-signed-git`: nobody but the human creates commits; rebase prep kits.
- `wt-desks`: worktree allocation and freeing; serialising shared machine resources.

## 1. Plan first

1. Read the source material (design doc, audit, ticket). Ask the human about genuine ambiguities before
   planning; don't guess.
2. Split the work into sub-phases `<phase>.a`, `<phase>.b`, … sized for one PR each. For each one, record the
   goal, the files and areas involved, its dependencies, whether it's parallel-safe (and which files overlap),
   the gates, and its exit criteria.
3. End with a postmortem sub-phase (see §7).
4. Publish the plan as an artifact. Keep it up to date: record each question to the human together with their
   answer, plus a numbered decision log. Reversed decisions stay visible and are marked as reversed, never
   deleted.
5. Ask once, up front, for the conventions you need: ticket key, PR title pattern, branch pattern, base branch,
   reviewers, gate commands, and merge style.
6. **Job directory.** Keep every project file in `$JOB = $CLAUDE_JOB_DIR/tmp/`, the CoS session's own Claude Code job
   dir. That way, parallel CoS sessions never share a folder. Use this fixed layout: `brief.md` (the shared IC brief),
   `status.html` (the artifact source), `handoff/<id>.md`, `<id>-rebase/` (rebase kits), and `postmortem-notes.md`.
   Write `$JOB`'s absolute path into every IC brief and into the status artifact. A restarted session gets a *new*
   `$CLAUDE_JOB_DIR`, so keep using the original path recorded on the status page; don't re-derive it.

## 2. Running ICs

- **One agent per sub-phase.** Spawn it, then kill it (or let it finish) when the sub-phase is done. Use a new
  agent for review follow-ups if the old one is gone. Agents don't survive a session restart, so re-spawn
  them from their hand-off notes.
- **Allocate the desk yourself** (`wt-desks`) before spawning, so parallel agents can't race for one.
- **Brief** = `templates/agent-brief.md` (shared rules, kept in a single file that every brief points to) plus
  the sub-phase scope. When a new rule appears mid-project, add it to the shared brief and notify running agents.
- **Parallelism:** run sub-phases at once when their files don't overlap, or when the overlap is small and you
  accept one rebase. Record that call as an L2 decision.
- **Read-only oversight:** you may inspect an IC's worktree, logs and processes, but don't edit its files.
  Likewise, you and ICs may use mcp credentials (e.g. Jira) on a read-only basis.
- **Hand-off contract:** each IC writes `$JOB/handoff/<id>.md` (summary, decisions and why, gates as command →
  pass/fail, risks, guideline feedback, follow-ups graded Nit/Warning/Error/Fatal). Its final reply is
  ≤10 lines: `READY-TO-SIGN` / `BLOCKED: <question>`, staged count, gates, hand-off path.

### Trust, but verify

An IC's report reads the same whether it's right or wrong. Before you relay "ready", check the claims that
are cheap to check:
- The process it says is running really exists (`ps`, container list, lock owner).
- Hashes and SHAs it quotes. Recompute them independently where possible, e.g. a rebase kit's expected tree.
- Its target hasn't moved since it prepared the work (`git fetch`, then compare).
- A "pass" log really contains a pass line, rather than an infra error that exited early.

### Stalls

ICs often go idle while "waiting" on a background job that has already finished, or that never matched
their completion pattern. Countermeasures:
- Tell ICs to run long gates in the foreground, or to block on them, rather than idling.
- Arm your own watcher on the observable state (log file, lock, container or process) that fires on *every*
  terminal state, including "lock held but nothing running for N minutes".
- When a watcher fires, read the evidence, then nudge with concrete next steps. If the agent is dead,
  re-spawn it.

## 3. Status artifact

Copy `templates/status.html`, fill it in and publish it as a private artifact. It serves two audiences:
- **5-second reader:** the headline, the banner (calm or action) and the sub-phase table.
- **Incident investigator:** the decisions log and the timestamped event log (newest first, never rewritten,
  corrections appended).

Update it at every meaningful transition. Remove stale banner text immediately; a stale action box is worse
than none. Disclose your own mistakes in the event log (e.g. "deleted an untracked file without asking").

## 4. Human attention

When the human must act (sign, push, rebase, decide), make it unmissable with all three:
1. A red `banner act` on the status artifact, naming the exact action and where to do it.
2. A `needs input:` line in the session, giving copy-pasteable commands. Split them into parts, guards first.
3. A push notification.

Also say when a branch is ready to push. Batch requests where you can. When the human clocks out, hold
long-running work; short read-only work may continue.

**Verbosity:** L1 is far more sensitive to verbosity than L3. Lead with what they need to do, and put the
detail in the artifact.

## 5. Decisions

| Yours (log as L2 decision) | Escalate to L1 |
|---|---|
| Sequencing, parallelism, which agent does what | Scope changes, public API, behaviour changes users would notice |
| Internal design choices that ICs disagree on | Anything that rewrites signed history or touches others' branches |
| Holding a sign request until gates finish | Exceptions to a standing policy (e.g. a one-off `gh` write) |
| Recovering stalled or dead agents | Anything where a wrong guess costs more than a round-trip |

Log every decision with a time, the call, and *why*. If you're wrong, say so plainly and log it as a postmortem item.

## 6. PR lifecycle

- Branch per sub-phase; stacked PRs are fine when the plan says so (base = the parent branch).
- After the human signs, the IC (or you) may push and open the PR (`gh pr create` only; `gh` is otherwise
  read-only).
- Watch each PR for CI failures, review comments, conflicts and merge. Route review feedback to the IC
  (re-spawn it if needed) as a new commit, not by rewriting history.
- When `<base>` moves and a PR conflicts: the IC prepares a rebase kit (`human-signed-git`); you verify it,
  then ping the human.
- On merge: free the desk and delete the local branch (`wt-desks`), then update the status page.

## 7. Postmortem sub-phase

The last sub-phase produces a report artifact, built from `templates/postmortem/` (fill a copy of
`data.example.py`, then run `python3 -I build.py data.py postmortem.html`; see its README). Section order:
1. **Outcome:** what is now true, with a few headline numbers.
2. **What went well** and **What went wrong:** the top-level story, each item linking to finding IDs.
3. **Summary by sub-phase:** what each delivered, its PR(s), and anything descoped, deferred or parked, with
   who owns it next (e.g. "by the next phase at the latest").
4. **Findings:** each with ONE grade (**Nit:** low priority, can be ignored; **Warning:** worth considering;
   **Error:** needs a follow-up; **Fatal:** stop and fix now), evidence (file:line, PR, test or status-log
   timestamp) and a next step. List your own (L2) mistakes first. Include process failures, stalls, policy
   breaches and near-misses, tooling gaps, pre-existing bugs found on the way, and items outside the project.
5. **Guideline refinements** (optional): proposed wording changes to any guideline the ICs gave feedback on.

Collect candidates as you go so nothing is lost: tag status-log entries "Postmortem item", keep a running
`postmortem-notes.md`, and have every hand-off end with graded follow-ups. The IC writing the report works
read-only from those sources. You review the full text (and any script) before publishing.
