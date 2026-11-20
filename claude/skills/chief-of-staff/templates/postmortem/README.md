# Postmortem report template

1. Copy `data.example.py` to `data.py` and replace every value. Each field has a comment; strings are HTML fragments.
2. Build: `python3 -I build.py data.py out.html`. It prints the finding count per grade.
3. Section order is fixed: Outcome → What went well → What went wrong → Summary → Findings → Guideline refinements
   (optional: set `REFINEMENTS = None`). The page has light and dark themes, a grade filter, and works at phone width.

## Grading (one grade per finding)
- **Nit**: low priority, can be ignored.
- **Warning**: worth considering.
- **Error**: needs a follow-up.
- **Fatal**: stop and fix now.

## Evidence and next step
Every finding needs both. Evidence is something a reader can check: `file:line` at a named commit, a PR, a test,
or a timestamped log entry. Never "it seemed". The next step is concrete and has an owner or a trigger
("fix by the next phase at the latest, starting from the saved patch"), not "consider improving".
Raise each Error or Fatal with its owner; don't leave it only in the report.

"What went wrong" tells the overall story and cites finding IDs (`See PR-1`); the builder turns them into links.
Keep it candid, including the orchestrator's own mistakes, and balance it with "What went well".
