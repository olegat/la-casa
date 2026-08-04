---
description: Write commit-message.txt with a JIRA prefix and Claude co-author trailer
argument-hint: [JIRA-TICKET]
---
Write a git commit message to `commit-message.txt` in the project root, describing the
current staged/working changes.

The JIRA ticket number is: $ARGUMENTS
If that is empty, ASK me for the JIRA ticket number and wait for my reply before writing the file.

Requirements for the message:
- Prefix the first line (subject) with the ticket number followed by a `[CLAUDE]` tag that
  sign-posts this as an AI-generated commit, e.g. `CRT-1125 [CLAUDE] ...`. Keep the subject concise.
- Base the body on the ACTUAL diff (`git status` / `git diff`): what changed and why. Do not invent changes.
- Follow this repo's commit conventions if any are documented.
- End the file with this final line:
  Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
  (adjust to the correct model-name when applicable)
