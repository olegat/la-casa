# Example data for build.py. Copy to data.py, replace every value, then:
#   python3 -I build.py data.py out.html
# Every string is an HTML fragment: use <code>…</code> for identifiers and &lt; &gt; &amp; for literal < > &.

META = {
    'project': 'Example project',          # eyebrow line, e.g. the project or ticket name
    'phase': 'Phase 1',                     # eyebrow line; also the default title ("Phase 1 Postmortem")
    'title': None,                          # optional explicit <title>/<h1>; None uses "<phase> Postmortem"
    'standfirst': 'What each step delivered, what is now true, and {total} graded findings.',  # {total} = finding count
    'meta': ['Period 1–14 Mar', 'Measured on main abc1234'],  # small mono facts under the heading
    'evidence_note': 'Evidence cites file:line on main abc1234 unless a log is named.',  # appended to the grading line
}

OUTCOME = {
    'tiles': [                              # (big number or short text, caption); 2–4 tiles
        ('40 &rarr; 5', 'Escapes left'),
        ('6', 'PRs merged'),
    ],
    'bullets': [                            # (bold lead-in, sentence); what is now true, and what is not yet
        ('Boundary', 'Five escapes remain, each with a reason in the code.'),
        ('Not yet true', 'The legacy helper module still exists.'),
    ],
}

WENT_WELL = [                               # (heading, short paragraph); 4–7 items
    ('Tests were written before the refactor', 'Pinning behaviour first caught two wrong assumptions.'),
    ('The policy held', 'No agent committed; every slip was disclosed the same day.'),
]

WENT_WRONG = [                              # (heading, short paragraph citing finding IDs); 5–8 items
    ('The plan was wrong in places', 'Two API claims were never checked against the code. See PR-1.'),  # IDs become links
    ('Real bugs surfaced late', 'A listener leak predates the work and was found last. See CD-1, TP-1.'),
]

SUMMARY = {
    'title': 'Summary by step',
    'lede': 'In execution order. Notes cover anything descoped, deferred or parked.',
    'columns': ['ID', 'Step', 'Delivered', 'PR, merged', 'Notes'],  # five columns, fixed order
    'flag': 'Parked',                       # rows whose reference contains this text are highlighted
    'rows': [                               # (id, name, delivered, reference, date, notes)
        ('1.a', 'API gaps', 'Six new methods with tests.', '#101', '03-04', 'One method dropped, then re-added.'),
        ('1.b', 'Call sites', 'Twelve call sites moved to the new API.', '#102', '03-06', ''),
        ('1.c', 'Leak fix', 'Fix confirmed by two tests.', 'Parked', '', 'Must land by the next phase; patch saved.'),
    ],
    'after': 'Behaviour changes accepted and pinned by tests: …',  # optional paragraph under the table
}

GROUPS = [                                  # finding groups, in display order
    {
        'id': 'process',                    # anchor id (lowercase, no spaces)
        'title': 'Process and orchestration',
        'intro': 'Mistakes by the orchestrator are listed first, on purpose.',  # optional
        'findings': [                       # (id, grade, title, what happened, evidence, next step)
            ('PR-1', 'Warning', 'Plan claims were not checked',
             'The plan said X was equivalent to Y; it is not under Z.',
             'Plan decision 4; <code>src/a.ts:120</code>.',   # always cite: file:line, PR, test or log timestamp
             'Check every "equivalent" claim against the code before work starts.'),  # always concrete
            ('PR-2', 'Nit', 'Stale banner text', 'Finished steps stayed on the status banner.',
             'Status log 03-05 10:30.', 'Rebuild the banner from current state.'),
        ],
    },
    {
        'id': 'design',
        'title': 'Code and design',
        'findings': [
            ('CD-1', 'Error', 'Unsubscribe is a no-op', 'Removing an internal listener does nothing.',
             '<code>src/events.ts:88</code>; saved patch <code>fix.patch</code>.',
             'Fix by the next phase at the latest, starting from the saved patch.'),
            ('TP-1', 'Fatal', 'Example of the top grade', 'Use only for stop-the-line problems.',
             'CI run 1234.', 'Stop other work and fix now.'),
        ],
    },
]

REFINEMENTS = {                             # optional section; set to None or {} to omit
    'title': 'Test guideline refinements',
    'intro': 'Proposed wording changes to the draft guide.',
    'items': [                              # (id, grade, title, what the guide says today, proposed wording)
        ('R1', 'Warning', 'Prove each test can fail', 'Silent.',
         'Add: "Mutate the code under test once and check the test fails."'),
        ('R2', 'Nit', 'Fix typos', '"preferrable".', '"preferable".'),
    ],
}

FOOTER = [                                  # (bold lead-in, sentence); sources and what was not verified
    ('Sources', 'The plan, the status log and every hand-off.'),
    ('Not verified', 'Item PR-2 on the current main.'),
]
