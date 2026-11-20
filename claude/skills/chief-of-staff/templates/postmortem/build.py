"""Build a postmortem report page from a data file.

Usage: python3 -I build.py data.py out.html
See README.md and data.example.py for the data format.
"""
import collections
import html
import re
import runpy
import sys

ORDER = ['Fatal', 'Error', 'Warning', 'Nit']
GRADE_HELP = ('Nit: low priority, can be ignored. Warning: worth considering. '
              'Error: needs a follow-up. Fatal: stop and fix now.')


def chip(g):
    if g not in ORDER:
        raise SystemExit(f'unknown grade {g!r}; use one of {ORDER}')
    return f'<span class="grade g-{g.lower()}">{g}</span>'


def link_ids(text, ids):
    """Turn finding IDs mentioned in text (e.g. 'see PR-1') into in-page links."""
    return re.sub(r'\b([A-Z]{1,4}-\d+)\b',
                  lambda m: f'<a href="#{m.group(1)}">{m.group(1)}</a>' if m.group(1) in ids else m.group(1),
                  text)


def main(data_path, out_path):
    d = runpy.run_path(data_path)
    meta = d['META']
    outcome = d.get('OUTCOME', {})
    well = d.get('WENT_WELL', [])
    wrong = d.get('WENT_WRONG', [])
    summary = d.get('SUMMARY', {})
    groups = d['GROUPS']
    refine = d.get('REFINEMENTS') or {}
    footer = d.get('FOOTER', [])

    title = meta.get('title') or f"{meta['phase']} Postmortem"
    ids = {f[0] for g in groups for f in g['findings']}
    counts = collections.Counter(f[1] for g in groups for f in g['findings'])
    total = sum(counts.values())

    h = [CSS.replace('__TITLE__', html.escape(title)), '<div class="wrap">']

    eyebrow = ' &middot; '.join(x for x in (meta.get('project'), meta.get('phase')) if x)
    metas = ''.join(f'<span>{m}</span>' for m in meta.get('meta', []))
    h.append(f'''<header>
  <div class="eyebrow">{eyebrow}</div>
  <h1>{html.escape(title)}</h1>
  <p class="standfirst">{meta.get('standfirst', '').replace('{total}', str(total))}</p>
  <div class="meta">{metas}</div>
</header>''')

    # Outcome
    tiles = ''.join(f'<div><span class="n">{n}</span><span class="k">{k}</span></div>' for n, k in outcome.get('tiles', []))
    bullets = ''.join(f'<li><strong>{b}.</strong> {t}</li>' for b, t in outcome.get('bullets', []))
    h.append(f'<section id="outcome"><h2>Outcome</h2>'
             + (f'<div class="tally">{tiles}</div>' if tiles else '')
             + (f'<div class="outcome"><ul>{bullets}</ul></div>' if bullets else '')
             + '</section>')

    # What went well / wrong
    if well:
        h.append('<section id="went-well"><h2>What went well</h2><div class="well">')
        h += [f'<div><h3>{t}</h3><p>{link_ids(b, ids)}</p></div>' for t, b in well]
        h.append('</div></section>')
    if wrong:
        h.append('<section id="went-wrong"><h2>What went wrong</h2>'
                 '<p class="lede">The overall story; each item links to the graded findings below.</p><div class="well wrong">')
        h += [f'<div><h3>{t}</h3><p>{link_ids(b, ids)}</p></div>' for t, b in wrong]
        h.append('</div></section>')

    # Summary table
    if summary.get('rows'):
        cols = summary.get('columns', ['ID', 'Item', 'Delivered', 'Reference', 'Notes'])
        rows = []
        for sid, name, what, ref, when, note in summary['rows']:
            cls = ' class="parked"' if summary.get('flag') and summary['flag'] in ref else ''
            rows.append(f'<tr{cls}><td class="id">{sid}</td><td><strong>{name}</strong></td><td class="what">{what}</td>'
                        f'<td class="pr">{ref}<br>{when}</td><td class="note">{note}</td></tr>')
        th = ''.join(f'<th>{c}</th>' for c in cols)
        h.append(f'''<section id="summary"><h2>{summary.get('title', 'Summary')}</h2>
  <p class="lede">{summary.get('lede', '')}</p>
  <div class="scroller"><table><thead><tr>{th}</tr></thead><tbody>{''.join(rows)}</tbody></table></div>
  {f'<p class="lede">{summary["after"]}</p>' if summary.get('after') else ''}
</section>''')

    # Findings
    tally = ''.join(f'<div><span class="n">{counts.get(g, 0)}</span><span class="k">{g}</span></div>' for g in ORDER)
    toc = ' '.join(f'<a href="#{g["id"]}">{g["title"]}</a>' for g in groups)
    buttons = ''.join(f'<button type="button" data-g="{g.lower()}" aria-pressed="false">{g}</button>'
                      for g in ORDER if counts.get(g))
    h.append(f'''<section id="findings">
  <h2>Findings</h2>
  <p class="lede">Each finding has one grade. {GRADE_HELP} {meta.get('evidence_note', '')}</p>
  <div class="tally">{tally}</div>
  <div class="filters" role="group" aria-label="Show findings by grade"><span>Show</span>
    <button type="button" data-g="all" aria-pressed="true">All</button>{buttons}
  </div>
  <nav class="toc" aria-label="Finding groups">{toc}</nav>''')
    for g in groups:
        c = collections.Counter(f[1] for f in g['findings'])
        cs = ', '.join(f'{c[x]} {x}' for x in ORDER if c[x])
        h.append(f'<div class="group" id="{g["id"]}"><h3>{g["title"]} <span class="fid">({cs})</span></h3>')
        if g.get('intro'):
            h.append(f'<p class="group-intro">{g["intro"]}</p>')
        for fid, grade, t, body, ev, nxt in g['findings']:
            h.append(f'''<article class="finding" id="{fid}" data-grade="{grade.lower()}">
  <div class="head"><span class="fid">{fid}</span>{chip(grade)}<h3>{t}</h3></div>
  <p class="body">{body}</p>
  <div class="fields"><div class="field"><span class="label">Evidence</span><p>{ev}</p></div>
  <div class="field"><span class="label">Next step</span><p>{nxt}</p></div></div>
</article>''')
        h.append('</div>')
    h.append('</section>')

    # Guideline refinements (optional)
    items = refine.get('items', [])
    if items:
        rc = collections.Counter(r[1] for r in items)
        rcs = ', '.join(f'{rc[x]} {x}' for x in ORDER if rc[x])
        h.append(f'''<section id="refinements"><h2>{refine.get('title', 'Guideline refinements')}</h2>
  <p class="lede">{refine.get('intro', '')} Graded like the findings: {rcs}.</p>''')
        for rid, grade, t, now, prop in items:
            h.append(f'''<article class="finding refine" data-grade="{grade.lower()}">
  <div class="head"><span class="fid">{rid}</span>{chip(grade)}<h3>{t}</h3></div>
  <p class="now"><strong>Today:</strong> {now}</p>
  <p class="proposed">{prop}</p>
</article>''')
        h.append('</section>')

    if footer:
        h.append('<footer>' + ''.join(f'<p><strong>{b}.</strong> {t}</p>' for b, t in footer) + '</footer>')
    h.append('</div>' + SCRIPT)

    with open(out_path, 'w') as f:
        f.write('\n'.join(h))
    print(f'{out_path}: {total} findings', dict(counts))


SCRIPT = '''
<script>
(function () {
  var buttons = document.querySelectorAll('.filters button');
  var items = document.querySelectorAll('#findings .finding');
  var groups = document.querySelectorAll('#findings .group');
  function show(g) {
    buttons.forEach(function (b) { b.setAttribute('aria-pressed', String(b.dataset.g === g)); });
    items.forEach(function (el) { el.hidden = !(g === 'all' || el.dataset.grade === g); });
    groups.forEach(function (grp) { grp.hidden = !grp.querySelector('.finding:not([hidden])'); });
  }
  buttons.forEach(function (b) { b.addEventListener('click', function () { show(b.dataset.g); }); });
  document.querySelectorAll('.well a').forEach(function (a) { a.addEventListener('click', function () { show('all'); }); });
})();
</script>'''

CSS = r"""
<title>__TITLE__</title>
<meta name="claude:viewport-fit-sides" content="cover">
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Archivo:wght@500;600;700&family=IBM+Plex+Sans:ital,wght@0,400;0,500;0,600;1,400&family=IBM+Plex+Mono:wght@400;500&display=swap">
<style>
/* Layout: one reading column; summary table, then findings grouped by area, each a compact record. */
:root {
  --ground: #F4F6F6; --surface: #FFFFFF; --sunk: #EBEFEF; --ink: #141B1B; --ink-soft: #3B4747; --ink-mute: #5E6B6B;
  --rule: #D5DDDC; --rule-soft: #E5EAEA; --accent: #165E73;
  --nit: #55626A; --nit-soft: #E9EDEF; --warn: #8A5A05; --warn-soft: #FBF0DC;
  --err: #A32E2E; --err-soft: #F7E7E6; --fatal: #7A1F5C; --fatal-soft: #F5E4EF; --ok: #2F6B3F; --ok-soft: #E5F0E7;
  --display: "Archivo", "Helvetica Neue", Arial, sans-serif;
  --body: "IBM Plex Sans", -apple-system, "Segoe UI", Roboto, sans-serif;
  --mono: "IBM Plex Mono", ui-monospace, "SF Mono", Menlo, monospace;
}
@media (prefers-color-scheme: dark) { :root:not([data-theme="light"]) {
  --ground: #0E1415; --surface: #151D1E; --sunk: #111819; --ink: #E7EEEE; --ink-soft: #C0CBCB; --ink-mute: #8B9999;
  --rule: #273334; --rule-soft: #1D2728; --accent: #6DB8CC;
  --nit: #A9B5BB; --nit-soft: #222B2E; --warn: #D9A94F; --warn-soft: #2E2716; --err: #E58C86; --err-soft: #341D1C;
  --fatal: #E08CC4; --fatal-soft: #331C2B; --ok: #8CC39A; --ok-soft: #1A2A1E; color-scheme: dark; } }
:root[data-theme="dark"] {
  --ground: #0E1415; --surface: #151D1E; --sunk: #111819; --ink: #E7EEEE; --ink-soft: #C0CBCB; --ink-mute: #8B9999;
  --rule: #273334; --rule-soft: #1D2728; --accent: #6DB8CC;
  --nit: #A9B5BB; --nit-soft: #222B2E; --warn: #D9A94F; --warn-soft: #2E2716; --err: #E58C86; --err-soft: #341D1C;
  --fatal: #E08CC4; --fatal-soft: #331C2B; --ok: #8CC39A; --ok-soft: #1A2A1E; color-scheme: dark; }
* { box-sizing: border-box; }
body { margin: 0; background: var(--ground); color: var(--ink); font-family: var(--body); font-size: 15.5px; line-height: 1.6; -webkit-font-smoothing: antialiased; }
.wrap { max-width: 64rem; margin: 0 auto; padding-block: 2.75rem 5rem;
  padding-left: max(16px, env(safe-area-inset-left, 0px)); padding-right: max(16px, env(safe-area-inset-right, 0px));
  display: flex; flex-direction: column; gap: 3rem; }
a { color: var(--accent); }
a:focus-visible, button:focus-visible { outline: 2px solid var(--accent); outline-offset: 2px; border-radius: 3px; }
code { font-family: var(--mono); font-size: .85em; background: var(--sunk); padding: .08em .32em; border-radius: 3px; color: var(--ink-soft); overflow-wrap: anywhere; }
h1, h2, h3 { font-family: var(--display); text-wrap: balance; margin: 0; }
h1 { font-size: clamp(2rem, 5vw, 2.8rem); font-weight: 700; letter-spacing: -.02em; line-height: 1.1; }
h2 { font-size: 1.45rem; font-weight: 600; letter-spacing: -.01em; padding-bottom: .5rem; border-bottom: 1px solid var(--rule); }
h3 { font-size: 1.1rem; font-weight: 600; }
p { margin: 0; }
.eyebrow { font-family: var(--display); font-size: .75rem; font-weight: 600; letter-spacing: .14em; text-transform: uppercase; color: var(--accent); }
header { display: flex; flex-direction: column; gap: 1rem; }
.standfirst { font-size: 1.1rem; color: var(--ink-soft); max-width: 62ch; }
.meta { font-family: var(--mono); font-size: .8rem; color: var(--ink-mute); display: flex; flex-wrap: wrap; gap: .3rem 1.25rem; }
section { display: flex; flex-direction: column; gap: 1.25rem; min-width: 0; }
.wrap > *, .group, .finding, .scroller { min-width: 0; }
.lede { color: var(--ink-soft); max-width: 68ch; }
.tally { display: grid; grid-template-columns: repeat(auto-fit, minmax(10rem, 1fr)); gap: 1px; background: var(--rule); border: 1px solid var(--rule); border-radius: 4px; overflow: hidden; }
.tally div { background: var(--surface); padding: 1rem 1.15rem; display: flex; flex-direction: column; gap: .2rem; }
.tally .n { font-family: var(--display); font-size: 1.75rem; font-weight: 700; line-height: 1.1; font-variant-numeric: tabular-nums; }
.tally .k { font-size: .72rem; letter-spacing: .08em; text-transform: uppercase; color: var(--ink-mute); font-weight: 500; }
.outcome { display: grid; gap: .75rem; }
.outcome li { color: var(--ink-soft); }
.outcome ul { margin: 0; padding-left: 1.1rem; display: flex; flex-direction: column; gap: .45rem; }
.scroller { overflow-x: auto; border: 1px solid var(--rule); border-radius: 4px; background: var(--surface); }
table { border-collapse: collapse; width: 100%; min-width: 46rem; font-size: .9rem; }
th, td { text-align: left; padding: .6rem .8rem; border-bottom: 1px solid var(--rule-soft); vertical-align: top; }
thead th { font-family: var(--display); font-size: .68rem; letter-spacing: .09em; text-transform: uppercase; color: var(--ink-mute); font-weight: 600; background: var(--sunk); border-bottom: 1px solid var(--rule); }
tbody tr:last-child td { border-bottom: none; }
td.id { font-family: var(--mono); font-weight: 500; white-space: nowrap; }
td.pr { font-family: var(--mono); font-size: .8rem; white-space: nowrap; color: var(--ink); }
td.note { color: var(--ink-mute); font-size: .85rem; }
td.what { color: var(--ink-soft); }
tr.parked td.pr { color: var(--warn); }
.grade { display: inline-block; font-size: .66rem; font-weight: 600; letter-spacing: .07em; text-transform: uppercase; padding: .16rem .45rem; border-radius: 3px; white-space: nowrap; }
.g-nit { background: var(--nit-soft); color: var(--nit); }
.g-warning { background: var(--warn-soft); color: var(--warn); }
.g-error { background: var(--err-soft); color: var(--err); }
.g-fatal { background: var(--fatal-soft); color: var(--fatal); }
.filters { display: flex; flex-wrap: wrap; gap: .5rem; align-items: center; }
.filters span { font-size: .8rem; color: var(--ink-mute); margin-right: .25rem; }
.filters button { font: inherit; font-size: .82rem; padding: .3rem .7rem; border-radius: 999px; border: 1px solid var(--rule); background: var(--surface); color: var(--ink-soft); cursor: pointer; }
.filters button[aria-pressed="true"] { background: var(--ink); color: var(--surface); border-color: var(--ink); }
.toc { display: flex; flex-wrap: wrap; gap: .4rem 1rem; font-size: .9rem; }
.group { display: flex; flex-direction: column; gap: .9rem; }
.group-intro { color: var(--ink-soft); font-size: .95rem; max-width: 70ch; }
.finding { background: var(--surface); border: 1px solid var(--rule); border-radius: 4px; padding: 1rem 1.15rem; display: flex; flex-direction: column; gap: .6rem; }
.finding .head { display: flex; flex-wrap: wrap; align-items: baseline; gap: .35rem .65rem; }
.finding .fid { font-family: var(--mono); font-size: .8rem; color: var(--ink-mute); font-weight: 500; }
.finding h3 { font-size: 1rem; flex: 1 1 18rem; min-width: 0; }
.finding .body { color: var(--ink-soft); font-size: .93rem; }
.fields { display: grid; grid-template-columns: 1fr 1fr; gap: .6rem 1.25rem; }
.field { display: flex; flex-direction: column; gap: .15rem; min-width: 0; }
.field .label { font-size: .66rem; font-weight: 600; letter-spacing: .09em; text-transform: uppercase; color: var(--ink-mute); }
.field p { font-size: .88rem; color: var(--ink-soft); }
.refine .now { font-size: .88rem; color: var(--ink-mute); }
.refine .proposed { font-size: .92rem; color: var(--ink); background: var(--sunk); border-radius: 3px; padding: .6rem .75rem; }
.well { display: grid; grid-template-columns: repeat(auto-fit, minmax(18rem, 1fr)); gap: .9rem; }
.well div { background: var(--surface); border: 1px solid var(--rule); border-radius: 4px; padding: .9rem 1.05rem; display: flex; flex-direction: column; gap: .35rem; }
.well h3 { font-size: .98rem; color: var(--ok); }
.well p { font-size: .9rem; color: var(--ink-soft); }
.well.wrong h3 { color: var(--err); }
footer { border-top: 1px solid var(--rule); padding-top: 1.25rem; color: var(--ink-mute); font-size: .86rem; display: flex; flex-direction: column; gap: .5rem; }
footer p { max-width: 75ch; }
@media (max-width: 40rem) { .fields { grid-template-columns: 1fr; } }
@media (prefers-reduced-motion: reduce) { * { transition: none !important; } }
</style>
"""

if __name__ == '__main__':
    if len(sys.argv) != 3:
        raise SystemExit('usage: python3 -I build.py data.py out.html')
    main(sys.argv[1], sys.argv[2])
