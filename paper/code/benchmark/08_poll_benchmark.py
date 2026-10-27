"""Poll benchmark for the 2023 forecast.

Source: English Wikipedia, "Opinion polling for the 2023 Argentine general election"
(wikitext downloaded to paper/data/raw/polls/polls_wikipedia_en.wikitext; revision id
recorded in the output). Tables used:
  * First round: "By political coalition / 2023" (after the PASO each coalition
    had a single candidate). Polls with fieldwork ending 14 Aug - 13 Oct 2023
    (the legal ban on publishing polls began on 14 Oct).
  * Runoff: "Second round / After the primaries", Massa vs Milei, fieldwork
    ending after 22 Oct 2023.
Wikitables use rowspan/colspan, so the parser rebuilds the full cell grid.
Outputs: paper/data/polls_2023_first_round.csv, paper/data/polls_2023_runoff.csv,
         paper/output/tables/poll_benchmark.csv, paper/output/key_numbers_polls.tex
Usage (repo root): python -I paper/code/benchmark/08_poll_benchmark.py
"""
import csv, datetime as dt, re, statistics as st, sys
sys.stdout.reconfigure(encoding='utf-8')

WT = 'paper/data/raw/polls/polls_wikipedia_en.wikitext'
ACT_G = {'MASSA': 36.78, 'MILEI': 29.99, 'BULLRICH': 23.81, 'SCHIARETTI': 6.73, 'BREGMAN': 2.70}
ACT_R_MILEI = 55.65
MONTHS = {m: i + 1 for i, m in enumerate(['january', 'february', 'march', 'april', 'may', 'june', 'july',
                                          'august', 'september', 'october', 'november', 'december'])}
wt = open(WT, encoding='utf-8').read()

def strip(s):
    s = re.sub(r'<ref[^>/]*/>|<ref[^>]*>.*?</ref>', '', s, flags=re.S)
    s = re.sub(r'\{\{[^{}]*\}\}', '', s)
    s = re.sub(r"'''|''|<br\s*/?>", ' ', s)
    return re.sub(r'\s+', ' ', s).strip()

def split_cells(line):
    body = line[1:]
    parts = re.split(r'\|\|', body)
    out = []
    for p in parts:
        attrs, val = '', p
        # attribute section: "style=... |value" (a single pipe not inside [[ ]] or {{ }})
        depth, cut = 0, None
        for i, ch in enumerate(p):
            if p[i:i + 2] in ('[[', '{{'): depth += 1
            if p[i:i + 2] in (']]', '}}'): depth -= 1
            if ch == '|' and depth == 0: cut = i; break
        if cut is not None:
            attrs, val = p[:cut], p[cut + 1:]
        rs = re.search(r'rowspan\s*=\s*"?(\d+)', attrs); cs = re.search(r'colspan\s*=\s*"?(\d+)', attrs)
        out.append((strip(val), int(rs.group(1)) if rs else 1, int(cs.group(1)) if cs else 1))
    return out

def grid(table_text):
    rows, pending = [], {}          # pending[col] = (value, remaining_rows)
    for raw in re.split(r'\n\|-[^\n]*', table_text)[1:]:
        cells = []
        for line in raw.split('\n'):
            line = line.strip()
            if line.startswith('|}'): break
            if line.startswith('|') and not line.startswith('|-'):
                cells.extend(split_cells(line))
        if not cells:
            continue
        row, col, k = [], 0, 0
        while k < len(cells) or col in pending:
            if col in pending:
                v, left = pending[col]; row.append(v)
                if left > 1: pending[col] = (v, left - 1)
                else: del pending[col]
                col += 1; continue
            v, rs, cs = cells[k]; k += 1
            for _ in range(cs):
                row.append(v)
                if rs > 1: pending[col] = (v, rs - 1)
                col += 1
        rows.append(row)
    return rows

def section_table(heading):
    i = wt.index(heading); j = wt.index('{|', i); e = wt.index('\n|}', j)
    return wt[j:e]

def end_date(s):
    m = re.findall(r'(\d{1,2})\s+([A-Za-z]+)\s+(\d{4})', s)
    if not m: return None
    d, mo, y = m[-1]
    return dt.date(int(y), MONTHS.get(mo.lower(), 0) or 1, int(d)) if mo.lower() in MONTHS else None

def num(s):
    s = s.replace('%', '').replace(',', '.').strip()
    try: return float(s)
    except ValueError: return None

# ---------------------------------------------------------------- first round
fr_rows = []
for r in grid(section_table('==== 2023 ====')):
    if len(r) < 8: continue
    d = end_date(r[0])
    if not d or not (dt.date(2023, 8, 14) <= d <= dt.date(2023, 10, 13)): continue
    vals = dict(MASSA=num(r[3]), BULLRICH=num(r[4]), MILEI=num(r[5]), BREGMAN=num(r[6]), SCHIARETTI=num(r[7]))
    if any(v is None for v in vals.values()): continue
    tot = sum(vals.values())
    fr_rows.append(dict(end_date=d.isoformat(), firm=r[1], sample=r[2], **{k: round(100 * v / tot, 2) for k, v in vals.items()}))
# ---------------------------------------------------------------- runoff
ro_rows = []
for r in grid(section_table('=== After the primaries ===')):
    if len(r) < 6: continue
    d = end_date(r[0])
    if not d or not (dt.date(2023, 10, 23) <= d <= dt.date(2023, 11, 18)): continue
    massa, milei = num(r[4]), num(r[5])
    if massa is None or milei is None or massa + milei == 0: continue
    ro_rows.append(dict(end_date=d.isoformat(), firm=r[1], sample=r[2], massa=massa, milei=milei,
                        milei_two_party=round(100 * milei / (massa + milei), 2)))

def write(path, rows):
    with open(path, 'w', newline='', encoding='utf-8') as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0].keys())); w.writeheader(); w.writerows(rows)
write('paper/data/polls_2023_first_round.csv', fr_rows)
write('paper/data/polls_2023_runoff.csv', ro_rows)

# ---------------------------------------------------------------- benchmark
def summarise(rows, since):
    sel = [r for r in rows if r['end_date'] >= since]
    avg = {k: st.mean(r[k] for r in sel) for k in ACT_G}
    mae = st.mean(abs(avg[k] - ACT_G[k]) for k in ACT_G)
    first = [max(ACT_G, key=lambda k: r[k]) for r in sel]
    top2 = [tuple(sorted(sorted(ACT_G, key=lambda k: -r[k])[:2])) for r in sel]
    return dict(window=f'{since} to 2023-10-13', n_polls=len(sel), mae=mae,
                p_massa_first=sum(f == 'MASSA' for f in first) / len(sel),
                p_massa_milei_top2=sum(t == ('MASSA', 'MILEI') for t in top2) / len(sel),
                **{f'avg_{k.lower()}': avg[k] for k in ACT_G})
fr_final = summarise(fr_rows, '2023-09-23')     # final three weeks before the ban
fr_all = summarise(fr_rows, '2023-08-14')       # all post-PASO polls
ro_avg = st.mean(r['milei_two_party'] for r in ro_rows)
ro_ahead = sum(r['milei_two_party'] > 50 for r in ro_rows) / len(ro_rows)

with open('paper/output/tables/poll_benchmark.csv', 'w', newline='', encoding='utf-8') as fh:
    w = csv.DictWriter(fh, fieldnames=list(fr_final.keys())); w.writeheader(); w.writerows([fr_final, fr_all])
rev = re.search(r'revid', wt)
K = {'PollNFinal': fr_final['n_polls'], 'PollNAll': fr_all['n_polls'],
     'PollMAEFinal': f"{fr_final['mae']:.2f}", 'PollMAEAll': f"{fr_all['mae']:.2f}",
     'PollMassaFirstFinal': f"{100 * fr_final['p_massa_first']:.0f}",
     'PollMMTopTwoFinal': f"{100 * fr_final['p_massa_milei_top2']:.0f}",
     'PollAvgMassa': f"{fr_final['avg_massa']:.1f}", 'PollAvgMilei': f"{fr_final['avg_milei']:.1f}",
     'PollAvgBullrich': f"{fr_final['avg_bullrich']:.1f}",
     'PollRunoffN': len(ro_rows), 'PollRunoffMilei': f'{ro_avg:.1f}', 'PollRunoffMileiAhead': f'{100 * ro_ahead:.0f}'}
with open('paper/output/key_numbers_polls.tex', 'w', encoding='utf-8') as fh:
    fh.write('% Auto-generated by paper/code/benchmark/08_poll_benchmark.py\n')
    fh.writelines(f'\\newcommand{{\\{k}}}{{{v}}}\n' for k, v in K.items())
print('First round, final 3 weeks:', fr_final)
print('First round, all post-PASO:', fr_all)
print(f'Runoff polls: n={len(ro_rows)}, Milei two-party avg={ro_avg:.2f}, share with Milei ahead={ro_ahead:.2f}')
for r in fr_rows[:5]: print('  sample row:', r)
for r in ro_rows[:5]: print('  sample runoff row:', r)
