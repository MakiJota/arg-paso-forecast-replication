# Data

| File | Content |
|---|---|
| `paso_2023.csv` | PASO 13 Aug 2023, presidential category, eleven origin categories. `votes_provisional`: the provisional count used for the ex-ante forecast (what was known on PASO night). `votes_definitive`: the CNE definitive count (escrutinio definitivo), used for sensitivity checks. OTROS includes Solano (FIT-U list B). NOVOTO = registered voters − voters. |
| `actual_results_2023.csv` | Official results of the 22 Oct 2023 general election and the 19 Nov 2023 runoff, CNE definitive count (votes and % of positive votes). These are the evaluation targets throughout the paper. |
| `ranges_first_round_{min,max}.csv` | Transfer bounds, PASO → first round, as fixed in August 2023 in the author's original forecast scripts. |
| `ranges_runoff_{min,max}.csv` | Transfer bounds, PASO → runoff, as fixed in October 2023 in the author's original forecast scripts. |
| `departments_2023.csv` | Department-level (n = 528) returns for the three 2023 elections, built by `code/validation/01_build_department_data.R` (provisional count). |
| `departments_2015.csv` | Department-level returns for 2015, built by `code/validation/04_*`. |
| `paso_history_national.csv` | National PASO and general results 2011–2023 (provisional counts), built by `code/validation/03_paso_history.*`. |
| `raw/` | DINE API responses read by steps 03 and 04 (`history_api/`, `api_2015/`, `paso_api/`) and the Wikipedia poll page (`polls/`). The 2023 polling-station files and the market series are not included (see the main README). |

Sources for the definitive counts: CNE escrutinio definitivo, as reported by Infobae (PASO: 31 Aug 2023; general election: 2–3 Nov 2023; runoff: 29 Nov 2023), and the CNE-based tables in the Spanish Wikipedia articles on the 2023 primaries and presidential election. The PASO list-level figures sum exactly to the reported coalition totals and to the registered electorate (35,405,013).

Rows of the range matrices are PASO categories (origin), and columns are destinations, in the same order:
MILEI, BULLRICH, LARRETA, MASSA, GRABOIS, SCHIARETTI, BREGMAN, OTROS, BLANCO, ANULADOS, NOVOTO.
