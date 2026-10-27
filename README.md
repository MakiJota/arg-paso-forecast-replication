# Replication files: *How much do mandatory primaries reveal? Forecasting Argentina's presidency from the PASO, 2011–2023*

Jonathan Cohen

This repository holds the code, input data and generated figures and tables for the article.
The 2023 forecast and its scoring (including the figures and tables those steps produce) can be
regenerated from the files here. So can most of the validation analyses. The exceptions are
listed under [What is not included](#what-is-not-included).

## Layout

```
paper/
├── code/
│   ├── R/model.R                    simulation, electoral rules (Arts. 97–98), scoring functions
│   ├── R/agnostic.R                 persistence ("retention + noise") model, chained first round -> runoff
│   ├── run_all.R                    2023 forecast, scoring, naive benchmark          (Sections 6.2–6.3)
│   ├── validation/
│   │   ├── 01_build_department_data.R   department-level 2023 dataset (needs raw DINE files, see below)
│   │   ├── 02_ecological_inference.R    ecological inference, 2023                   (Section 6.4)
│   │   ├── 03_paso_history.py / .R      PASO vs. general election, 2011–2023         (Section 6.1)
│   │   └── 04_fetch_2015.py / 04_ei_2015.R  ecological inference, 2015               (Section 6.6)
│   ├── robustness/
│   │   ├── 05_stress_test.R             how much the PASO alone determines           (Section 6.5)
│   │   └── 06_historical_transfer_forecast.R  cross-cycle validation 2015 <-> 2023   (Section 6.6)
│   ├── markets/07_market_reactions.R    market reactions on election nights          (Section 6.7)
│   └── benchmark/08_poll_benchmark.py   final-poll benchmark                         (Sections 5.5, 6.2)
├── data/            inputs (CSV) and small raw API downloads; see paper/data/README.md
└── output/
    ├── figures/     every figure in the article (PNG and PDF)
    ├── tables/      every table, plus supporting tables (CSV)
    ├── key_numbers*.tex   numbers quoted in the text, as LaTeX macros
    └── sessionInfo.txt    R session used to produce the outputs
```

### Figures

| Article | File |
|---|---|
| Fig. 1 | `paper/output/figures/fig_paso_history.png` |
| Fig. 2 | `paper/output/figures/fig_first_round_density.png` |
| Fig. 3 | `paper/output/figures/fig_first_round_outcomes.png` |
| Fig. 4 | `paper/output/figures/fig_runoff.png` |
| Fig. 5 | `paper/output/figures/fig_validation_general.png` |
| Fig. 6 | `paper/output/figures/fig_validation_runoff.png` |
| Fig. 7 | `paper/output/figures/fig_stress_test.png` |
| Fig. 8 | `paper/output/figures/fig_market_reactions.png` |
| Supplementary | `paper/output/figures/fig_ei2015_{general,runoff}.png` |

## Requirements

- R ≥ 4.3 with `ggplot2`, `patchwork`, `data.table`, `jsonlite`, `lphom`, `eiPack`, `ggrepel`
- Python ≥ 3.9 (standard library only) for the poll benchmark and the API download scripts

```r
install.packages(c("ggplot2", "patchwork", "data.table", "jsonlite", "lphom", "eiPack", "ggrepel"))
```

The outputs in this repository were produced with R 4.5.1 on Windows 11 (`paper/output/sessionInfo.txt`).

## Reproduce

Run every command **from the repository root**: the scripts use paths relative to it.

```bash
Rscript paper/code/run_all.R                                  # 2023 forecast and scoring
Rscript paper/code/validation/02_ecological_inference.R       # EI 2023 (writes the .rds used by step 06)
Rscript paper/code/validation/03_paso_history.R               # PASO record 2011–2023
Rscript paper/code/validation/04_ei_2015.R                    # EI 2015
Rscript paper/code/robustness/05_stress_test.R
Rscript paper/code/robustness/06_historical_transfer_forecast.R   # needs the outputs of 02 and 04
python  paper/code/benchmark/08_poll_benchmark.py
```

Each script overwrites its files in `paper/output/`. Random seeds are fixed in each script, so the
outputs are identical on re-runs.

The Bayesian ecological inference (90% credible intervals) is slow and off by default.
To run it, set `RUN_BAYES=TRUE` for step 02 and pass `--bayes` to step 04:

```bash
RUN_BAYES=TRUE Rscript paper/code/validation/02_ecological_inference.R
Rscript paper/code/validation/04_ei_2015.R --bayes
```

### Rebuilding the raw downloads

The small DINE API responses that steps 03 and 04 read are included in `paper/data/raw/`.
To download them again from the official results API:

```bash
python paper/code/validation/03_paso_history.py
python paper/code/validation/04_fetch_2015.py
```

## What is not included

- **2023 polling-station (mesa) files** for the general election and runoff (about 1.2 GB), read by
  `01_build_department_data.R`. Its output, `paper/data/departments_2023.csv`, is included, so every
  later step runs without them. They are available from the Dirección Nacional Electoral (DINE).
- **Raw market series** (Ámbito, Yahoo Finance, Bluelytics), read by `07_market_reactions.R`. They are
  third-party data and are not redistributed here. The derived results are included:
  `paper/output/tables/market_reactions.csv`, `market_placebo_summary.csv` and Fig. 8.

## Data sources and licences

- Electoral results: Dirección Nacional Electoral, provisional counts (CC-BY 4.0); definitive counts
  from the Cámara Nacional Electoral. See `paper/data/README.md` for details.
- Polls: English Wikipedia, *Opinion polling for the 2023 Argentine general election* (CC BY-SA 4.0).
- Code: MIT licence (`LICENSE`).

## Citation

If you use these files, please cite the article:

> Cohen, J. (forthcoming). How much do mandatory primaries reveal? Forecasting Argentina's presidency
> from the PASO, 2011–2023.
