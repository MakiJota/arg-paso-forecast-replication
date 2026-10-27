"""Fetch the daily market series used by 07_market_reactions.R and check_raw.R.

  blue_ambito.json      parallel ("blue") peso rate                 - mercados.ambito.com
  blue_bluelytics.json  official peso rate (context)                - api.bluelytics.com.ar
  merval_yahoo.json    Merval index (^MERV), 2011-2023             - Yahoo Finance
  riesgo_ambito.json    EMBI country-risk spread, Argentina         - mercados.ambito.com

Raw JSON is saved to paper/data/raw/markets/ (gitignored: third-party data,
not redistributed). The sources may revise or extend their series, so a fresh
download can differ slightly from the one used in the article (20 Oct 2026).
Run from the repository root:

    python paper/code/markets/fetch_markets.py
"""
import os
import sys
import time
import urllib.request

OUT = sys.argv[1] if len(sys.argv) > 1 else os.path.join("paper", "data", "raw", "markets")
UA = {"User-Agent": "Mozilla/5.0 (research)"}
SERIES = {
    "blue_ambito": ("https://mercados.ambito.com//dolar/informal/historico-general/2010-01-01/2024-01-31",
                    {"Referer": "https://www.ambito.com/"}),
    "blue_bluelytics": ("https://api.bluelytics.com.ar/v2/evolution.json", {}),
    # period1/period2 = 2011-01-01 / 2024-01-01 (Unix time)
    "merval_yahoo": ("https://query1.finance.yahoo.com/v8/finance/chart/%5EMERV"
                     "?period1=1293840000&period2=1704067200&interval=1d", {}),
    "riesgo_ambito": ("https://mercados.ambito.com//riesgopais/historico-general/2010-01-01/2024-01-31",
                      {"Referer": "https://www.ambito.com/"}),
}


def get(url, headers, tries=5):
    for k in range(tries):
        try:
            req = urllib.request.Request(url, headers={**UA, **headers})
            with urllib.request.urlopen(req, timeout=90) as r:
                return r.read()
        except Exception as e:  # noqa: BLE001
            wait = 2 ** k
            print(f"  retry {k + 1} after error {e!r}; waiting {wait}s")
            time.sleep(wait)
    raise RuntimeError(f"failed: {url}")


os.makedirs(OUT, exist_ok=True)
for name, (url, headers) in SERIES.items():
    data = get(url, headers)
    with open(os.path.join(OUT, name + ".json"), "wb") as f:
        f.write(data)
    print(f"{name}: {len(data):,} bytes")
