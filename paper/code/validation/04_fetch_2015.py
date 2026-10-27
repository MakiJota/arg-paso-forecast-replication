"""Fetch 2015 presidential results (PASO, general, runoff) at department level
from the official DINE results API (provisional count, tipoRecuento=1).

Raw JSON is cached in paper/data/raw/api_2015/ (gitignored); re-running only
downloads what is missing. Run from the repository root:

    python paper/code/validation/04_fetch_2015.py
"""
import json
import os
import time
import urllib.parse
import urllib.request

BASE = "https://resultados.mininterior.gob.ar/api"
OUT = os.path.join("paper", "data", "raw", "api_2015")
UA = {"User-Agent": "Mozilla/5.0 (research)"}
ELECTIONS = {1: "paso", 2: "generales", 3: "balotaje"}


def get(url, tries=5):
    for k in range(tries):
        try:
            req = urllib.request.Request(url, headers=UA)
            with urllib.request.urlopen(req, timeout=60) as r:
                return r.read()
        except Exception as e:  # noqa: BLE001
            wait = 2 ** k
            print(f"  retry {k + 1} after error {e!r}; waiting {wait}s")
            time.sleep(wait)
    return None


def main():
    os.makedirs(OUT, exist_ok=True)
    menu_path = os.path.join(OUT, "menu.json")
    if not os.path.exists(menu_path):
        raw = get(f"{BASE}/menu?" + urllib.parse.urlencode({"año": 2015}))
        open(menu_path, "wb").write(raw)
    menu = json.load(open(menu_path, encoding="utf-8"))

    # Presidential cargo id per election, and the union of departments
    cargo = {}
    depts = {}
    for e in menu:
        c = [c for c in e["Cargos"] if "PRESIDENTE" in c["Cargo"].upper()][0]
        cargo[e["IdEleccion"]] = c["IdCargo"]
        for d in c["Distritos"]:
            if d["IdDistrito"] == 0:
                continue
            for sp in d["SeccionesProvinciales"]:
                for s in sp["Secciones"]:
                    if s["IdSeccion"] is not None:
                        depts[(d["IdDistrito"], s["IdSeccion"])] = (d["Distrito"], s["Seccion"])
    print("cargo ids:", cargo, "departments (union):", len(depts))

    failed = []
    for eid, ename in ELECTIONS.items():
        d_out = os.path.join(OUT, ename)
        os.makedirs(d_out, exist_ok=True)
        base_q = dict(anioEleccion=2015, tipoRecuento=1, tipoEleccion=eid, categoriaId=cargo[eid])
        # national total
        f = os.path.join(d_out, "national.json")
        if not os.path.exists(f):
            raw = get(f"{BASE}/resultados/getResultados?" + urllib.parse.urlencode(base_q))
            if raw:
                open(f, "wb").write(raw)
            time.sleep(0.3)
        for (dist, sec) in sorted(depts):
            f = os.path.join(d_out, f"d{dist:02d}_s{sec:03d}.json")
            if os.path.exists(f):
                continue
            q = dict(base_q, distritoId=dist, seccionId=sec)
            raw = get(f"{BASE}/resultados/getResultados?" + urllib.parse.urlencode(q))
            if raw is None:
                failed.append((ename, dist, sec))
                continue
            open(f, "wb").write(raw)
            time.sleep(0.3)
        print(ename, "done")
    json.dump(failed, open(os.path.join(OUT, "failed.json"), "w"))
    print("failed requests:", failed)


if __name__ == "__main__":
    main()
