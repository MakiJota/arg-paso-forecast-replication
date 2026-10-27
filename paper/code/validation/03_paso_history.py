"""Fetch national presidential results (PASO, general, runoff) for 2011-2023
from the official DINE results API (provisional count, tipoRecuento=1).

Raw JSON is saved to paper/data/raw/history_api/ (gitignored).
Run from the repo root:  python paper/code/validation/03_paso_history.py
"""
import json
import os
import time
import urllib.parse
import urllib.request

BASE = "https://resultados.mininterior.gob.ar/api"
OUT = os.path.join("paper", "data", "raw", "history_api")
YEARS = [2011, 2015, 2019, 2023]
HDR = {"User-Agent": "Mozilla/5.0 (research)"}


def get(url, tries=5):
    for i in range(tries):
        try:
            req = urllib.request.Request(url, headers=HDR)
            with urllib.request.urlopen(req, timeout=60) as r:
                data = r.read()
            time.sleep(0.5)
            return json.loads(data.decode("utf-8"))
        except Exception as e:  # noqa: BLE001
            print(f"  retry {i + 1} for {url}: {e}")
            time.sleep(2 * (i + 1))
    raise RuntimeError(f"failed: {url}")


def save(obj, name):
    with open(os.path.join(OUT, name), "w", encoding="utf-8") as f:
        json.dump(obj, f, ensure_ascii=False, indent=1)


def is_empty(res):
    er = res.get("estadoRecuento") or {}
    return not res.get("valoresTotalizadosPositivos") or not er.get("cantidadVotantes")


def aggregate_sections(y, ide, cargo):
    """Fallback when the national query is empty (2019 PASO): query every
    section (departamento) listed in the menu and sum them. District-level
    queries are NOT used because several districts return only a handful of
    mesas (e.g. 2019 PASO Cordoba: 90 mesas)."""
    sec_dir = os.path.join(OUT, f"sections_{y}_e{ide}")
    os.makedirs(sec_dir, exist_ok=True)
    est_keys = ["mesasTotalizadas", "cantidadElectores", "cantidadVotantes"]
    oth_keys = ["votosNulos", "votosEnBlanco", "votosRecurridosComandoImpugnados"]
    est = {k: 0 for k in est_keys}
    oth = {k: 0 for k in oth_keys}
    pos = {}
    log = []
    for d in cargo["Distritos"]:
        did = d["IdDistrito"]
        if did == 0:
            continue
        for sp in d["SeccionesProvinciales"]:
            for s in sp["Secciones"]:
                sid = s["IdSeccion"]
                if sid is None:
                    continue
                fn = os.path.join(sec_dir, f"d{did:02d}_s{sid:03d}.json")
                if os.path.exists(fn):
                    with open(fn, encoding="utf-8") as f:
                        r = json.load(f)
                else:
                    q = urllib.parse.urlencode({
                        "anioEleccion": y, "tipoRecuento": 1, "tipoEleccion": ide,
                        "categoriaId": cargo["IdCargo"], "distritoId": did,
                        "seccionId": sid})
                    r = get(f"{BASE}/resultados/getResultados?{q}")
                    with open(fn, "w", encoding="utf-8") as f:
                        json.dump(r, f, ensure_ascii=False)
                er = r["estadoRecuento"]
                ot = r["valoresTotalizadosOtros"]
                for k in est_keys:
                    est[k] += er.get(k) or 0
                for k in oth_keys:
                    oth[k] += ot.get(k) or 0
                for a in r.get("valoresTotalizadosPositivos", []):
                    n = a["nombreAgrupacion"].strip()
                    pos[n] = pos.get(n, 0) + a["votos"]
                log.append({"distrito": did, "seccion": sid,
                            "mesas": er.get("mesasTotalizadas"),
                            "electores": er.get("cantidadElectores"),
                            "votantes": er.get("cantidadVotantes")})
                print(f"  d{did} s{sid}: mesas={er.get('mesasTotalizadas')}")
    agg = {"estadoRecuento": est, "valoresTotalizadosOtros": oth,
           "valoresTotalizadosPositivos": [
               {"nombreAgrupacion": n, "votos": v, "listas": []}
               for n, v in sorted(pos.items(), key=lambda x: -x[1])],
           "note": "aggregated from section-level queries by 03_paso_history.py",
           "sections": log}
    save(agg, f"res_{y}_e{ide}_c{cargo['IdCargo']}_aggregated.json")


def main():
    os.makedirs(OUT, exist_ok=True)
    for y in YEARS:
        menu = get(f"{BASE}/menu?" + urllib.parse.urlencode({"año": y}))
        save(menu, f"menu_{y}.json")
        for el in menu:
            ide = el.get("IdEleccion")
            cargos = [c for c in el.get("Cargos", [])
                      if "PRESIDENTE" in str(c.get("Cargo", "")).upper()]
            for c in cargos:
                cid = c["IdCargo"]
                q = urllib.parse.urlencode({
                    "anioEleccion": y, "tipoRecuento": 1,
                    "tipoEleccion": ide, "categoriaId": cid})
                print(f"{y} eleccion={ide} cargo={cid} ({c.get('Cargo')})")
                res = get(f"{BASE}/resultados/getResultados?{q}")
                save(res, f"res_{y}_e{ide}_c{cid}.json")
                if is_empty(res):
                    print(f"  national query empty -> aggregating sections")
                    aggregate_sections(y, ide, c)

    # independent cross-check for 2019 PASO: PolAr / electorArg compilation
    u = ("https://raw.githubusercontent.com/electorArg/PolAr_Data/master/"
         "data/arg_presi_paso2019.csv")
    try:
        req = urllib.request.Request(u, headers=HDR)
        with urllib.request.urlopen(req, timeout=120) as r:
            data = r.read()
        with open(os.path.join(OUT, "polar_arg_presi_paso2019.csv"), "wb") as f:
            f.write(data)
    except Exception as e:  # noqa: BLE001
        print("PolAr cross-check download failed:", e)


if __name__ == "__main__":
    main()
