# Regenerate with dynamic years (1972–2023) and requested column name
import pandas as pd
import re
from unicodedata import normalize
from pathlib import Path
from caas_jupyter_tools import display_dataframe_to_user

in_csv = Path("/mnt/data/IMF_GFSE_GES_G14_WIDEF.csv")
df_wide = pd.read_csv(in_csv)

# 1) Filters
df_wide = df_wide[df_wide["UNIT_MEASURE_LABEL"].astype(str) == "Percentage of GDP"].copy()
df_wide = df_wide[df_wide["SECTOR_LABEL"].astype(str) == "Sector: General government"].copy()

# 2) Country mapping to requested 43
requested_countries = [
"Albania","Austria","Belarus","Belgium","Bosnia and Herzegovina","Bulgaria","Canada","Croatia","Czechia",
"Denmark","Estonia","Finland","France","Georgia","Germany","Greece","Hungary","China","Iceland","Ireland",
"Italy","Japan","Latvia","Lithuania","Moldova","Montenegro","Netherlands","New Zealand","Norway","Poland",
"Portugal","Romania","Russian Federation","Serbia","Slovak Republic","Slovenia","Spain","Sweden","Switzerland",
"Turkiye","Ukraine","United Kingdom","United States"
]

def simplify_precise(s: str) -> str:
    s = normalize("NFKD", str(s))
    s = "".join(ch for ch in s if ord(ch) < 128)
    s = s.lower()
    s = s.replace("&", " and ")
    s = re.sub(r"[^a-z]+", " ", s)
    return re.sub(r"\s+", " ", s).strip()

req_norm = {simplify_precise(k): k for k in requested_countries}
aliases = {
    "czech republic": "Czechia",
    "slovakia": "Slovak Republic",
    "russia": "Russian Federation",
    "russian federation": "Russian Federation",
    "turkey": "Turkiye",
    "uk": "United Kingdom",
    "united kingdom": "United Kingdom",
    "great britain": "United Kingdom",
    "england": "United Kingdom",
    "usa": "United States",
    "united states": "United States",
    "united states of america": "United States",
}
for a, canon in aliases.items():
    if canon in requested_countries:
        req_norm[simplify_precise(a)] = canon

def map_to_canonical(raw):
    key = simplify_precise(raw)
    return req_norm.get(key, None)

df_wide["__MatchedCountry__"] = df_wide["REF_AREA_LABEL"].astype(str).map(map_to_canonical)
df_wide["__OriginalCountry__"] = df_wide["REF_AREA_LABEL"]

# 3) Dynamic year detection: 4-digit numeric column names between 1972 and 2023 inclusive
year_cols = []
for c in df_wide.columns:
    sc = str(c)
    if re.fullmatch(r"\d{4}", sc):
        y = int(sc)
        if 1972 <= y <= 2023:
            year_cols.append(sc)

year_cols = sorted(year_cols)

# 4) Melt to long and rename value column
id_cols = ["__MatchedCountry__", "__OriginalCountry__"]
long = df_wide.melt(id_vars=id_cols, value_vars=year_cols,
                    var_name="Year", value_name="Social benefits expense %GDP")
long["Year"] = pd.to_numeric(long["Year"], errors="coerce").astype("Int64")
long["Social benefits expense %GDP"] = pd.to_numeric(long["Social benefits expense %GDP"], errors="coerce")

subset = long.loc[long["__MatchedCountry__"].notna()].copy()
subset = subset.rename(columns={"__MatchedCountry__": "Country"})
subset = subset[["Country", "Year", "Social benefits expense %GDP"]].dropna(subset=["Social benefits expense %GDP"])
subset = subset.sort_values(["Country", "Year"], kind="stable")

# 5) Save
out_long = Path("/mnt/data/IMF_GES_GeneralGov_pctGDP_1972_2023_long_filtered.csv")
subset.to_csv(out_long, index=False)

display_dataframe_to_user("Social benefits expense %GDP (1972–2023, long, filtered to requested countries)", subset.head(30))

(len(subset), subset["Country"].nunique(), (year_cols[0], year_cols[-1]) if year_cols else None, str(out_long))
