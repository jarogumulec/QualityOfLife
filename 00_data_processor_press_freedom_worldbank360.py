# Re-run with correct import for normalization
import pandas as pd
import re
from unicodedata import normalize
from pathlib import Path
from caas_jupyter_tools import display_dataframe_to_user

in_csv = Path("/mnt/data/RWB_PFI_WIDEF.csv")
df_wide = pd.read_csv(in_csv)

# Filter desired metric
metric = "Press Freedom Index Rank"
df_wide = df_wide[df_wide["INDICATOR_LABEL"].astype(str) == metric].copy()

# Canonical country mapping
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

# Years
requested_years = ["2002","2003","2004","2005","2006","2007","2008","2009","2010",
                   "2012","2013","2014","2015","2016","2017","2018","2019","2020",
                   "2021","2022","2023","2024"]
present_year_cols = [y for y in requested_years if y in df_wide.columns]

# Melt to long
id_cols = ["__MatchedCountry__", "__OriginalCountry__"]
long = df_wide.melt(id_vars=id_cols, value_vars=present_year_cols,
                    var_name="Year", value_name="Press Freedom Index Rank")
long["Year"] = pd.to_numeric(long["Year"], errors="coerce").astype("Int64")
long["Press Freedom Index Rank"] = pd.to_numeric(long["Press Freedom Index Rank"], errors="coerce")
long = long.dropna(subset=["Press Freedom Index Rank"]).copy()

subset = long.loc[long["__MatchedCountry__"].notna()].copy()
subset = subset.rename(columns={"__MatchedCountry__": "Country"})
subset = subset[["Country", "Year", "Press Freedom Index Rank"]].sort_values(["Country", "Year"], kind="stable")

out_long_rank = Path("/mnt/data/RWB_PFI_Rank_long_filtered.csv")
subset.to_csv(out_long_rank, index=False)

display_dataframe_to_user("Press Freedom Index Rank (long, filtered to requested countries)", subset.head(30))

(len(subset), subset["Country"].nunique(), str(out_long_rank))
