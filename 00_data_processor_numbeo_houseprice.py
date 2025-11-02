# Re-run corrected filtering logic (session reset, so reimport everything)
import pandas as pd
import re
from unicodedata import normalize as u_normalize
from pathlib import Path

# Reload Excel
inpath = Path("/mnt/data/houseprice_numbeo.xlsx")
xl = pd.ExcelFile(inpath)
df = xl.parse("List1")

country_col = "Country"

requested_countries = [
"Albania","Austria","Belarus","Belgium","Bosnia and Herzegovina","Bulgaria","Canada","Croatia","Czechia",
"Denmark","Estonia","Finland","France","Georgia","Germany","Greece","Hungary","China","Iceland","Ireland",
"Italy","Japan","Latvia","Lithuania","Moldova","Montenegro","Netherlands","New Zealand","Norway","Poland",
"Portugal","Romania","Russian Federation","Serbia","Slovak Republic","Slovenia","Spain","Sweden","Switzerland",
"Turkiye","Ukraine","United Kingdom","United States"
]

# Keep words like Kingdom, States intact
def simplify_precise(s: str) -> str:
    s = u_normalize("NFKD", str(s))
    s = "".join(ch for ch in s if ord(ch) < 128)
    s = s.lower()
    s = s.replace("&"," and ")
    s = re.sub(r"[^a-z]+"," ", s)
    return re.sub(r"\s+"," ", s).strip()

req_norm = {simplify_precise(k): k for k in requested_countries}

aliases = {
    "czech republic":"Czechia",
    "slovakia":"Slovak Republic",
    "russia":"Russian Federation",
    "russian federation":"Russian Federation",
    "turkey":"Turkiye",
}
for a, canon in aliases.items():
    req_norm[simplify_precise(a)] = canon

def map_to_requested(raw):
    key = simplify_precise(raw)
    return req_norm.get(key, None)

matched = df[country_col].astype(str).map(map_to_requested)
filtered = df.loc[matched.notna()].copy()
filtered["__MatchedCountry__"] = matched.loc[matched.notna()].values
filtered["__OriginalCountry__"] = filtered[country_col]
filtered[country_col] = filtered["__MatchedCountry__"]

# Order same as requested
order = {c:i for i,c in enumerate(requested_countries)}
filtered["__order__"] = filtered[country_col].map(order)
filtered = filtered.sort_values("__order__", kind="stable")

out_csv2 = Path("/mnt/data/filtered_countries_fixed.csv")
filtered.to_csv(out_csv2, index=False)

from caas_jupyter_tools import display_dataframe_to_user
display_dataframe_to_user("Filtered countries (fixed UK vs US)", filtered.head(20))

(len(filtered), filtered["Country"].nunique(), str(out_csv2))
