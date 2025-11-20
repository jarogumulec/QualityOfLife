library(data.table)

# ==== vstupní dataset ====
infile <- "qualityoflife_merged.csv"

dt <- fread(infile)

# Kontrola nutných sloupců
stopifnot("Country" %in% names(dt))
stopifnot("Year" %in% names(dt))

# ================================
# 1) Překladová tabulka: ZEMĚ
# ================================
countries <- unique(dt$Country)
countries <- sort(countries)

countries_dt <- data.table(
  original = countries,
  translation = ""     # sem doplníš ručně český překlad
)

fwrite(countries_dt, "countries_for_translation.csv")

# ================================
# 2) Překladová tabulka: INDIKÁTORY
# ================================
indicator_cols <- setdiff(names(dt), c("Country", "Year"))
indicator_cols <- sort(indicator_cols)

indicators_dt <- data.table(
  original = indicator_cols,
  translation = ""     # sem doplníš překlady
)

fwrite(indicators_dt, "indicators_for_translation.csv")

cat("Vytvořeno:\n - countries_for_translation.csv\n - indicators_for_translation.csv\n")
