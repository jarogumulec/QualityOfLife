# ==== Packages ====
library(data.table)
library(dplyr)
library(readxl)

# ==== Načtení všech tabulek ====
wb    <- fread("qualityoflife_wide_WB.csv")
iep   <- fread("qualityoflofe_wide_iep.csv")
imf   <- fread("qualityoflife_wide_imf.csv")

# manual: ignoruj první sloupec country_ID
manual <- read_excel("qualityoflife_wide_manual.xlsx") |>
  select(-country_ID) |>
  as.data.table()

sports <- fread("qualityoflife_wide_world_sports_rankings.csv")

# --- Nové zdroje ---
# Numbeo: explicitně 3 sloupce
numbeo <- fread(
  "2025_timedata/numbeo/houseprice_numbero_filtered_countries_fixed.csv",
  select = c("Year", "Country", "House_Price To Income Ratio")
)

# World Bank – Press Freedom Index Rank (long, 3 sloupce)
pfi <- fread("2025_timedata/worldbank_360/RWB_PFI_Rank_long_filtered.csv")

# IMF – Social benefits (ponecháme názvy sloupců z CSV)
imf_ges <- fread("2025_timedata/worldbank_360/IMF_GES_GeneralGov_pctGDP_filtered.csv")
# Ověření minimální struktury
stopifnot(all(c("Country", "Year") %in% names(imf_ges)))
# Pokud by soubor měl víc datových sloupců, zapojí se všechny (mimo Country/Year)

# ==== Základní hygiena: typy a hlavičky ====
req <- c("Country","Year")
stopifnot(all(req %in% names(wb)),
          all(req %in% names(iep)),
          all(req %in% names(imf)),
          all(req %in% names(manual)),
          all(req %in% names(sports)),
          all(req %in% names(numbeo)),
          all(req %in% names(pfi)))

# sjednotit typ roku na integer
for (x in list(wb, iep, imf, manual, sports, numbeo, pfi, imf_ges)) x$Year <- as.integer(x$Year)
wb$Year      <- as.integer(wb$Year)
iep$Year     <- as.integer(iep$Year)
imf$Year     <- as.integer(imf$Year)
manual$Year  <- as.integer(manual$Year)
sports$Year  <- as.integer(sports$Year)
numbeo$Year  <- as.integer(numbeo$Year)
pfi$Year     <- as.integer(pfi$Year)
imf_ges$Year <- as.integer(imf_ges$Year)

# ==== Sloučení všech zdrojů ====
merged <- Reduce(function(x, y) full_join(x, y, by = c("Country","Year")),
                 list(wb, iep, imf, manual, sports, numbeo, pfi, imf_ges)) %>%
  arrange(Country, Year)

merged_dt <- as.data.table(merged)
setorder(merged_dt, Country, Year)

# ==== Bez jakéhokoli kopírování hodnot ====
merged_out <- merged_dt

# ==== Volitelná hygiena typů: převést integer metriky na numeric (vyjma Year) ====
metric_cols <- setdiff(names(merged_out), c("Country","Year"))
merged_out[, (metric_cols) := lapply(.SD, function(x) if (is.integer(x)) as.numeric(x) else x),
           .SDcols = metric_cols]

# ==== Uložení ====
fwrite(merged_out, "qualityoflife_merged.csv", na = "")

# # volitelná kontrola
# summary(merged_out$Year)
# length(unique(merged_out$Country))
# str(merged_out)
