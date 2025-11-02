# ==== Packages ====
library(data.table)
library(dplyr)
library(readxl)

# ==== Načtení všech tabulek ====
wb    <- fread("qualityoflife_wide_WB.csv")
iep   <- fread("qualityoflofe_wide_iep.csv")
imf   <- fread("qualityoflife_wide_imf.csv")

manual <- read_excel("qualityoflife_wide_manual.xlsx") |>
  select(-country_ID) |>
  as.data.table()
# manual bez prvniho sloupce s id country
sports <- fread("qualityoflife_wide_world_sports_rankings.csv")

# --- Nové zdroje ---
# Numbeo: vezmeme pouze první tři sloupce (explicitně jmény)
numbeo <- fread(
  "2025_timedata/numbeo/houseprice_numbero_filtered_countries_fixed.csv",
  select = c("Year", "Country", "House_Price To Income Ratio")
)

# World Bank – Press Freedom Index Rank (už je long se 3 sloupci)
pfi <- fread("2025_timedata/worldbank_360/RWB_PFI_Rank_long_filtered.csv")

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
for (x in list(wb, iep, imf, manual, sports, numbeo, pfi)) x$Year <- as.integer(x$Year)
wb$Year     <- as.integer(wb$Year)
iep$Year    <- as.integer(iep$Year)
imf$Year    <- as.integer(imf$Year)
manual$Year <- as.integer(manual$Year)
sports$Year <- as.integer(sports$Year)
numbeo$Year <- as.integer(numbeo$Year)
pfi$Year    <- as.integer(pfi$Year)

# ==== Sloučení všech zdrojů ====
merged <- Reduce(function(x, y) full_join(x, y, by = c("Country","Year")),
                 list(wb, iep, imf, manual, sports, numbeo, pfi)) %>%
  arrange(Country, Year)

merged_dt <- as.data.table(merged)
setorder(merged_dt, Country, Year)

# ==== Přenesení JEDNORÁZOVÝCH hodnot (pouze tam, kde je v dané zemi přesně 1 hodnota) od r. 1995 ====
value_cols <- setdiff(names(merged_dt), c("Country","Year"))

part_pre95 <- merged_dt[Year < 1995]
part_95p   <- merged_dt[Year >= 1995]

part_95p[, (value_cols) := lapply(.SD, function(v) {
  nn <- sum(!is.na(v))
  if (nn == 1L) {
    rep(v[which(!is.na(v))[1]], .N)
  } else {
    v
  }
}), by = Country, .SDcols = value_cols]

merged_out <- rbindlist(list(part_pre95, part_95p), use.names = TRUE, fill = TRUE)
setorder(merged_out, Country, Year)

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
