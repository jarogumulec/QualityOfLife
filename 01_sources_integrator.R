# ==== Packages ====
library(data.table)
library(dplyr)

# ==== Načtení všech tří tabulek ====
wb  <- fread("qualityoflife_wide_WB.csv")
iep <- fread("qualityoflofe_wide_iep.csv")
imf <- fread("qualityoflife_wide_imf.csv")

# ==== Kontrola hlaviček ====
stopifnot(all(c("Country", "Year") %in% names(wb)),
          all(c("Country", "Year") %in% names(iep)),
          all(c("Country", "Year") %in% names(imf)))

# ==== Sloučení ====
merged <- wb %>%
  full_join(iep, by = c("Country", "Year")) %>%
  full_join(imf, by = c("Country", "Year")) %>%
  arrange(Country, Year)

# ==== Přenesení jednorázových hodnot (forward fill od 1995 dál) ====
merged_dt <- as.data.table(merged)

# identifikuj všechny sloupce mimo identifikátory
value_cols <- setdiff(names(merged_dt), c("Country", "Year"))

# pro každý stát a indikátor doplň dopředně (na období po 1995)
# jeste to overit, ze to skutecne funguje tak, ze doplnuje jen to, co je presne 1x a ne treba 2x
merged_dt <- merged_dt[
  order(Country, Year)
][
  Year >= 1995
][
  , (value_cols) := lapply(.SD, function(v) {
    # Pokud má daná země pouze jednu nenulovou hodnotu -> replikuj ji do všech let
    if (sum(!is.na(v)) == 1L) rep(v[which(!is.na(v))[1]], .N)
    else {
      # jinak standardní forward fill (přepočítáno po letech)
      # fill by last observation carried forward
      nafill(v, type = "locf")
    }
  }),
  by = Country, .SDcols = value_cols
]

# ==== Uložení ====
fwrite(merged_dt, "qualityoflife_merged.csv", na = "")

# volitelná kontrola
summary(merged_dt$Year)
length(unique(merged_dt$Country))
