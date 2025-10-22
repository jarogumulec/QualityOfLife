# ==== Packages ====
library(data.table)
library(dplyr)

# ==== Načtení všech tří tabulek ====
wb  <- fread("qualityoflife_wide_WB.csv")
iep <- fread("qualityoflofe_wide_iep.csv")
imf <- fread("qualityoflife_wide_imf.csv")

# ==== Kontrola hlaviček ====
# Sloupce Country, Year musí existovat
stopifnot(all(c("Country", "Year") %in% names(wb)),
          all(c("Country", "Year") %in% names(iep)),
          all(c("Country", "Year") %in% names(imf)))

# ==== Sloučení ====
merged <- wb %>%
  full_join(iep, by = c("Country", "Year")) %>%
  full_join(imf, by = c("Country", "Year"))

# ==== Seřazení ====
merged <- merged %>%
  arrange(Country, Year)

# ==== Uložení ====
fwrite(merged, "qualityoflife_merged.csv", na = "")

# (volitelné: rychlá kontrola rozsahu)
summary(merged$Year)
length(unique(merged$Country))

