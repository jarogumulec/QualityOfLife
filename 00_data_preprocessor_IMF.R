library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

imf_path <- "2025_timedata/IMF/Debt_GDP_imf-dm-export-20251018.xls"

target_countries <- c(
  "Albania","Austria","Belgium","Bulgaria","Bosnia and Herzegovina","Belarus",
  "Canada","Switzerland","China","Czechia","Germany","Denmark","Spain","Estonia",
  "Finland","France","United Kingdom","Georgia","Greece","Croatia","Hungary",
  "Ireland","Iceland","Italy","Japan","Lithuania", "Latvia","Moldova",
  "Montenegro","Netherlands","Norway","New Zealand","Poland","Romania",
  "Russian Federation","Serbia","Slovak Republic","Slovenia","Sweden","Turkiye",
  "Ukraine","United States"
)

name_map <- c(
  "Czech Republic"            = "Czechia",
  "United States of America"  = "United States",
  "Russia"                    = "Russian Federation",
  "Slovakia"                  = "Slovak Republic",
  "Türkiye"                   = "Turkiye",
  "Turkey"                    = "Turkiye",
  "Türkiye, Republic of"     = "Turkiye",
  "Bosnia & Herzegovina"      = "Bosnia and Herzegovina",
  "China, People's Republic of" = "China"
)

norm_country <- function(x) {
  y <- str_squish(as.character(x))
  dplyr::recode(y, !!!name_map, .default = y)
}

# --- 1) Načti Excel; 'no data' rovnou jako NA ---
imf_raw <- read_excel(imf_path, sheet = 1, col_names = TRUE, na = "no data")

# --- 2) První sloupec je jméno země, přejmenuj na Country a odfiltruj prázdný řádek 2 ---
imf_raw <- imf_raw %>%
  rename(Country = 1) %>%
  filter(!is.na(Country) & Country != "")

# --- 3) Najdi sloupce let (YYYY) ---
year_cols <- grep("^[0-9]{4}$", names(imf_raw), value = TRUE)

# --- 4) Wide -> Long; omez na <= 2023; normalizuj názvy a filtruj jen požadované země ---
imf_debt_long <- imf_raw %>%
  pivot_longer(cols = all_of(year_cols), names_to = "Year", values_to = "debt_to_GDP") %>%
  mutate(
    Year        = as.integer(Year),
    Country     = norm_country(Country),
    debt_to_GDP = suppressWarnings(as.numeric(debt_to_GDP))
  ) %>%
  filter(Year <= 2024, Country %in% target_countries) %>%
  arrange(Country, Year)

# --- 5) Ulož ---
write.csv(imf_debt_long, "qualityoflife_wide_imf.csv", row.names = FALSE, na = "")
