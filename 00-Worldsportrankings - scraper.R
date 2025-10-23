# packages
library(rvest)
library(dplyr)
library(stringr)
library(readr)
library(purrr)

years <- 2014:2025

read_one_year <- function(y) {
  url <- paste0("https://sportsrankings.world/rankings?year=", y)
  # 1) zkusíme standardní html_table (na webu je to skutečně klasická tabulka)
  pg <- read_html(url)
  tb <- pg %>%
    html_element("table") %>%           # pokud by stránka jednou změnila strukturu, může vrátit NULL
    html_table(fill = TRUE)
  
  if (is.null(tb)) stop("Tabulka pro rok ", y, " nebyla nalezena.")
  
  # očekávané sloupce: Rank, Flag, Country, Points
  # necháme si jen to podstatné
  out <- tb %>%
    select(Rank = 1, Country = 3, Points = 4) %>%
    mutate(
      Year   = y,
      Points = readr::parse_number(Points),  # odstraní čárky apod.
      Rank   = suppressWarnings(as.integer(Rank))
    ) %>%
    filter(!is.na(Rank), !is.na(Country), !is.na(Points))
  
  out
}

all_years <- map_dfr(years, ~tryCatch(read_one_year(.x),
                                      error = function(e) {message(e$message); tibble()}))

# normalizace názvů států – volitelné
all_years <- all_years %>%
  mutate(Country = str_squish(Country))

# --- Normalizace a filtrování zemí ---

# --- Normalizace a filtrování zemí + přejmenování sloupců ---

library(stringr)

keep_canonical <- c(
  "Albania","Austria","Belarus","Belgium","Bosnia and Herzegovina","Bulgaria",
  "Canada","Croatia","Czechia","Denmark","Estonia","Finland","France","Georgia",
  "Germany","Greece","Hungary","China","Iceland","Ireland","Italy","Japan","Latvia",
  "Lithuania","Moldova","Montenegro","Netherlands","New Zealand","Norway","Poland",
  "Portugal","Romania","Russian Federation","Serbia","Slovak Republic","Slovenia",
  "Spain","Sweden","Switzerland","Turkiye","Ukraine","United Kingdom","United States"
)

norm_key <- function(x) {
  x |>
    toupper() |>
    iconv(to = "ASCII//TRANSLIT") |>
    str_replace_all("[^A-Z ]", " ") |>
    str_squish()
}

map_norm_to_canon <- c(
  "ALBANIA"="Albania","AUSTRIA"="Austria","BELARUS"="Belarus","BELGIUM"="Belgium",
  "BOSNIA AND HERZEGOVINA"="Bosnia and Herzegovina","BULGARIA"="Bulgaria",
  "CANADA"="Canada","CROATIA"="Croatia","CZECHIA"="Czechia","CZECH REPUBLIC"="Czechia",
  "DENMARK"="Denmark","ESTONIA"="Estonia","FINLAND"="Finland","FRANCE"="France",
  "GEORGIA"="Georgia","GERMANY"="Germany","GREECE"="Greece","HUNGARY"="Hungary",
  "CHINA"="China","ICELAND"="Iceland","IRELAND"="Ireland","ITALY"="Italy",
  "JAPAN"="Japan","LATVIA"="Latvia","LITHUANIA"="Lithuania",
  "MOLDOVA"="Moldova","MOLDOVA REPUBLIC OF"="Moldova","REPUBLIC OF MOLDOVA"="Moldova",
  "MONTENEGRO"="Montenegro","NETHERLANDS"="Netherlands","NEW ZEALAND"="New Zealand",
  "NORWAY"="Norway","POLAND"="Poland","PORTUGAL"="Portugal","ROMANIA"="Romania",
  "RUSSIA"="Russian Federation","RUSSIAN FEDERATION"="Russian Federation",
  "SERBIA"="Serbia","SLOVAKIA"="Slovak Republic","SLOVAK REPUBLIC"="Slovak Republic",
  "SLOVENIA"="Slovenia","SPAIN"="Spain","SWEDEN"="Sweden","SWITZERLAND"="Switzerland",
  "TURKIYE"="Turkiye","TURKEY"="Turkiye",
  "UK"="United Kingdom","UNITED KINGDOM"="United Kingdom",
  "USA"="United States","UNITED STATES"="United States",
  "UNITED STATES OF AMERICA"="United States"
)

all_years <- all_years %>%
  mutate(
    .key = norm_key(Country),
    Country = unname(map_norm_to_canon[.key])
  ) %>%
  filter(!is.na(Country) & Country %in% keep_canonical) %>%
  select(Country, Year, `World Sport Ranking` = Points) %>%
  arrange(Country, Year)

# --- uložení ---
write_csv(all_years, "qualityoflife_wide_world_sports_rankings.csv")
