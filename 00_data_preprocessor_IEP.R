# ==== Packages ====
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

# ==== Cesty ====
meta_xlsx <- "qualityoflife_country_compare_2021.xlsx"
iep_dir   <- "2025_timedata/IEP"

# ==== Metadata: shortname -> file ====
meta_iep <- read_excel(meta_xlsx, sheet = "2025datadesc") %>%
  filter(tolower(src) == "iep") %>%
  mutate(file_path = file.path(iep_dir, file)) %>%
  select(shortname, file_path)

path_of <- function(short) meta_iep$file_path[match(short, meta_iep$shortname)]

# ==== Readers (minimální, bez mapování na ISO) ====

# 1) GPI — "Overall Scores", header řádek 8 -> skip = 7; sloupce YYYY_score
read_gpi <- function(path) {
  df <- read_excel(path, sheet = "Overall Scores", skip = 7)
  sc <- grep("_score$", names(df), value = TRUE)
  df %>%
    transmute(country = .data[["country"]], !!!df[sc]) %>%
    pivot_longer(cols = -country, names_to = "k", values_to = "val") %>%
    mutate(Country = country,
           Year    = as.integer(str_extract(k, "\\d{4}")),
           value   = suppressWarnings(as.numeric(val)),
           Indicator = "Global Peace Index") %>%
    select(Country, Year, Indicator, value)
}

# 2) GTI — "Overall Scores", header řádek 7 -> skip = 6; sloupce "YYYY score"
read_gti <- function(path) {
  df <- read_excel(path, sheet = "Overall Scores", skip = 6)
  names(df) <- str_replace(names(df), " score$", "_score")
  sc <- grep("_score$", names(df), value = TRUE)
  df %>%
    transmute(country = .data[["country"]], !!!df[sc]) %>%
    pivot_longer(cols = -country, names_to = "k", values_to = "val") %>%
    mutate(Country = country,
           Year    = as.integer(str_extract(k, "\\d{4}")),
           value   = suppressWarnings(as.numeric(val)),
           Indicator = "Global Terrorism Index") %>%
    select(Country, Year, Indicator, value)
}

# 3) PPI — listy = roky (2009–2022); header řádek 5 -> skip = 4
read_ppi <- function(path) {
  sheets <- excel_sheets(path)
  yrs <- sheets[grepl("^\\d{4}$", sheets)]
  if (!length(yrs)) return(tibble(Country=character(), Year=integer(), Indicator=character(), value=numeric()))
  bind_rows(lapply(yrs, function(s) {
    df <- read_excel(path, sheet = s, skip = 4, .name_repair = "unique")
    names(df) <- str_squish(names(df))
    # Country = první sloupec začínající "Country"
    cand_country <- names(df)[grepl("^Country", names(df))]
    if (!length(cand_country)) return(NULL)
    country_col <- cand_country[1]
    # Score = regex; fallback = první numerický sloupec
    score_col <- names(df)[grepl("PPI.*Overall.*Score|Overall.*PPI.*Score|PPI.*Overall",
                                 names(df), ignore.case = TRUE)]
    if (!length(score_col)) {
      num_cols <- names(df)[sapply(df, is.numeric)]
      if (!length(num_cols)) return(NULL)
      score_col <- num_cols[1]
    }
    tibble(
      Country   = as.character(df[[country_col]]),
      Year      = as.integer(s),
      Indicator = "Positive Peace Index",
      value     = suppressWarnings(as.numeric(df[[score_col[1]]]))
    ) %>% filter(!is.na(Country), Country != "")
  }))
}

# 4) ETR 2024 — list "Data"; header řádek 5 -> skip = 4; "Overall Score"
read_etr <- function(path, etr_year = 2024L) {
  if (!("Data" %in% excel_sheets(path))) return(tibble(Country=character(), Year=integer(), Indicator=character(), value=numeric()))
  df <- read_excel(path, sheet = "Data", skip = 4, .name_repair = "minimal")
  names(df) <- str_squish(names(df))
  if (!("Country" %in% names(df))) return(tibble(Country=character(), Year=integer(), Indicator=character(), value=numeric()))
  metric <- if ("Overall Score" %in% names(df)) "Overall Score" else {
    cand <- setdiff(names(df)[grepl("overall|index|score", names(df), ignore.case = TRUE)], "Country")
    if (!length(cand)) return(tibble(Country=character(), Year=integer(), Indicator=character(), value=numeric()))
    cand[1]
  }
  tibble(
    Country   = as.character(df$Country),
    Year      = etr_year,
    Indicator = "Ecological Threat Report",
    value     = suppressWarnings(as.numeric(df[[metric]]))
  ) %>% filter(!is.na(Country), Country != "")
}

# ==== Načtení dat ====
gpi <- read_gpi(path_of("Global Peace Index"))
gti <- read_gti(path_of("Global Terrorism Index"))
ppi <- read_ppi(path_of("Positive Peace Index"))
etr <- read_etr(path_of("Ecological Threat Report"))

# ==== Normalizace názvů + výběr jen cílových zemí ====
target_countries <- c(
  "Albania","Austria","Belgium","Bulgaria","Bosnia and Herzegovina","Belarus",
  "Canada","Switzerland","China","Czechia","Germany","Denmark","Spain","Estonia",
  "Finland","France","United Kingdom","Georgia","Greece","Croatia","Hungary",
  "Ireland","Iceland","Italy","Japan","Lithuania","Latvia","Moldova",
  "Montenegro","Netherlands","Norway","New Zealand","Poland","Romania",
  "Russian Federation","Serbia","Slovak Republic","Slovenia","Sweden","Turkiye",
  "Ukraine","United States" 
)
# "Luxembourg"

name_map <- c(
  "Bosnia & Herzegovina"     = "Bosnia and Herzegovina",
  "United States of America" = "United States",
  "Russia"                   = "Russian Federation",
  "Slovakia"                 = "Slovak Republic",
  "Türkiye"                  = "Turkiye",
  "Turkey"                   = "Turkiye"
)

norm_country <- function(x) {
  y <- str_squish(as.character(x))
  dplyr::recode(y, !!!name_map, .default = y)
}

fix_iepcountries <- function(df) {
  df %>%
    mutate(Country = norm_country(Country),
           value   = suppressWarnings(as.numeric(value))) %>%
    filter(Country %in% target_countries)
}

gpi <- fix_iepcountries(gpi)
gti <- fix_iepcountries(gti)
ppi <- fix_iepcountries(ppi)
etr <- fix_iepcountries(etr)

# ==== Sjednocení, deduplikace, pivot ====
iep_long <- bind_rows(gpi, gti, ppi, etr) %>%
  group_by(Country, Year, Indicator) %>%
  summarise(
    value = {x <- value[!is.na(value)]; if (length(x)) x[1] else NA_real_},
    .groups = "drop"
  ) %>%
  arrange(Country, Year, Indicator)

iep_wide <- iep_long %>%
  pivot_wider(names_from = Indicator, values_from = value) %>%
  arrange(Country, Year)

# ==== Export ====
#write.csv(iep_long, "iep_long.csv", row.names = FALSE, na = "")
write.csv(iep_wide, "qualityoflofe_wide_iep.csv", row.names = FALSE, na = "")

