# ==== Packages ====
library(readxl)
library(dplyr)
library(tidyr)

# ==== Paths ====
meta_xlsx    <- "qualityoflife_sourcedesc.xlsx"
worldbank_dir <- "2025_timedata/worldbank"

# ==== 1) Load metadata and countries ====
meta <- read_excel(meta_xlsx, sheet = "2025datadesc")
countries <- read_excel(meta_xlsx, sheet = "countries")

meta_wb <- subset(meta, tolower(src) == "worldbank")
meta_wb$file_path <- file.path(worldbank_dir, meta_wb$file)

# keep only countries flagged for World Bank stats
keep_codes <- countries$`Country Code`[countries$interrest_worldbank == 1]

# ==== 2) Reader for a single World Bank indicator ====
read_wb_file <- function(path, shortname, keep_codes) {
  # Sheet 'Data'; header on row 4 -> skip = 3
  df <- read_excel(path, sheet = "Data", skip = 3)
  
  # Select identifiers + all year columns (YYYY)
  year_cols <- grep("^[0-9]{4}$", names(df), value = TRUE)
  keep_cols <- c("Country Name", "Country Code", year_cols)
  df <- df[, keep_cols]
  
  # Filter to selected countries
  df <- df[df$`Country Code` %in% keep_codes, ]
  
  # Long format
  long <- pivot_longer(
    data = df,
    cols = all_of(year_cols),
    names_to = "Year",
    values_to = "value"
  )
  long$Year  <- as.integer(long$Year)
  long$value <- suppressWarnings(as.numeric(long$value))
  
  # Rename value -> indicator shortname
  names(long)[names(long) == "value"] <- shortname
  
  # Return standardized columns
  long <- long[, c("Country Name", "Country Code", "Year", shortname)]
  long
}

# ==== 3) Load all indicators and merge ====
qualityoflife_list <- vector("list", nrow(meta_wb))
for (i in seq_len(nrow(meta_wb))) {
  qualityoflife_list[[i]] <- read_wb_file(
    path       = meta_wb$file_path[i],
    shortname  = meta_wb$shortname[i],
    keep_codes = keep_codes
  )
}

# Merge by (Country Name, Country Code, Year)
qualityoflife_long_merged <- Reduce(function(x, y) {
  merge(x, y, by = c("Country Name", "Country Code", "Year"), all = TRUE)
}, qualityoflife_list)

# ==== 4) Final tables ====
# Wide panel: Country Name, Year, indicator columns
qualityoflife_wide <- qualityoflife_long_merged[, c(
  "Country Name", "Year",
  setdiff(names(qualityoflife_long_merged), c("Country Name","Country Code","Year"))
)]
qualityoflife_wide <- qualityoflife_wide[
  order(qualityoflife_wide$`Country Name`, qualityoflife_wide$Year), 
]
colnames(qualityoflife_wide)[1] <- "Country"

# ==== 5) Save (optional) ====
write.csv(qualityoflife_wide, "qualityoflife_wide_WB.csv", row.names = FALSE, na = "")

