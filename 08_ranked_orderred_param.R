# ==========================================================
# 05_indicator_barplot_from_merged.R — FINAL CLEAN VERSION
# ==========================================================

library(data.table)
library(ggplot2)
library(svglite)

# ---- Parametry ----
infile      <- "qualityoflife_merged.csv"
id_country  <- "Country"
id_year     <- "Year"
cutoff_year <- 2025
param       <- "debt_to_GDP" #"House_Price To Income Ratio"     # název indikátoru v datasetu
cz_name     <- "Czechia"                         # originální EN název země

# ---- Překlady ----
tr_countries <- fread("translated/countries_for_translation.csv")
tr_indicators <- fread("translated/indicators_for_translation.csv")

translate_vec <- function(vec, dict){
  m <- merge(data.table(original = vec), dict,
             by = "original", all.x = TRUE, sort = FALSE)
  ifelse(is.na(m$translation), m$original, m$translation)
}

# ---- Načtení dat ----
dt <- fread(infile)
stopifnot(all(c(id_country,id_year) %in% names(dt)))
stopifnot(param %in% names(dt))

dt[, (param) := as.numeric(get(param))]

# ---- Latest ≤ cutoff ----
setorderv(dt, c(id_country,id_year))
df <- dt[, {
  yr <- get(id_year); v <- get(param)
  idx <- which(!is.na(v) & yr <= cutoff_year)
  val <- if (length(idx)) v[tail(idx,1)] else {
    idx2 <- which(!is.na(v))
    if (length(idx2)) v[tail(idx2,1)] else NA_real_
  }
  .(value = val)
}, by = id_country]

setnames(df, id_country, "Country")
df <- df[is.finite(value)]
N <- nrow(df)

# ---- Percentily ----
df[, rnk := frank(value, ties.method = "average")]
df[, percentil := if (N > 1) 100 * (rnk - 1) / (N - 1) else 100]

# ---- Regiony ----
eu_members <- c(
  "Austria","Belgium","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland",
  "France","Germany","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg",
  "Malta","Netherlands","Poland","Portugal","Romania","Slovak Republic","Slovenia","Spain","Sweden"
)

europe_non_eu <- c(
  "Albania","Bosnia and Herzegovina","Belarus","Iceland","Moldova","Montenegro",
  "North Macedonia","Norway","Serbia","Switzerland","Ukraine","United Kingdom"
)

df[, Region := fifelse(Country %in% eu_members, "EU",
                       fifelse(Country %in% europe_non_eu, "Evropa mimo EU",
                               "Mimo Evropu"))]

# ---- Překlady ----
df[, Country_cz := translate_vec(Country, tr_countries)]
param_cz <- translate_vec(param, tr_indicators)

# ---- Seřazení low → high ----
df <- df[order(value)]
df[, Country_cz := factor(Country_cz, levels = Country_cz)]

# ---- Označení ČR ----
df[, is_cz := Country == cz_name]

df[, Highlight := ifelse(is_cz,
                         translate_vec(cz_name, tr_countries),
                         "ostatní země")]

label_cz <- translate_vec(cz_name, tr_countries)   # např. "Česko"

# ==========================================================
# ---------------------- BARPLOT ---------------------------
# ==========================================================

p <- ggplot(
  df,
  aes(x = value,
      y = Country_cz,
      fill  = Region,
      alpha = Highlight)
) +
  geom_col(width = 0.85) +
  scale_fill_manual(
    values = c(
      "EU"             = "#4C72B0",
      "Evropa mimo EU" = "#55A868",
      "Mimo Evropu"    = "#C44E52"
    ),
    name = "Region"
  ) +
  scale_alpha_manual(
    values = c("ostatní země" = 0.60, label_cz = 1.0),
    name   = "Zvýrazněná země"
  ) +
  labs(
    x = param_cz,
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "NONE",
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

# ---- Popisek Česka: jen "12.9 (percentil xx.x)" ----
if (cz_name %in% df$Country) {
  cz_row <- df[Country == cz_name]
  
  p <- p +
    geom_text(
      data = cz_row,
      aes(
        x     = value + 0.25,
        y     = Country_cz,
        label = sprintf("%.1f  (percentil %.1f)",
                        value, percentil)
      ),
      hjust = 0,  # text ven doprava
      vjust = 0.5,
      size  = 3.5,
      color = "black"
    ) +
    expand_limits(x = cz_row$value * 1.15)
}

print(p)

# ---- Uložení SVG (350 × 490 px ekvivalent) ----
ggsave(
  filename = "indikator_barplot.svg",
  plot     = p,
  device   = svglite,
  width    = 3.65,   # palce
  height   = 5.60    # palce
)
