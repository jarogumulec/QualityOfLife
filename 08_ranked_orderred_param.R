# 05_indicator_ranking_from_merged.R
# Ranking (low→high) pro zadaný indikátor z qualityoflife_merged.csv (poslední≤cutoff)

library(data.table)
library(ggplot2)

# ---- Parametry ----
infile      <- "qualityoflife_merged.csv"
id_country  <- "Country"
id_year     <- "Year"
cutoff_year <- 2025
param       <- "House_Price To Income Ratio"  # název sloupce/indikátoru
cz_name     <- "Czechia"

# ---- Načtení ----
dt <- fread(infile)
stopifnot(all(c(id_country, id_year) %in% names(dt)))
if (!param %in% names(dt)) stop(sprintf("Sloupec '%s' nebyl nalezen.", param))
dt[, (param) := as.numeric(get(param))]

# ---- Poslední hodnota ≤ cutoff_year (fallback) pouze pro vybraný indikátor ----
setorderv(dt, c(id_country, id_year), c(1, 1))
df <- dt[,
         {
           yr <- get(id_year); v <- get(param)
           idx_le <- which(!is.na(v) & yr <= cutoff_year)
           val <- if (length(idx_le)) v[tail(idx_le, 1L)] else {
             idx_all <- which(!is.na(v)); if (length(idx_all)) v[tail(idx_all, 1L)] else NA_real_
           }
           .(value = val)
         },
         by = c(id_country)
]
setnames(df, id_country, "Country")

# ---- Drop NA a spočti percentil ----
df <- df[is.finite(value)]
N  <- nrow(df)
df[, rnk := frank(value, ties.method = "average")]                 # 1 = nejnižší
df[, percentil := if (N > 1) 100 * (rnk - 1) / (N - 1) else 100 ]  # 0..100
df[, Country := factor(Country, levels = df[order(value), Country])]
df[, is_cz := as.character(Country) == cz_name]

# ---- Graf (lollipop) ----
p <- ggplot(df[order(value)], aes(x = value, y = Country)) +
  geom_segment(aes(x = min(value, na.rm = TRUE), xend = value, yend = Country),
               color = "grey80", linewidth = 0.6) +
  geom_point(aes(color = is_cz), size = 2.4) +
  scale_color_manual(values = c(`TRUE` = "firebrick", `FALSE` = "grey30"), guide = "none") +
  labs(
    title = sprintf("%s — seřazeno low→high (latest≤%d)", param, cutoff_year),
    x = param, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor    = element_blank())

# popisek pro Czechia s percentilem (pokud je v datech)
if (cz_name %in% levels(df$Country)) {
  cz <- df[as.character(Country) == cz_name]
  p <- p +
    geom_text(
      data = cz,
      aes(label = sprintf("%s: %g  |  percentil: %.1f", cz_name, value, percentil)),
      hjust = -0.1, vjust = 0.5, size = 3.2, color = "firebrick"
    ) +
    expand_limits(x = cz$value * 1.05)
}

print(p)
