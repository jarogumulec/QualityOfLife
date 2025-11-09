# 05_indicator_ranking_percentile.R
# Seřazený graf (low→high) pro zadaný indikátor; zvýrazní Czechia + percentil

library(data.table)
library(ggplot2)

# ---- Parametry ----
infile  <- "fixedyear_latest.csv"   # široká tabulka: Country + indikátory
param   <- "House_Price To Income Ratio"            # << název sloupce/indikátoru

# House_Price To Income Ratio "debt_to_GDP" 
cz_name <- "Czechia"                # název státu v datech

# ---- Načtení a příprava ----
dt <- fread(infile)
stopifnot("Country" %in% names(dt))
if (!param %in% names(dt)) stop(sprintf("Sloupec '%s' nebyl nalezen.", param))

df <- dt[, .(Country, value = as.numeric(get(param)))]
df <- df[is.finite(value)]

# Percentil: PERCENTRANK = 100*(rank-1)/(N-1), průměrné pořadí při shodách
N <- nrow(df)
df[, rnk := frank(value, ties.method = "average")]                # 1 = nejnižší
df[, percentil := if (N > 1) 100 * (rnk - 1) / (N - 1) else 100 ] # 0..100

# Řazení low→high a flag pro Czechia
df[, Country := factor(Country, levels = df[order(value), Country])]
df[, is_cz := Country == cz_name]

# ---- Graf (lollipop) ----
p <- ggplot(df[order(value)], aes(x = value, y = Country)) +
  geom_segment(aes(x = min(value, na.rm = TRUE), xend = value, yend = Country),
               color = "grey80", linewidth = 0.6) +
  geom_point(aes(color = is_cz), size = 2.4) +
  scale_color_manual(values = c(`TRUE` = "firebrick", `FALSE` = "grey30"), guide = "none") +
  labs(
    title = sprintf("%s — seřazeno low→high", param),
    x = param, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# popisek pro Czechia s percentilem
if (cz_name %in% df$Country) {
  cz <- df[Country == cz_name]
  p <- p +
    geom_text(
      data = cz,
      aes(label = sprintf("%s: %g  |  percentil: %.1f", cz_name, value, percentil)),
      hjust = -0.1, vjust = 0.5, size = 3.2, color = "firebrick"
    ) +
    expand_limits(x = cz$value * 1.05)
}

print(p)
