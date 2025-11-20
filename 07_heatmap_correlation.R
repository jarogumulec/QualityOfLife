# 04_heatmaps_from_merged.R
# Clustered correlation + data heatmap z qualityoflife_merged.csv (poslední rok per země×indikátor)

library(data.table)
library(pheatmap)

# ==== Parametry ====
infile      <- "qualityoflife_merged.csv"
id_country  <- "Country"
id_year     <- "Year"
cutoff_year <- 2025   # poslední akceptovaný rok (vezme ≤ cutoff, jinak fallback na úplně poslední)

# ==== Načtení ====
dt <- fread(infile)
stopifnot(all(c(id_country, id_year) %in% names(dt)))
# přetypování numerických sloupců
ind_cols <- setdiff(names(dt), c(id_country, id_year))
dt[, (ind_cols) := lapply(.SD, function(z) suppressWarnings(as.numeric(z))), .SDcols = ind_cols]

# ==== Poslední hodnota ≤ cutoff_year (fallback na úplně poslední) pro KAŽDÝ indikátor ====
setorderv(dt, c(id_country, id_year), c(1, 1))
fixedyear <- dt[,
                {
                  yr <- get(id_year)
                  out <- lapply(.SD, function(v) {
                    idx_le  <- which(!is.na(v) & yr <= cutoff_year)
                    if (length(idx_le)) v[tail(idx_le, 1L)]
                    else {
                      idx_all <- which(!is.na(v))
                      if (length(idx_all)) v[tail(idx_all, 1L)] else NA_real_
                    }
                  })
                  as.data.table(out)
                },
                by = c(id_country), .SDcols = ind_cols
]

# ==== Matice pro heatmapy (země × indikátory) ====
X <- as.matrix(fixedyear[, ..ind_cols])
rownames(X) <- fixedyear[[id_country]]


# =======================================================
# === Překlad názvů zemí a indikátorů pro heatmapy =======
# =======================================================

tr_countries_file  <- "translated/countries_for_translation.csv"
tr_indicators_file <- "translated/indicators_for_translation.csv"

tr_countries  <- fread(tr_countries_file)     # columns: original, translation
tr_indicators <- fread(tr_indicators_file)    # columns: original, translation

stopifnot(all(c("original","translation") %in% names(tr_countries)))
stopifnot(all(c("original","translation") %in% names(tr_indicators)))

# ---- univerzální překladová funkce ----
translate_vec <- function(vec, dict) {
  m <- merge(
    data.table(original = vec),
    dict,
    by = "original",
    all.x = TRUE,
    sort = FALSE
  )
  ifelse(is.na(m$translation), m$original, m$translation)
}

# ---- aplikace překladu na ROWs (země) ----
translated_rows <- translate_vec(rownames(X), tr_countries)
rownames(X) <- translated_rows

# ---- aplikace překladu na COLs (indikátory) ----
translated_cols <- translate_vec(colnames(X), tr_indicators)
colnames(X) <- translated_cols

# (pozn.: pheatmap pracuje se jmény řádků/sloupců právě z rownames() a colnames())
# =======================================================




# ---------------- (1) Korelační heatmapa indikátorů ----------------
# párová korelace dovolí NA
cormat <- cor(X, use = "pairwise.complete.obs", method = "pearson")

# plně saturovaná červeno-bílá-modrá, bílá = 0, rozsah [-1,1]
col_corr <- colorRampPalette(c("blue","white","red"))(100)
br_corr  <- seq(-1, 1, length.out = 101)

pheatmap(
  cormat,
  color = col_corr, breaks = br_corr,
  cluster_rows = TRUE, cluster_cols = TRUE,
  main = "Clustered correlation heatmap (indicators, latest≤cutoff)",
  fontsize_row = 6, fontsize_col = 6,
  border_color = NA
)

# ---------------- (2) Datová heatmapa (země × standardizované indikátory) ----------------
# z-score per sloupec (ignoruje NA), NA ponecháme
X_scaled <- X



col_means <- apply(X_scaled, 2, function(v) mean(v, na.rm = TRUE))
col_sds   <- apply(X_scaled, 2, function(v) sd(v,   na.rm = TRUE))
for (j in seq_along(ind_cols)) {
  v <- X_scaled[, j]
  m <- col_means[j]; s <- col_sds[j]
  if (is.finite(s) && s > 0) X_scaled[, j] <- (v - m) / s else X_scaled[, j] <- NA_real_
}

# symetrické zlomy kolem 0, bílá = 0
zmax   <- max(abs(X_scaled), na.rm = TRUE); if (!is.finite(zmax)) zmax <- 1
col_z  <- colorRampPalette(c("blue","white","red"))(100)
br_z   <- seq(-zmax/2.5, zmax/2.5, length.out = 101)

# --- předvýpočet distančních matic s párovou korelací ---
dist_rows <- as.dist(1 - cor(t(X_scaled), use = "pairwise.complete.obs"))
dist_cols <- as.dist(1 - cor(X_scaled,    use = "pairwise.complete.obs"))

pheatmap(
  X_scaled,
  color = col_z, breaks = br_z,
  na_col = "grey90",
  clustering_distance_rows = dist_rows,
  clustering_distance_cols = dist_cols,
  cluster_rows = TRUE, cluster_cols = TRUE,
#  main = "Clustered standardized data (countries × indicators, latest≤cutoff)",
  fontsize_row = 6, fontsize_col = 6,
  border_color = NA
)




# --- Jen dendrogramy (bez heatmapy) -------------------------------------------
# při volání heatmapy si uložte objekt:
ph <- pheatmap(
  X_scaled,
  color = col_z, breaks = br_z,
  na_col = "grey90",
  clustering_distance_rows = dist_rows,
  clustering_distance_cols = dist_cols,
  clustering_method = "complete",   # stejné jako v pheatmap (default je "complete")
  cluster_rows = TRUE, cluster_cols = TRUE,
  main = "Clustered standardized data (countries × indicators, latest≤cutoff)",
  fontsize_row = 6, fontsize_col = 6,
  border_color = NA,
  silent = TRUE                     # potlačí okamžité vykreslení objektu
)

# --- jen dendrogramy přesně z pheatmap ---
op <- par(mfrow = c(1,2), mar = c(12,2,3,2), cex = 0.6)
plot(as.dendrogram(ph$tree_row), main = "Dendrogram států", ylab = "1 - r", xlab = "")
plot(as.dendrogram(ph$tree_col), main = "Dendrogram indikátorů", ylab = "1 - r", xlab = "")
par(op)

# pokud chcete i heatmapu, vytáhněte ji zvlášť:
# grid::grid.newpage(); grid::grid.draw(ph$gtable)
