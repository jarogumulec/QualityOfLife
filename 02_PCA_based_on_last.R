# ==== Packages ====
library(data.table)
library(ggplot2)
library(ggrepel)

# ==== Parametry ====
infile <- "qualityoflife_merged.csv"
cutoff_year <- 2025

# (a) země k vyloučení (ponechte character(0) pro žádný filtr)
exclude_countries <- c("China", "Japan", "Turkiye") #, "China"
# exclude_countries <- character(0) # kdyz chci vyloucit

# (b) indikátory k vyloučení:
#    - přesný název sloupce (exact match)
exclude_indicators_exact <- character(0)
#    - substring/regex (case-insensitive), např. "Protected area"
exclude_indicators_like  <- c("Relig_Buddhists", "GINI Index", "Forrest area", "Socal contributions")
# exclude_indicators_like  <- character(0)

# ==== Načtení ====
dt <- fread(infile)

# ---- Filtrace (a): země ----
if (length(exclude_countries) > 0) {
  dt <- dt[!(Country %in% exclude_countries)]
}

# ---- Filtrace (b): indikátory (sloupce) ----
# chránit identifikátory
id_country <- "Country"
id_year    <- "Year"

all_cols <- names(dt)
id_cols  <- c(id_country, id_year)

# přesná jména k dropu
drop_exact <- setdiff(intersect(all_cols, exclude_indicators_exact), id_cols)

# substring/regex k dropu (case-insensitive)
drop_like <- character(0)
if (length(exclude_indicators_like) > 0) {
  like_matches <- unique(unlist(
    lapply(exclude_indicators_like, function(pat)
      grep(pat, all_cols, ignore.case = TRUE, value = TRUE))
  ))
  drop_like <- setdiff(like_matches, id_cols)
}

drop_cols <- unique(c(drop_exact, drop_like))
if (length(drop_cols) > 0) {
  dt[, (drop_cols) := NULL]
}

# ---- Numerické sloupce na numeric ----
numcols <- setdiff(names(dt), c(id_country, id_year))
dt[, (numcols) := lapply(.SD, as.numeric), .SDcols = numcols]

# ==== 1) Sort by country + year ascending ====
setorderv(dt, cols = c(id_country, id_year), order = c(1, 1), na.last = TRUE)

# ==== 2) Latest ≤ cutoff_year; fallback na poslední dostupnou hodnotu ====
ind_cols <- setdiff(names(dt), c(id_country, id_year))

fixedyear <- dt[,
                {
                  yr <- get(id_year)
                  out <- lapply(.SD, function(v) {
                    idx_le <- which(!is.na(v) & yr <= cutoff_year)
                    if (length(idx_le)) {
                      v[tail(idx_le, 1L)]
                    } else {
                      idx_all <- which(!is.na(v))
                      if (length(idx_all)) v[tail(idx_all, 1L)] else NA_real_
                    }
                  })
                  as.data.table(out)
                },
                by = c(id_country),
                .SDcols = ind_cols
]

# ==== 3) (Volitelně) imputace průměrem pro zbývající NA ====
for (col in ind_cols) {
  mean_val <- mean(fixedyear[[col]], na.rm = TRUE)
  if (is.finite(mean_val)) {
    fixedyear[is.na(get(col)), (col) := mean_val]
  }
}

# ==== 4) PCA (centered & scaled) ====
mask_complete <- complete.cases(fixedyear[, ..ind_cols])
X <- as.matrix(fixedyear[mask_complete, ..ind_cols])
cnames <- fixedyear[[id_country]][mask_complete]

p <- prcomp(X, center = TRUE, scale. = TRUE)

# ==== 5) Scores & loadings ====
scores <- data.table(`Country Name` = cnames,
                     PC1 = p$x[,1], PC2 = p$x[,2])

loadings <- data.table(indicator = colnames(X),
                       PC1 = p$rotation[,1], PC2 = p$rotation[,2])

# ==== 6) Plots ====
ggplot(scores, aes(x = PC1, y = PC2, label = `Country Name`)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.4) +
  geom_point(size = 1.6, color = "gray25") +
  geom_text_repel(size = 3, max.overlaps = Inf, box.padding = 0.5, point.padding = 0.3) +
  labs(
    x = paste0("PC1 (", round(100 * summary(p)$importance[2,1], 1), "%)"),
    y = paste0("PC2 (", round(100 * summary(p)$importance[2,2], 1), "%)"),
    title = paste0("Countries in PC1–PC2 (", cutoff_year, ")")
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_blank(),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9)
  )

ggplot(loadings, aes(x = PC1, y = PC2, label = indicator)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.4) +
  geom_point(size = 1.8, color = "gray25") +
  geom_text_repel(size = 3, max.overlaps = Inf, box.padding = 0.5, point.padding = 0.3) +
  labs(
    x = paste0("PC1 (", round(100 * summary(p)$importance[2,1], 1), "%)"),
    y = paste0("PC2 (", round(100 * summary(p)$importance[2,2], 1), "%)"),
    title = paste0("Characteristics in PC1–PC2 (", cutoff_year, ")")
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_blank(),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9)
  )

# ==== 7) Export artefaktů ====
saveRDS(
  list(
    pca       = p,
    variables = colnames(X),
    center    = p$center,
    scale     = p$scale
  ),
  file = sprintf("PCAmodel/pca_model_%d.rds", cutoff_year)
)

fwrite(scores,   sprintf("PCAmodel/pca_scores_%d_pc12.csv",   cutoff_year))
fwrite(loadings, sprintf("PCAmodel/pca_loadings_%d_pc12.csv", cutoff_year))
