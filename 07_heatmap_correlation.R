# 04_simple_heatmaps.R
# Simple clustered correlation and data heatmaps using pheatmap

library(data.table)
library(pheatmap)

# ==== Load ====
dt <- fread("fixedyear_latest.csv")
id_country <- "Country"
ind_cols <- setdiff(names(dt), id_country)

X <- as.matrix(dt[, ..ind_cols])
rownames(X) <- dt[[id_country]]

# ==== 1) Correlation heatmap ====
cormat <- cor(X, use = "pairwise.complete.obs", method = "pearson")

# symmetric red–white–blue palette, white at 0
col_fun <- colorRampPalette(c("blue", "white", "red"))(100)

pheatmap(
  cormat,
  color = col_fun,
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  main = "Clustered correlation heatmap (indicators)",
  fontsize_row = 6,
  fontsize_col = 6,
  border_color = NA
)

# ==== 2) Standardized data heatmap ====
X_scaled <- scale(X)
rownames(X_scaled) <- rownames(X)

pheatmap(
  X_scaled,
  color = col_fun,
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  main = "Clustered standardized data (countries × indicators)",
  fontsize_row = 6,
  fontsize_col = 6,
  border_color = NA
)
