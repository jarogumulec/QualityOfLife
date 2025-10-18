# Minimal PCA on "latest available per indicator per country" (no imputation)
library(data.table)

infile <- "qualityoflife_wide.csv"
dt <- fread(infile)

id_country <- "Country Name"
id_year    <- "Year"
ind_cols   <- setdiff(names(dt), c(id_country, id_year))

# 1) Order by country, then YEAR ASC so "last non-NA" is simply the last index
setorderv(dt, cols = c(id_country, id_year), order = c(1, 1), na.last = TRUE)

# 2) For each country, take the LAST non-NA value per indicator
latest <- dt[, lapply(.SD, function(v) {
  idx <- which(!is.na(v))
  if (length(idx)) v[tail(idx, 1L)] else NA_real_
}), by = c(id_country), .SDcols = ind_cols]


# tohle nedelam rad, ale musim nahradit prumerem par blbosti. 
#slovensko napriklad nema social .... 
# replace any remaining NAs by column mean (ignoring NA)
for (col in ind_cols) {
  mean_val <- mean(latest[[col]], na.rm = TRUE)
  latest[is.na(get(col)), (col) := mean_val]
}



# 3) Keep only countries with complete rows (no imputation)
mask_complete <- complete.cases(latest[, ..ind_cols])
X <- as.matrix(latest[mask_complete, ..ind_cols])
cnames <- latest[[id_country]][mask_complete]

# 4) PCA (centered & scaled)
p <- prcomp(X, center = TRUE, scale. = TRUE)

# 5) Scores (countries) and loadings (indicators), PC1–PC2
scores <- data.table(`Country Name` = cnames,
                     PC1 = p$x[,1], PC2 = p$x[,2])

loadings <- data.table(indicator = colnames(X),
                       PC1 = p$rotation[,1], PC2 = p$rotation[,2])

# 6) Plots
plot(scores$PC1, scores$PC2,
     xlab = paste0("PC1 (", round(100*summary(p)$importance[2,1],1), "%)"),
     ylab = paste0("PC2 (", round(100*summary(p)$importance[2,2],1), "%)"),
     main = "Countries in PC1–PC2 (latest available per indicator)", pch = 19, cex = 0.6)
 text(scores$PC1, scores$PC2, labels = scores$`Country Name`, cex = 0.6, pos = 3)

plot(loadings$PC1, loadings$PC2,
     xlab = paste0("PC1 (", round(100*summary(p)$importance[2,1],1), "%)"),
     ylab = paste0("PC2 (", round(100*summary(p)$importance[2,2],1), "%)"),
     main = "Loadings in PC1–PC2 (latest snapshot)", pch = 19, cex = 0.6)
text(loadings$PC1, loadings$PC2, labels = loadings$indicator, cex = 0.6, pos = 3)

# 7) Save minimal artifacts for later projection
saveRDS(list(pca = p, variables = colnames(X), center = p$center, scale = p$scale),
        "pca_model_latest.rds")
fwrite(scores,   "pca_scores_latest_pc12.csv")
fwrite(loadings, "pca_loadings_latest_pc12.csv")
