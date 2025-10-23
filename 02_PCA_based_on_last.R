# Minimal PCA on "latest available per indicator per country" (no imputation)
library(data.table)
library(ggplot2) #for PCA
library(ggrepel) # for PCA

infile <- "qualityoflife_merged.csv"
dt <- fread(infile)

id_country <- "Country"
id_year    <- "Year"
ind_cols   <- setdiff(names(dt), c(id_country, id_year))
cutoff_year <- 2019  # use any earlier year if missing

# ==== 1) Sort by country + year ascending ====
setorderv(dt, cols = c(id_country, id_year), order = c(1, 1), na.last = TRUE)

# ==== 2) For each country, take the last non-NA value ≤ cutoff_year ====
fixedyear <- dt[get(id_year) <= cutoff_year,
                lapply(.SD, function(v) {
                  idx <- which(!is.na(v))
                  if (length(idx)) v[tail(idx, 1L)] else NA_real_
                }),
                by = c(id_country),
                .SDcols = ind_cols
]


# nahrada prumerem

# tohle nedelam rad, ale musim nahradit prumerem par blbosti. 
#slovensko napriklad nema social .... 
# replace any remaining NAs by column mean (ignoring NA)
for (col in ind_cols) {
  mean_val <- mean(fixedyear[[col]], na.rm = TRUE)
  fixedyear[is.na(get(col)), (col) := mean_val]
}



# 3) Keep only countries with complete rows (no imputation)
mask_complete <- complete.cases(fixedyear[, ..ind_cols])
X <- as.matrix(fixedyear[mask_complete, ..ind_cols])
cnames <- fixedyear[[id_country]][mask_complete]

# 4) PCA (centered & scaled)
p <- prcomp(X, center = TRUE, scale. = TRUE)

# 5) Scores (countries) and loadings (indicators), PC1–PC2
scores <- data.table(`Country Name` = cnames,
                     PC1 = p$x[,1], PC2 = p$x[,2])

loadings <- data.table(indicator = colnames(X),
                       PC1 = p$rotation[,1], PC2 = p$rotation[,2])

# 6) Plots

# #oldschool but text overlapping
# plot(scores$PC1, scores$PC2,
#      xlab = paste0("PC1 (", round(100*summary(p)$importance[2,1],1), "%)"),
#      ylab = paste0("PC2 (", round(100*summary(p)$importance[2,2],1), "%)"),
#      main = "Countries in PC1–PC2 (2025*)", pch = 19, cex = 0.6)
# text(scores$PC1, scores$PC2, labels = scores$`Country Name`, cex = 0.6, pos = 3)
# 
# plot(loadings$PC1, loadings$PC2,
#      xlab = paste0("PC1 (", round(100*summary(p)$importance[2,1],1), "%)"),
#      ylab = paste0("PC2 (", round(100*summary(p)$importance[2,2],1), "%)"),
#      main = "Loadings in PC1–PC2 (2025*)", pch = 19, cex = 0.6)
# text(loadings$PC1, loadings$PC2, labels = loadings$indicator, cex = 0.6, pos = 3)

#alternatively in ggplot
ggplot(scores, aes(x = PC1, y = PC2, label = `Country Name`)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.4) +
  geom_point(size = 1.6, color = "gray25") +
  geom_text_repel(
    size = 3,
    max.overlaps = Inf,
    box.padding = 0.5,
    point.padding = 0.3
  ) +
  labs(
    x = paste0("PC1 (", round(100 * summary(p)$importance[2,1], 1), "%)"),
    y = paste0("PC2 (", round(100 * summary(p)$importance[2,2], 1), "%)"),
    title = paste0("Countries in PC1–PC2 (", cutoff_year, ")")   # <── dynamic year
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),     # remove gray grid
    axis.line = element_blank(),      # remove ggplot’s own axis lines
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9)
  )

#loadings plotting

ggplot(loadings, aes(x = PC1, y = PC2, label = indicator)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.4) +
  geom_point(size = 1.8, color = "gray25") +
  geom_text_repel(
    size = 3,
    max.overlaps = Inf,
    box.padding = 0.5,
    point.padding = 0.3
  ) +
  labs(
    x = paste0("PC1 (", round(100 * summary(p)$importance[2,1], 1), "%)"),
    y = paste0("PC2 (", round(100 * summary(p)$importance[2,2], 1), "%)"),
    title = paste0("Characteristics in PC1–PC2 (", cutoff_year, ")")   # <── dynamic year
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),     # removes background grid
    axis.line = element_blank(),      # remove default axis lines
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9)
  )



# 7) Save minimal artifacts for later projection

saveRDS(
  list(
    pca     = p,
    variables = colnames(X),
    center  = p$center,
    scale   = p$scale
  ),
  file = sprintf("pca_model_%d.rds", cutoff_year)
)

fwrite(scores,   sprintf("pca_scores_%d_pc12.csv",   cutoff_year))
fwrite(loadings, sprintf("pca_loadings_%d_pc12.csv", cutoff_year))


