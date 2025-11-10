# ==== Packages ====
library(data.table)
library(dplyr)
library(ggplot2)
library(ggrepel)

set.seed(1234)

# ==== Parametry ====
infile <- "qualityoflife_merged.csv"
outdir <- "PCAmodel_opt_simple"
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

cutoff_year <- 2025
exclude_countries <- c("China", "Japan", "Turkiye")

# Volitelné slovní filtry indikátorů (ponechte character(0) pokud nechcete nic mazat)
exclude_indicators_exact <- character(0)
exclude_indicators_like  <- character(0)

# Prahy
max_na_ratio     <- 1.0    # max. podíl NA v indikátoru na "latest" tabulce
corr_threshold   <- 0.999   # tvrdý filtr: pokud |r| > threshold, jednu proměnnou odhoď
min_features     <- 7
max_features     <- 55

# ==== Pomocné funkce ====

# Poslední nenulová hodnota ≤ cutoff_year, jinak poslední dostupná vůbec
latest_per_country <- function(dt, id_country="Country", id_year="Year",
                               cutoff_year=2025, ind_cols=NULL) {
  if (is.null(ind_cols)) ind_cols <- setdiff(names(dt), c(id_country, id_year))
  setorderv(dt, c(id_country, id_year))
  res <- dt[, {
    yr <- get(id_year)
    out <- lapply(.SD, function(v) {
      idx_le <- which(!is.na(v) & yr <= cutoff_year)
      if (length(idx_le)) v[tail(idx_le, 1L)]
      else {
        idx_all <- which(!is.na(v))
        if (length(idx_all)) v[tail(idx_all, 1L)] else NA_real_
      }
    })
    as.data.table(out)
  }, by = id_country, .SDcols = ind_cols]
  setnames(res, id_country, "Country")
  res
}

# Jednoduchý korelační filtr: při |r|>threshold odhoď tu s menší sd
cor_filter_simple <- function(DT, cols, thr=0.97) {
  if (length(cols) < 2L) return(cols)
  X <- as.matrix(DT[, ..cols])
  S <- suppressWarnings(cor(X, use = "pairwise.complete.obs"))
  keep <- cols
  repeat {
    if (length(keep) < 2L) break
    Ssub <- S[keep, keep, drop = FALSE]
    Sab <- abs(Ssub); diag(Sab) <- 0
    mx <- max(Sab, na.rm = TRUE)
    if (!is.finite(mx) || mx <= thr) break
    idx <- which(Sab == mx, arr.ind = TRUE)[1, ]
    a <- keep[idx[1]]; b <- keep[idx[2]]
    sda <- sd(X[, a], na.rm = TRUE); sdb <- sd(X[, b], na.rm = TRUE)
    drop <- ifelse(sda <= sdb, a, b)
    keep <- setdiff(keep, drop)
  }
  keep
}

# Greedy výběr: maximalizace podílu variance PC1+PC2 (po standardizaci)
greedy_select <- function(DT_scaled, pool, k_min=8, k_max=20) {
  selected <- character(0)
  best_obj <- -Inf
  while (length(selected) < k_max && length(setdiff(pool, selected)) > 0) {
    rem <- setdiff(pool, selected)
    # skóre kandidátů po jednom přírůstku
    cand <- lapply(rem, function(f) {
      cols <- c(selected, f)
      X <- as.matrix(DT_scaled[, ..cols])  # už je škálováno
      if (ncol(X) < 2L || any(!is.finite(X))) return(list(f=f, obj=-Inf))
      p <- tryCatch(prcomp(X, center = FALSE, scale. = FALSE), error = function(e) NULL)
      if (is.null(p)) return(list(f=f, obj=-Inf))
      imp <- summary(p)$importance
      obj <- sum(imp[2, 1:min(2, ncol(X))], na.rm = TRUE)
      list(f=f, obj=obj)
    })
    cd <- rbindlist(cand)
    cd <- cd[order(-obj)]
    best <- cd[1]
    if (is.infinite(best$obj) || best$obj <= best_obj && length(selected) >= k_min) break
    selected <- c(selected, best$f)
    best_obj <- max(best_obj, best$obj)
  }
  # pokud méně než k_min, doplň prvních pár z poolu
  if (length(selected) < k_min) {
    selected <- unique(c(selected, head(setdiff(pool, selected), k_min - length(selected))))
  }
  selected
}

# ==== Načtení ====
dt <- fread(infile)

# ---- Filtrace zemí ----
if (length(exclude_countries) > 0) {
  dt <- dt[!(Country %in% exclude_countries)]
}

# ---- Filtrace indikátorů (volitelně) ----
id_country <- "Country"; id_year <- "Year"
all_cols <- names(dt); id_cols <- c(id_country, id_year)

drop_exact <- setdiff(intersect(all_cols, exclude_indicators_exact), id_cols)
drop_like <- character(0)
if (length(exclude_indicators_like) > 0) {
  like_matches <- unique(unlist(
    lapply(exclude_indicators_like, function(pat)
      grep(pat, all_cols, ignore.case = TRUE, value = TRUE))
  ))
  drop_like <- setdiff(like_matches, id_cols)
}
drop_cols <- unique(c(drop_exact, drop_like))
if (length(drop_cols) > 0) dt[, (drop_cols) := NULL]

# ---- Numerické sloupce na numeric ----
numcols <- setdiff(names(dt), c(id_country, id_year))
dt[, (numcols) := lapply(.SD, as.numeric), .SDcols = numcols]

# ==== Latest per country (s fallback) ====
ind_all <- setdiff(names(dt), c(id_country, id_year))
latest_raw <- latest_per_country(dt, id_country, id_year, cutoff_year, ind_cols = ind_all)

# ==== NA filtr na "latest" + imputace ====
na_ratio <- sapply(ind_all, function(cn) mean(is.na(latest_raw[[cn]])))
ind_pool <- names(na_ratio)[na_ratio <= max_na_ratio]
if (length(ind_pool) == 0L) stop("Po NA filtru nezbyl žádný indikátor. Zvyšte max_na_ratio.")

latest <- copy(latest_raw[, c("Country", ind_pool), with = FALSE])
for (col in setdiff(names(latest), "Country")) {
  m <- mean(latest[[col]], na.rm = TRUE)
  if (is.finite(m)) {
    idx_na <- which(is.na(latest[[col]]))
    if (length(idx_na)) set(latest, i = idx_na, j = col, value = m)
  }
}

# ==== Drop konstantních sloupců ====
var_ok <- sapply(setdiff(names(latest), "Country"), function(cn) sd(latest[[cn]], na.rm = TRUE) > 0)
ind_pool <- setdiff(names(latest), "Country")[var_ok]

# ==== Korelační redundance ====
if (length(ind_pool) >= 2L) {
  ind_pool <- cor_filter_simple(latest, ind_pool, thr = corr_threshold)
}
if (length(ind_pool) < min_features) {
  warning(sprintf("Po korelačním filtru zůstalo jen %d indikátorů. Zvažte snížení nároků.", length(ind_pool)))
}

# ==== Škálování (standardizace) ====
scale_latest <- as.data.table(scale(latest[, ..ind_pool]))
scale_latest[, Country := latest$Country]
setcolorder(scale_latest, c("Country", setdiff(names(scale_latest), "Country")))

# ==== Greedy výběr ====
selected <- greedy_select(scale_latest, pool = ind_pool,
                          k_min = min_features, k_max = max_features)

# ==== Finální PCA ====
X <- as.matrix(scale_latest[, ..selected])   # již standardizováno
cnames <- scale_latest$Country

p <- prcomp(X, center = FALSE, scale. = FALSE)

scores <- data.table(`Country Name` = cnames,
                     PC1 = p$x[,1], PC2 = p$x[,2])
loadings <- data.table(indicator = colnames(X),
                       PC1 = p$rotation[,1], PC2 = p$rotation[,2])

imp <- summary(p)$importance
expl_pc1 <- round(100 * imp[2,1], 1)
expl_pc2 <- round(100 * imp[2,2], 1)

# ==== Grafy ====
g_scores <-
  ggplot(scores, aes(x = PC1, y = PC2, label = `Country Name`)) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  geom_point(size = 1.6) +
  geom_text_repel(size = 3, max.overlaps = Inf, box.padding = 0.5, point.padding = 0.3) +
  labs(
    x = paste0("PC1 (", expl_pc1, "%)"),
    y = paste0("PC2 (", expl_pc2, "%)"),
    title = paste0("Countries in PC1–PC2 (", cutoff_year, ")"),
    subtitle = paste0("Selected indicators (", length(selected), "): ",
                      paste(selected, collapse = ", "))
  ) +
  theme_minimal(base_size = 11) + theme(panel.grid = element_blank())

g_loadings <-
  ggplot(loadings, aes(x = PC1, y = PC2, label = indicator)) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  geom_point(size = 1.8) +
  geom_text_repel(size = 3, max.overlaps = Inf, box.padding = 0.5, point.padding = 0.3) +
  labs(
    x = paste0("PC1 (", expl_pc1, "%)"),
    y = paste0("PC2 (", expl_pc2, "%)"),
    title = paste0("Characteristics in PC1–PC2 (", cutoff_year, ")")
  ) +
  theme_minimal(base_size = 11) + theme(panel.grid = element_blank())

 print(g_scores); print(g_loadings)  # při interaktivním běhu

# ==== Export artefaktů ====
saveRDS(
  list(
    pca         = p,
    variables   = colnames(X),
    selected    = selected,
    cutoff_year = cutoff_year
  ),
  file = file.path(outdir, sprintf("pca_model_opt_simple_%d.rds", cutoff_year))
)

fwrite(scores,   file.path(outdir, sprintf("pca_scores_opt_simple_%d_pc12.csv",   cutoff_year)))
fwrite(loadings, file.path(outdir, sprintf("pca_loadings_opt_simple_%d_pc12.csv", cutoff_year)))
ggsave(file.path(outdir, sprintf("pca_scores_opt_simple_%d.png", cutoff_year)),  g_scores,  width = 9, height = 6, dpi = 150)
ggsave(file.path(outdir, sprintf("pca_loadings_opt_simple_%d.png", cutoff_year)), g_loadings, width = 9, height = 6, dpi = 150)

cat(sprintf(
  "\nSelected %d indicators.\nPC1 variance: %.1f%% | PC2 variance: %.1f%% | PC1+PC2: %.1f%%\n",
  length(selected), expl_pc1, expl_pc2, expl_pc1 + expl_pc2
))

