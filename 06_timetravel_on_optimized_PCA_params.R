# 03_project_trajectory_fill_refyear.R
# Projekce časové trajektorie jedné země do PCA prostoru natrénovaného modelu.
# Funguje s modely obsahujícími center/scale i bez nich (rekonstrukce).

library(data.table)
library(ggplot2)
library(ggrepel)

# =================== USER PARAMETERS ===================
country_to_plot <- "Russian Federation"     # např. "Czechia", "Slovak Republic"
years_win       <- 1995:2025
ref_year        <- 2025   # rok, pro který byl model trénován (cutoff v trainingu)

# Nastavte shodně s tréninkem PCA!
exclude_countries <- c("China", "Japan", "Turkiye")

infile_data <- "qualityoflife_merged.csv"

# cesty k modelům/artefaktům (zkusí více variant názvů)
pca_dirs <- c("PCAmodel_opt_simple", "PCAmodel_opt", "PCAmodel")
out_dir  <- "trajectory"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# jméno RDS modelu – použij první existující
candidate_models <- file.path(pca_dirs, sprintf("pca_model_opt_simple_%d.rds", ref_year))
candidate_models <- c(candidate_models,
                      file.path(pca_dirs, sprintf("pca_model_opt_%d.rds", ref_year)),
                      file.path(pca_dirs, sprintf("pca_model_%d.rds", ref_year)))
model_rds <- candidate_models[file.exists(candidate_models)][1]
if (is.na(model_rds)) stop("Nenalezen žádný RDS model pro daný ref_year.")

# volitelné „background“ skóre (pro šedé body)
candidate_scores <- c(
  file.path(dirname(model_rds), sprintf("pca_scores_opt_simple_%d_pc12.csv", ref_year)),
  file.path(dirname(model_rds), sprintf("pca_scores_opt_%d_pc12.csv",       ref_year)),
  file.path(dirname(model_rds), sprintf("pca_scores_%d_pc12.csv",           ref_year))
)
scores_bg <- candidate_scores[file.exists(candidate_scores)][1]

# výstupy
base_country <- gsub("\\s+","_", country_to_plot)
out_csv_traj  <- file.path(out_dir, sprintf("trajectory_%s_%d_pc12_locf.csv",  base_country, ref_year))
out_csv_stats <- file.path(out_dir, sprintf("trajectory_metrics_%s_%d.csv",    base_country, ref_year))
out_png       <- file.path(out_dir, sprintf("trajectory_%s_%d_pc12_locf.png",  base_country, ref_year))
# =======================================================


# =============== Helpery (konzistentní preprocessing) ===============

# poslední nenulová hodnota ≤ cutoff_year, jinak poslední dostupná vůbec
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

# jednoduché LOCF+NOCB na vektoru
locf_nocb <- function(x) {
  if (all(is.na(x))) return(x)
  x <- nafill(x, type = "locf")
  x <- nafill(x, type = "nocb")
  x
}

# =============== Načtení modelu a dat ===============
mdl <- readRDS(model_rds)   # může obsahovat: pca, variables, selected, center, scale, cutoff_year
dt  <- fread(infile_data)

id_country <- "Country"
id_year    <- "Year"

# Shodná filtrace zemí jako při tréninku
if (length(exclude_countries) > 0) {
  dt <- dt[!(get(id_country) %in% exclude_countries)]
}

# Sada proměnných použitá v modelu
vars <- if (!is.null(mdl$selected)) mdl$selected else mdl$variables
if (is.null(vars) || length(vars) == 0L) stop("V RDS modelu není 'selected' ani 'variables'.")

# Rok cut-off – přednostně z modelu, jinak ref_year
cutoff_used <- if (!is.null(mdl$cutoff_year)) mdl$cutoff_year else ref_year

# =============== Rekonstrukce center/scale, pokud chybí ===============
need_reconstruct <- is.null(mdl$center) || is.null(mdl$scale)

if (need_reconstruct) {
  # 1) latest snapshot přes VŠECHNY (filtrované) země, pouze pro 'vars'
  all_ind <- setdiff(names(dt), c(id_country, id_year))
  if (!all(vars %in% all_ind)) {
    stop("Některé proměnné z modelu v datech chybí: ",
         paste(setdiff(vars, all_ind), collapse = ", "))
  }
  latest_all <- latest_per_country(dt, id_country, id_year,
                                   cutoff_year = cutoff_used, ind_cols = vars)
  # 2) imputace průměrem po sloupcích
  for (v in vars) {
    m <- mean(latest_all[[v]], na.rm = TRUE)
    if (is.finite(m)) {
      idx <- which(is.na(latest_all[[v]]))
      if (length(idx)) set(latest_all, i = idx, j = v, value = m)
    }
  }
  # 3) center/scale z tohoto snapshotu
  center_vec <- sapply(vars, function(v) mean(latest_all[[v]], na.rm = TRUE))
  scale_vec  <- sapply(vars, function(v) sd(latest_all[[v]],   na.rm = TRUE))
  # ochrana proti sd==0
  scale_vec[!is.finite(scale_vec) | scale_vec == 0] <- 1
  
  center_use <- center_vec
  scale_use  <- scale_vec
} else {
  center_use <- mdl$center[vars]
  scale_use  <- mdl$scale[vars]
}

# =============== Připrava časové řady pro cílovou zemi ===============
# podmnožina dat na danou zemi + proměnné
dsub <- dt[get(id_country) == country_to_plot, c(id_year, vars), with = FALSE]
setnames(dsub, id_year, "Year")

# doplň roky do okna a setřiď
dsub <- merge(data.table(Year = years_win), dsub, by = "Year", all.x = TRUE)
setorder(dsub, Year)

# LOCF+NOCB pro každou proměnnou; když zůstane vše NA, použij center
for (v in vars) {
  vec <- dsub[[v]]
  if (all(is.na(vec))) {
    dsub[[v]] <- center_use[[v]]
  } else {
    dsub[[v]] <- locf_nocb(vec)
    # pokud by i po LOCF+NOCB zůstalo NA (okrajový případ), nahraď center
    nas <- which(is.na(dsub[[v]]))
    if (length(nas)) set(dsub, i = nas, j = v, value = center_use[[v]])
  }
}

# =============== Standardizace a projekce ===============
# stejné měřítko jako pro trénink (uložené či rekonstruované)
for (v in vars) {
  dsub[[v]] <- (dsub[[v]] - center_use[[v]]) / scale_use[[v]]
}

X    <- as.matrix(dsub[, ..vars])
proj <- X %*% mdl$pca$rotation
traj <- data.table(Year = dsub$Year, PC1 = proj[,1], PC2 = proj[,2])

# =============== Metriky trajektorie ===============
euclid <- function(x1,y1,x2,y2) sqrt((x2 - x1)^2 + (y2 - y1)^2)

if (nrow(traj) >= 2) {
  dxv <- diff(traj$PC1)
  dyv <- diff(traj$PC2)
  seg_len     <- sqrt(dxv^2 + dyv^2)
  path_length <- sum(seg_len, na.rm = TRUE)
  
  n  <- nrow(traj)
  dx <- traj$PC1[n] - traj$PC1[1]
  dy <- traj$PC2[n] - traj$PC2[1]
  net_disp <- sqrt(dx^2 + dy^2)
  
  directionality <- if (is.finite(path_length) && path_length > 0) net_disp / path_length else NA_real_
  angle_deg      <- atan2(dy, dx) * 180 / pi
} else {
  path_length <- net_disp <- directionality <- angle_deg <- NA_real_
}

metrics <- data.table(
  Country = country_to_plot,
  ref_year = ref_year,
  start_year = min(years_win), end_year = max(years_win),
  path_length = path_length,
  net_displacement = net_disp,
  directionality = directionality,
  angle_deg = angle_deg
)

# =============== Uložení numerických výstupů ===============
fwrite(traj,    out_csv_traj)
fwrite(metrics, out_csv_stats)

# =============== Vizualizace ===============
# volitelný background (šedé body)
bg <- NULL
if (!is.na(scores_bg)) {
  bg <- fread(scores_bg)[, .(PC1, PC2)]
}

pc1_lab <- paste0("PC1 (", round(100 * summary(mdl$pca)$importance[2,1], 1), "%)")
pc2_lab <- paste0("PC2 (", round(100 * summary(mdl$pca)$importance[2,2], 1), "%)")
title_lab <- sprintf("%s: trajectory in PCA space (ref=%d, LOCF+NOCB)", country_to_plot, ref_year)

sub_lab <- sprintf("Path = %.2f, Net = %.2f, Dir = %.2f, Angle = %.1f°",
                   path_length, net_disp, directionality, angle_deg)

label_years <- sort(unique(c(min(years_win), max(years_win), seq(min(years_win), max(years_win), by = 5))))
lab_df <- traj[Year %in% label_years]

p <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.4) +
  { if (!is.null(bg)) geom_point(data = bg, aes(x = PC1, y = PC2),
                                 size = 1.2, color = "grey75") } +
  geom_path(data = traj, aes(x = PC1, y = PC2), linewidth = 1.0, color = "black") +
  geom_point(data = traj, aes(x = PC1, y = PC2), size = 1.6, color = "black") +
  ggrepel::geom_text_repel(data = lab_df, aes(x = PC1, y = PC2, label = Year),
                           size = 3, max.overlaps = Inf, box.padding = 0.3, point.padding = 0.3) +
  labs(x = pc1_lab, y = pc2_lab, title = title_lab, subtitle = sub_lab) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.line  = element_blank(),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text  = element_text(size = 9)
  )
p
ggsave(filename = out_png, plot = p, width = 7.2, height = 4.8, dpi = 300)

cat("Model RDS:              ", model_rds,     "\n")
if (!is.na(scores_bg)) cat("Background scores CSV: ", scores_bg,     "\n")
cat("Saved trajectory CSV:   ", out_csv_traj,  "\n")
cat("Saved metrics CSV:      ", out_csv_stats, "\n")
cat("Saved figure PNG:       ", out_png,       "\n")
