# =======================================================
# 03_project_trajectory_fill_refyear_LOOP.R
# =======================================================

library(data.table)
library(ggplot2)
library(ggrepel)
library(viridis)
library(stringi)

# =================== USER PARAMETERS ===================
years_win <- 1995:2024
ref_year  <- 2025

infile_data <- "qualityoflife_merged.csv"
pca_dir     <- "PCAmodel"
out_dir     <- "trajectory_asciicz"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

model_rds <- file.path(pca_dir, sprintf("pca_model_%d.rds", ref_year))
scores_bg <- file.path(pca_dir, sprintf("pca_scores_%d_pc12.csv", ref_year))

# překlady
tr_countries_file  <- "translated/countries_for_translation.csv"
tr_countries <- fread(tr_countries_file)

translate_vec <- function(vec, dict){
  m <- merge(
    data.table(original = vec),
    dict,
    by = "original",
    all.x = TRUE,
    sort = FALSE
  )
  ifelse(is.na(m$translation), m$original, m$translation)
}

# funkce pro odstranění diakritiky
strip_diacritics <- function(x){
  x <- stri_trans_general(x, "Latin-ASCII")
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

# ================== LOAD PCA MODEL & DATA ==================
mdl <- readRDS(model_rds)
vars <- mdl$variables
dt   <- fread(infile_data)

id_country <- "Country"
id_year    <- "Year"

# background PCA cloud
bg <- NULL
if (file.exists(scores_bg)) {
  bg <- fread(scores_bg)[, .(PC1, PC2)]
}

# fixní osy – musí být spočteny přes celý PCA space
global_xlim <- c(min(bg$PC1, na.rm=TRUE), max(bg$PC1, na.rm=TRUE))
global_ylim <- c(min(bg$PC2, na.rm=TRUE), max(bg$PC2, na.rm=TRUE))

# možnost ručního dotažení (tvoje původní hodnoty fungovaly dobře)
global_xlim <- c(-5.8, 7.8)
global_ylim <- c(-5, 5.5)

# =======================================================
# LOOP PRO VŠECHNY ZEMĚ
# =======================================================

countries <- sort(unique(dt[[id_country]]))

for (country_to_plot in countries) {
  
  cat("Processing:", country_to_plot, "\n")
  
  # Překlad jména země
  cz_country <- translate_vec(country_to_plot, tr_countries)
  
  # Bez diakritiky do souboru
  file_country <- strip_diacritics(cz_country)
  
  # -------------------------------------------------------
  # === PREPROCESSING & PROJECTION ========================
  # -------------------------------------------------------
  
  dsub <- dt[get(id_country) == country_to_plot, c(id_year, vars), with = FALSE]
  setnames(dsub, id_year, "Year")
  dsub <- merge(data.table(Year = years_win), dsub, by = "Year", all.x = TRUE)
  setorder(dsub, Year)
  
  # Fill missing, scale
  for (v in vars) {
    vec <- dsub[[v]]
    if (all(is.na(vec))) vec <- rep(mdl$center[v], length(vec))
    vec <- nafill(vec, type="locf")
    vec <- nafill(vec, type="nocb")
    dsub[[v]] <- (vec - mdl$center[v]) / mdl$scale[v]
  }
  
  X <- as.matrix(dsub[, ..vars])
  proj <- X %*% mdl$pca$rotation
  
  traj <- data.table(
    Year = dsub$Year,
    PC1 = proj[,1],
    PC2 = proj[,2]
  )
  
  # =======================================================
  # METRICS
  # =======================================================
  if (nrow(traj) >= 2) {
    dxv <- diff(traj$PC1)
    dyv <- diff(traj$PC2)
    seg_len <- sqrt(dxv^2 + dyv^2)
    path_length <- sum(seg_len, na.rm=TRUE)
    
    dx <- traj$PC1[nrow(traj)] - traj$PC1[1]
    dy <- traj$PC2[nrow(traj)] - traj$PC2[1]
    net_disp <- sqrt(dx^2 + dy^2)
    
    directionality <- if (path_length>0) net_disp/path_length else NA_real_
  } else {
    path_length <- directionality <- NA_real_
  }
  
  directionality_pct <- 100 * directionality
  
  # -------------------------------------------------------
  # LABELLED YEARS
  # -------------------------------------------------------
  
  label_years <- sort(unique(c(min(years_win), max(years_win),
                               seq(min(years_win), max(years_win), 5))))
  lab_df <- traj[Year %in% label_years]
  
  # -------------------------------------------------------
  # === PLOT =================================================
  # -------------------------------------------------------
  
  bg_layer <- if (!is.null(bg))
    geom_point(data = bg, aes(PC1, PC2),
               size = 1.4, color = "grey80") else NULL
  
  p <- ggplot(traj, aes(PC1, PC2)) +
    geom_hline(yintercept = 0, color="black", linewidth=0.4) +
    geom_vline(xintercept = 0, color="black", linewidth=0.4) +
    coord_cartesian(xlim = global_xlim, ylim = global_ylim) +
    bg_layer +
    geom_path(aes(color = Year), linewidth = 1.0) +
    geom_point(aes(color = Year), size = 2) +
    scale_color_viridis_c(option = "turbo", direction = 1) +
    ggrepel::geom_text_repel(
      data = lab_df,
      aes(PC1, PC2, label = Year, color = Year),
      size = 4, box.padding=0.4, point.padding=0.6,
      segment.size=0.2, seed=123
    ) +
    labs(x=NULL, y=NULL) +
    theme_minimal(base_size = 15) +
    theme(
      panel.grid = element_blank(),
      axis.text  = element_blank(),
      axis.title = element_blank(),
      legend.position = "none"
    ) +
    annotate("text", x = -Inf, y = Inf, hjust = 0, vjust = 1.3,
             label = cz_country, size = 5.3, fontface = "bold") +
    annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1.3,
             label = sprintf("vzdálenost %.2f   |   přímočarost %.1f%%",
                             path_length, directionality_pct),
             size = 5.3)
  
  # -------------------------------------------------------
  # SAVE
  # -------------------------------------------------------
  
  outfile <- file.path(out_dir, sprintf("Fig6_%s.svg", file_country))
  
  ggsave(
    filename = outfile,
    plot = p,
    device = "svg",
    width  = 620/72,
    height = 420/72,
    dpi = 72
  )
}
