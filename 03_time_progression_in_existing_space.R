# 03_project_trajectory_fill_refyear.R

library(data.table)
library(ggplot2)
library(ggrepel)

# =================== USER PARAMETERS ===================
country_to_plot <- "Poland"     # e.g. "Czechia", "Slovak Republic"
years_win       <- 1995:2023
ref_year        <- 2024

infile_data <- "qualityoflife_merged.csv"
pca_dir     <- "PCAmodel"
out_dir     <- "trajectory"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

model_rds <- file.path(pca_dir, sprintf("pca_model_%d.rds", ref_year))
scores_bg <- file.path(pca_dir, sprintf("pca_scores_%d_pc12.csv", ref_year))  # optional

out_csv_traj  <- file.path(out_dir, sprintf("trajectory_%s_%d_pc12_locf.csv",
                                            gsub("\\s+","_", country_to_plot), ref_year))
out_csv_stats <- file.path(out_dir, sprintf("trajectory_metrics_%s_%d.csv",
                                            gsub("\\s+","_", country_to_plot), ref_year))
out_png       <- file.path(out_dir, sprintf("trajectory_%s_%d_pc12_locf.png",
                                            gsub("\\s+","_", country_to_plot), ref_year))
# =======================================================

# --- load model and data ---
mdl <- readRDS(model_rds)    # list(pca, variables, center, scale)
vars <- mdl$variables
dt   <- fread(infile_data)

id_country <- "Country"
id_year    <- "Year"

# --- subset to country and ensure full year window ---
dsub <- dt[get(id_country) == country_to_plot, c(id_year, vars), with = FALSE]
setnames(dsub, id_year, "Year")
dsub <- merge(data.table(Year = years_win), dsub, by = "Year", all.x = TRUE)
setorder(dsub, Year)

# --- LOCF + NOCB per indicator; if all NA for this country, set to model center ---
for (v in vars) {
  vec <- dsub[[v]]
  if (all(is.na(vec))) {
    dsub[[v]] <- mdl$center[v]
  } else {
    vec <- nafill(vec, type = "locf")
    vec <- nafill(vec, type = "nocb")
    dsub[[v]] <- vec
  }
}

# --- standardize with same center/scale as the PCA model ---
for (v in vars) dsub[[v]] <- (dsub[[v]] - mdl$center[v]) / mdl$scale[v]

# --- project into PCA space ---
X    <- as.matrix(dsub[, ..vars])
proj <- X %*% mdl$pca$rotation
traj <- data.table(Year = dsub$Year, PC1 = proj[,1], PC2 = proj[,2])

# --- trajectory metrics: path length, net displacement, directionality, angle ---
euclid <- function(x1,y1,x2,y2) sqrt((x2 - x1)^2 + (y2 - y1)^2)

# traj: data.table/data.frame se sloupci PC1, PC2 (bez NA po projekci)

if (nrow(traj) >= 2) {
  # délka cesty (součet eukleid. vzdáleností mezi po sobě jdoucími roky)
  dxv <- diff(traj$PC1)
  dyv <- diff(traj$PC2)
  seg_len     <- sqrt(dxv^2 + dyv^2)
  path_length <- sum(seg_len, na.rm = TRUE)
  
  # čistý posun start -> end
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

# save numeric outputs
fwrite(traj,    out_csv_traj)
fwrite(metrics, out_csv_stats)

# --- optional background: reference-year scores (grey points) ---
bg <- NULL
if (file.exists(scores_bg)) {
  bg <- fread(scores_bg)[, .(PC1, PC2)]
}

# --- label years: endpoints + every 5th year (repelled to reduce overlaps) ---
label_years <- sort(unique(c(min(years_win), max(years_win), seq(min(years_win), max(years_win), by = 5))))
lab_df <- traj[Year %in% label_years]

# --- axis labels with variance explained ---
pc1_lab <- paste0("PC1 (", round(100 * summary(mdl$pca)$importance[2,1], 1), "%)")
pc2_lab <- paste0("PC2 (", round(100 * summary(mdl$pca)$importance[2,2], 1), "%)")
title_lab <- sprintf("%s: trajectory in PCA space (ref=%d, LOCF+NOCB)", country_to_plot, ref_year)

# subtitle with metrics (rounded)
sub_lab <- sprintf("Path length = %.2f, Net disp. = %.2f, Directionality = %.2f, Angle = %.1f°",
                   path_length, net_disp, directionality, angle_deg)

# --- ggplot figure in your clean style ---
p <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.4) +
  { if (!is.null(bg)) geom_point(data = bg, aes(x = PC1, y = PC2),
                                 size = 1.4, color = "grey75") } +
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

cat("Saved trajectory CSV:   ", out_csv_traj,  "\n")
cat("Saved metrics CSV:      ", out_csv_stats, "\n")
cat("Saved figure PNG:       ", out_png,       "\n")
