# =======================================================
# 03_project_trajectory_fill_refyear.R (corrected)
# =======================================================

library(data.table)
library(ggplot2)
library(ggrepel)
library(viridis)

# =================== USER PARAMETERS ===================
country_to_plot <- "Sweden"   # e.g. "Czechia"
years_win       <- 1995:2024
ref_year        <- 2025              # PCA trained on ≤2024 data 

infile_data <- "qualityoflife_merged.csv"
pca_dir     <- "PCAmodel"
out_dir     <- "trajectory"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

model_rds <- file.path(pca_dir, sprintf("pca_model_%d.rds", ref_year))
scores_bg <- file.path(pca_dir, sprintf("pca_scores_%d_pc12.csv", ref_year))

out_csv_traj  <- file.path(out_dir, sprintf("trajectory_%s_%d_pc12_locf.csv",
                                            gsub("\\s+","_", country_to_plot), ref_year))
out_csv_stats <- file.path(out_dir, sprintf("trajectory_metrics_%s_%d.csv",
                                            gsub("\\s+","_", country_to_plot), ref_year))
out_png       <- file.path(out_dir, sprintf("trajectory_%s_%d_pc12_locf.png",
                                            gsub("\\s+","_", country_to_plot), ref_year))
# =======================================================


# -------------------------------------------------------
# === LOAD MODEL AND INPUT DATA ===
# -------------------------------------------------------
mdl <- readRDS(model_rds)    # list(pca, variables, center, scale)
vars <- mdl$variables
dt   <- fread(infile_data)

id_country <- "Country"
id_year    <- "Year"


# -------------------------------------------------------
# === PROJECT ONE COUNTRY TRAJECTORY ===
# -------------------------------------------------------
dsub <- dt[get(id_country) == country_to_plot, c(id_year, vars), with = FALSE]
setnames(dsub, id_year, "Year")
dsub <- merge(data.table(Year = years_win), dsub, by = "Year", all.x = TRUE)
setorder(dsub, Year)



# Fill missing data and standardize relative to PCA model
for (v in vars) {
  vec <- dsub[[v]]
  if (all(is.na(vec))) {
    vec <- rep(mdl$center[v], length(vec))
  } else {
    vec <- nafill(vec, type = "locf")
    vec <- nafill(vec, type = "nocb")
  }
  dsub[[v]] <- (vec - mdl$center[v]) / mdl$scale[v]
}


# -------------------------------------------------------
# === ALIGN AND PROJECT TRAJECTORY INTO PCA SPACE =======
# -------------------------------------------------------

# Reorder matrix columns to PCA variable order
X <- as.matrix(dsub[, ..vars])

# Project into PCA space
proj <- X %*% mdl$pca$rotation

# Construct trajectory table
traj <- data.table(
  Year = dsub$Year,
  PC1  = proj[, 1],
  PC2  = proj[, 2]
)

# -------------------------------------------------------
# === COMPUTE TRAJECTORY METRICS ========================
# -------------------------------------------------------

if (nrow(traj) >= 2) {
  
  dxv <- diff(traj$PC1)
  dyv <- diff(traj$PC2)
  seg_len <- sqrt(dxv^2 + dyv^2)
  
  path_length <- sum(seg_len, na.rm = TRUE)
  
  n  <- nrow(traj)
  dx <- traj$PC1[n] - traj$PC1[1]
  dy <- traj$PC2[n] - traj$PC2[1]
  net_disp <- sqrt(dx^2 + dy^2)
  
  directionality <- if (path_length > 0) net_disp / path_length else NA_real_
  angle_deg      <- atan2(dy, dx) * 180 / pi
  
} else {
  path_length <- NA_real_
  net_disp <- NA_real_
  directionality <- NA_real_
  angle_deg <- NA_real_
}




# Compute metrics
if (nrow(traj) >= 2) {
  dxv <- diff(traj$PC1)
  dyv <- diff(traj$PC2)
  seg_len <- sqrt(dxv^2 + dyv^2)
  path_length <- sum(seg_len, na.rm = TRUE)
  
  n <- nrow(traj)
  dx <- traj$PC1[n] - traj$PC1[1]
  dy <- traj$PC2[n] - traj$PC2[1]
  net_disp <- sqrt(dx^2 + dy^2)
  directionality <- if (path_length > 0) net_disp / path_length else NA_real_
  angle_deg <- atan2(dy, dx) * 180 / pi
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

fwrite(traj,    out_csv_traj)
fwrite(metrics, out_csv_stats)


# -------------------------------------------------------
# === PLOT SINGLE COUNTRY TRAJECTORY (fixed) ===
# -------------------------------------------------------

bg <- NULL
if (file.exists(scores_bg)) {
  bg <- fread(scores_bg)[, .(PC1, PC2)]
}

label_years <- sort(unique(c(min(years_win), max(years_win), seq(min(years_win), max(years_win), by = 5))))
lab_df <- traj[Year %in% label_years]

pc1_lab <- paste0("PC1 (", round(100 * summary(mdl$pca)$importance[2,1], 1), "%)")
pc2_lab <- paste0("PC2 (", round(100 * summary(mdl$pca)$importance[2,2], 1), "%)")
title_lab <- sprintf("%s: trajectory in PCA space (ref=%d, LOCF+NOCB)", country_to_plot, ref_year)
sub_lab   <- sprintf("Path length = %.2f, Net disp. = %.2f, Directionality = %.2f, Angle = %.1f°",
                     path_length, net_disp, directionality, angle_deg)

# background layer created explicitly
bg_layer <- if (!is.null(bg)) geom_point(data = bg, aes(PC1, PC2),
                                         size = 1.4, color = "grey75") else NULL

p <- ggplot(traj, aes(PC1, PC2)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.4) +
  bg_layer +
  geom_path(aes(color = Year), linewidth = 1.0) +
  geom_point(aes(color = Year), size = 2) +
  scale_color_viridis_c(option = "turbo", direction = 1) +
  ggrepel::geom_text_repel(
    data = lab_df, aes(PC1, PC2, label = Year, color = Year),
    size = 3, box.padding = 0.5, point.padding = 0.8,
    segment.size = 0.2, seed = 123
  ) +
  labs(
    x = pc1_lab, y = pc2_lab,
    title = title_lab, subtitle = sub_lab, color = "Year"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_blank(),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    legend.position = "none"
  )



print(p)  # ensure it displays interactively

ggsave(filename = out_png, plot = p, width = 7.2, height = 4.8, dpi = 300)



# Vizuál překlopený do češtiny

# Cesty k tvým překladovým tabulkám
tr_countries_file  <- "translated/countries_for_translation.csv"
tr_indicators_file <- "translated/indicators_for_translation.csv"

# Načtení překladů (volitelné)
tr_countries  <- fread(tr_countries_file)   # sloupce: original, translation
tr_indicators <- fread(tr_indicators_file)  # sloupce: original, translation

# Zajistit správné názvy
stopifnot(all(c("original", "translation") %in% names(tr_countries)))
stopifnot(all(c("original", "translation") %in% names(tr_indicators)))

# =======================================================
# === Funkce pro přemapování =============================
# =======================================================

translate_vec <- function(vec, dict) {
  m <- merge(
    data.table(original = vec),
    dict,
    by = "original",
    all.x = TRUE,
    sort = FALSE
  )
  # pokud není překlad, vrátí původní
  ifelse(is.na(m$translation), m$original, m$translation)
}


# ======= Přeložený název země =======
cz_country <- translate_vec(country_to_plot, tr_countries)

# ======= Přímočarost v procentech =======



# === RECOMPUTE METRICS FOR THIS TRAJ ===
if (nrow(traj) >= 2) {
  dxv <- diff(traj$PC1)
  dyv <- diff(traj$PC2)
  seg_len <- sqrt(dxv^2 + dyv^2)
  path_length <- sum(seg_len, na.rm = TRUE)
  
  n <- nrow(traj)
  dx <- traj$PC1[n] - traj$PC1[1]
  dy <- traj$PC2[n] - traj$PC2[1]
  net_disp <- sqrt(dx^2 + dy^2)
  directionality <- if (path_length > 0) net_disp / path_length else NA_real_
  angle_deg <- atan2(dy, dx) * 180 / pi
} else {
  path_length <- net_disp <- directionality <- angle_deg <- NA_real_
}

directionality_pct <- directionality * 100

title_lab <- sprintf("%s", cz_country)
subtitle_lab <- sprintf("vzdálenost = %.2f, přímočarost = %.1f%%",
                        path_length, directionality_pct)




# ======= Vykreslení (sjednocený vzhled jako PCA států) =======

bg_layer <- if (!is.null(bg))
  geom_point(data = bg, aes(PC1, PC2),
             size = 1.4, color = "grey75") else NULL

p <- ggplot(traj, aes(PC1, PC2)) +
  
  # --- Osy ---
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.4) +
  coord_cartesian(xlim = c(-5.5, 7.3), # 7.5 max rusko ok 
                 ylim = c(-5, 5.5)) + # 5.4 5.4 ok

  # --- Pozadí ostatních států ---
  bg_layer +
  
  # --- Trajektorie ---
  geom_path(aes(color = Year), linewidth = 1.0) +
  geom_point(aes(color = Year), size = 2) +
  scale_color_viridis_c(option = "turbo", direction = 1) +
  
  # --- Popisky vybraných roků ---
  ggrepel::geom_text_repel(
    data = lab_df, aes(PC1, PC2, label = Year, color = Year),
    size = 4, box.padding = 0.4, point.padding = 0.6,
    segment.size = 0.2, seed = 123
  ) +
  
  # --- Český popisek os ---
  labs(
    x = NULL,
    y = NULL
 #   title = title_lab,
#  subtitle = subtitle_lab
  ) +
  
  theme_minimal(base_size = 15) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_blank(),
    axis.title = element_text(size = 14),
    axis.text  = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0),
   plot.subtitle = element_text(size = 14, hjust = 0),
    legend.position = "none"
  )

p <- p +
  annotate("text", x = -Inf, y = Inf, hjust = 0, vjust = 1.3,
           label = sprintf("%s", cz_country),
           size = 5.3, fontface = "bold") +
  annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1.3,
           label = sprintf("vzdálenost %.2f, přímočarost %.1f%%",
                           path_length, directionality_pct),
           size = 5.3)


print(p)


#450x300


ggsave(
  filename = file.path(out_dir, sprintf("Fig6_%s_%d.svg",
                                        gsub("\\s+","_", cz_country), ref_year)),
  plot = p,
  device = "svg",
  width = 620/72,   # 450 px při 72 dpi # 320x210
  height = 420/72,  # 300 px při 72 dpi
  dpi = 72
)




# -------------------------------------------------------
# === METRICS FOR ALL COUNTRIES (CONSISTENTLY) ===
# -------------------------------------------------------
message("Computing metrics for all countries...")

dt_all <- fread(infile_data)
countries <- sort(unique(dt_all[[id_country]]))
metrics_list <- list()

for (cty in countries) {
  dsub <- dt_all[get(id_country) == cty, c(id_year, vars), with = FALSE]
  setnames(dsub, id_year, "Year")
  dsub <- merge(data.table(Year = years_win), dsub, by = "Year", all.x = TRUE)
  setorder(dsub, Year)
  
  # --- consistent LOCF + NOCB and scaling with model alignment ---
  for (v in vars) {
    if (!v %in% names(dsub)) dsub[[v]] <- mdl$center[v]
    vec <- dsub[[v]]
    if (all(is.na(vec))) vec <- rep(mdl$center[v], length(vec))
    vec <- nafill(vec, type = "locf")
    vec <- nafill(vec, type = "nocb")
    dsub[[v]] <- (vec - mdl$center[v]) / mdl$scale[v]
  }
  
  # --- reorder columns ---
  X <- as.matrix(dsub[, ..vars])
  proj <- X %*% mdl$pca$rotation
  traj_cty <- data.table(Year = dsub$Year, PC1 = proj[,1], PC2 = proj[,2])
  
  if (nrow(traj_cty) < 2) next
  
  dxv <- diff(traj_cty$PC1)
  dyv <- diff(traj_cty$PC2)
  seg_len <- sqrt(dxv^2 + dyv^2)
  path_length <- sum(seg_len, na.rm = TRUE)
  
  n <- nrow(traj_cty)
  dx <- traj_cty$PC1[n] - traj_cty$PC1[1]
  dy <- traj_cty$PC2[n] - traj_cty$PC2[1]
  net_disp <- sqrt(dx^2 + dy^2)
  directionality <- if (path_length > 0) net_disp / path_length else NA_real_
  angle_deg <- atan2(dy, dx) * 180 / pi
  
  metrics_list[[cty]] <- data.table(
    Country = cty,
    path_length = path_length,
    net_displacement = net_disp,
    directionality = directionality,
    angle_deg = angle_deg
  )
}

metrics_all <- rbindlist(metrics_list, use.names = TRUE, fill = TRUE)
out_csv_all <- file.path(out_dir, sprintf("trajectory_metrics_all_%d.csv", ref_year))
fwrite(metrics_all, out_csv_all)
cat("Saved all-country metrics CSV:", out_csv_all, "\n")


# -------------------------------------------------------
# === CLASSIFY COUNTRIES INTO REGIONAL GROUPS ===
# -------------------------------------------------------
eu_members <- c(
  "Austria","Belgium","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland",
  "France","Germany","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg",
  "Malta","Netherlands","Poland","Portugal","Romania","Slovak Republic","Slovenia","Spain","Sweden"
)

europe_non_eu <- c(
  "Albania","Bosnia and Herzegovina","Belarus","Iceland","Moldova","Montenegro",
  "North Macedonia","Norway","Serbia","Switzerland","Ukraine","United Kingdom"
)

metrics_all[, Region := fifelse(Country %in% eu_members, "EU",
                                fifelse(Country %in% europe_non_eu, "Europe (non-EU)",
                                        "Non-European"))]
metrics_all[, Region := factor(Region, levels = c("EU","Europe (non-EU)","Non-European"))]


# -------------------------------------------------------
# === VISUALIZATION: COMPARATIVE METRICS ===
# -------------------------------------------------------

# --- 1) Path length ---
ggplot(metrics_all[order(path_length)], aes(x = reorder(Country, path_length), y = path_length, fill = Region)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("EU" = "#4C72B0", "Europe (non-EU)" = "#55A868", "Non-European" = "#C44E52")) +
  theme_minimal(base_size = 11) +
  labs(
    title = sprintf("Path length across countries (ref=%d)", ref_year),
    x = NULL, y = "Path length", fill = NULL
  ) +
  theme(legend.position = "bottom")

# --- 2) Net displacement ---
ggplot(metrics_all[order(net_displacement)], aes(x = reorder(Country, net_displacement), y = net_displacement, fill = Region)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("EU" = "#4C72B0", "Europe (non-EU)" = "#55A868", "Non-European" = "#C44E52")) +
  theme_minimal(base_size = 11) +
  labs(
    title = sprintf("Net displacement across countries (ref=%d)", ref_year),
    x = NULL, y = "Net displacement", fill = NULL
  ) +
  theme(legend.position = "bottom")



# =======================================================
# === 1) Scatterplot: Directionality vs Path Length  ====
# =======================================================

ggplot(metrics_all, aes(x = path_length, y = directionality, color = Region)) +
  geom_point(size = 3, alpha = 0.9) +
  geom_text_repel(
    aes(label = Country),
    size = 3, color = "black", max.overlaps = Inf,
    box.padding = 0.3, point.padding = 0.2, seed = 42
  ) +
  scale_color_manual(values = c("EU" = "#4C72B0", "Europe (non-EU)" = "#55A868", "Non-European" = "#C44E52")) +
  theme_minimal(base_size = 11) +
  labs(
    title = sprintf("Trajectory coherence vs extent (ref=%d)", ref_year),
    x = "Path length", y = "Directionality", color = NULL
  ) +
  theme(legend.position = "bottom")



# =======================================================
# === 2) Polar Angle Plot (zoomed, angled labels)  ======
# =======================================================

library(ggplot2)

metrics_all[, angle_rad := angle_deg * pi / 180]
metrics_all[, radius := 1 + 0.05 * (path_length / max(path_length, na.rm = TRUE))]

# --- optional: focus on dense cluster window around median angle ---
ang_med <- median(metrics_all$angle_deg, na.rm = TRUE)
window <- 40   # ±degrees around the median to display
metrics_zoom <- metrics_all[angle_deg > ang_med - window & angle_deg < ang_med + window]

ggplot(metrics_zoom, aes(x = angle_rad, y = radius)) +
  geom_point(size = 2.6, color = "black") +
  geom_text(
    aes(label = Country,
        angle = angle_deg,           # label rotated along spoke
        hjust = ifelse(angle_deg > 90 | angle_deg < -90, 1, 0)), 
    size = 3, vjust = 0.5, color = "black", check_overlap = FALSE
  ) +
  coord_polar(start = pi/2, direction = -1, clip = "off") +
  expand_limits(y = 1.3) +
  theme_void(base_size = 11) +
  labs(
    title = sprintf("Trajectory directions across countries (ref=%d, ±%d° zoom)", ref_year, window)
  ) +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
  )
