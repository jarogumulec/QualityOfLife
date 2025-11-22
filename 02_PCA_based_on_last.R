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
#exclude_indicators_like  <- c("Relig_Buddhists", "GINI Index", "Forrest area", "Socal contributions")
exclude_indicators_like  <- c("Relig_Buddhists", "LGBT Public Opinion Index", "LGBT Legal index")
# exclude_indicators_like  <- character(0) #kdyz nic

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







# =======================================================
# === Načtení překladů zemí a indikátorů ================
# =======================================================

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

# =======================================================
# === Aplikace překladů do PCA artefaktů ================
# =======================================================

# --- 1) Scores (země) ---
scores[, Country_translated :=
         translate_vec(`Country Name`, tr_countries)]

# --- 2) Loadings (indikátory) ---
loadings[, indicator_translated :=
           translate_vec(indicator, tr_indicators)]




# =======================================================
# === REGIONY: EU / Evropa mimo EU / Mimo Evropu =========
# =======================================================

eu_members <- c(
  "Austria","Belgium","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland",
  "France","Germany","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg",
  "Malta","Netherlands","Poland","Portugal","Romania","Slovak Republic","Slovenia","Spain","Sweden"
)

europe_non_eu <- c(
  "Albania","Bosnia and Herzegovina","Belarus","Iceland","Moldova","Montenegro",
  "North Macedonia","Norway","Serbia","Switzerland","Ukraine","United Kingdom"
)

scores[, Region := fifelse(
  `Country Name` %in% eu_members, "EU",
  fifelse(`Country Name` %in% europe_non_eu, "Evropa mimo EU", "Mimo Evropu")
)]

scores[, Region := factor(Region, levels = c("EU","Evropa mimo EU","Mimo Evropu"))]








# ==== 6) Plots ====
#nontranslated
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
#translated

library(ggplot2)
library(ggrepel)

pplot <- ggplot(scores, aes(PC1, PC2)) +
  
  # --- Osy ---
  geom_vline(xintercept = 0, color = "black", linewidth = 0.4) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  
  # --- Kompromisní text: malé odskočení, občasný překryv ---
  geom_text_repel(
    aes(label = Country_translated, color = Region),
    size = 3,
    
    # Klíčové parametry:
    force = 0.1,            # malé odpuzování
    force_pull = 0.01,      # text se drží u bodu
    max.overlaps = Inf,     # nezmizí
    box.padding = 0.0,     # minimální padding
    point.padding = 0.02,
    max.time = 0.5,         # krátký čas → méně odskočení
    min.segment.length = Inf,  # bez spojovacích čar
    
    show.legend = FALSE
  ) +
  
  # --- Barvy ---
  scale_color_manual(values = c(
    "EU"             = "#4C72B0",
    "Evropa mimo EU" = "#55A868",
    "Mimo Evropu"    = "#C44E52"
  )) +
  
  # --- Popisky os ---
  annotate("text",
           x = max(scores$PC1)*0.97, y = -0.3,
           label = "PC1", size = 2.6, hjust = 1) +
  annotate("text",
           x = 0.3, y = max(scores$PC2)*0.97,
           label = "PC2", size = 2.6, vjust = 1, angle = 90) +
  
  # --- Téma ---
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    axis.text  = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

print(pplot)



# jak chtel Honza puntiky zpet
pplot <- ggplot(scores, aes(PC1, PC2)) +
  
  # --- Osy ---
  geom_vline(xintercept = 0, color = "black", linewidth = 0.4) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  
  # --- Puntíky států ---
  geom_point(aes(color = Region), size = 1.5, show.legend = FALSE) +
  
  # --- Kompromisní text: malé odskočení, povolený mírný překryv ---
  geom_text_repel(
    aes(label = Country_translated, color = Region),
    size = 3,
    
    # minimal repel
    force = 0.1,
    force_pull = 0.01,
    
    max.overlaps = Inf,     # nikdy nezmizí
    box.padding = 0.2,
    point.padding = 0.02,
    max.time = 0.5,
    min.segment.length = Inf,   # bez čárek
    
    show.legend = FALSE
  ) +
  
  # --- Barvy regionů ---
  scale_color_manual(values = c(
    "EU"             = "#4C72B0",
    "Evropa mimo EU" = "#55A868",
    "Mimo Evropu"    = "#C44E52"
  )) +
  
  # # --- Popisky os ---
  # annotate("text",
  #          x = max(scores$PC1)*0.97, y = -0.3,
  #          label = "PC1", size = 2.6, hjust = 1) +
  # annotate("text",
  #          x = 0.3, y = max(scores$PC2)*0.97,
  #          label = "PC2", size = 2.6, vjust = 1, angle = 90) +
  # 
  # --- Téma ---
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    axis.text  = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

print(pplot)





#save at 400x400px, that looks nice
ggsave(
  filename = "PCA.svg",
  plot     = pplot,         # zde musí být objekt tvého PCA grafu
  device   = svglite,
  width    = 620/96,    # ≈ 4.17 in
  height   = 460/96,    # ≈ 4.17 in
  units    = "in"
)


# loadings -------------
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

#translated
ggplot(loadings, aes(x = PC1, y = PC2, label =  indicator_translated)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.4) +
  geom_point(size = 1.8, color = "gray25") +
  geom_text_repel(size = 3, max.overlaps = Inf, box.padding = 0.5, point.padding = 0.3) +
  labs(
    x = paste0("PC1 (", round(100 * summary(p)$importance[2,1], 1), "%)"),
    y = paste0("PC2 (", round(100 * summary(p)$importance[2,2], 1), "%)")
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_blank(),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9)
  )



# loadings translated, s rotovanými popisky ------

library(ggplot2)
library(data.table)
library(dplyr)

df <- as.data.table(loadings)

# --- Výpočet úhlů 0–360 ---
df[, angle := atan2(PC2, PC1) * 180/pi]
df[angle < 0, angle := angle + 360]

# --- barva podle úhlu ---
df[, color := hsv(h = angle/360, s = 0.9, v = 0.9)]

# --- rotace textu + zarovnání ---
df[, raw_angle := atan2(PC2, PC1) * 180/pi]
df[, angle2 := ifelse(cos(raw_angle*pi/180) >= 0, raw_angle, raw_angle + 180)]
df[, hjust  := ifelse(cos(raw_angle*pi/180) >= 0, 0, 1)]

# --- odsazení od šipky ---
df[, label_x := PC1 + 0.02 * sign(PC1)]
df[, label_y := PC2 + 0.02 * sign(PC2)]

# --- ořez grafu ---
xr <- range(df$label_x)
yr <- range(df$label_y)
expand_x <- diff(xr) * 0.15
expand_y <- diff(yr) * 0.15

xmin <- xr[1] - expand_x
xmax <- xr[2] + expand_x
ymin <- yr[1] - expand_y
ymax <- yr[2] + expand_y

# --- graf ---
pplot <- ggplot(df, aes(PC1, PC2)) +
  
  # vektory
  geom_segment(
    aes(x = 0, y = 0, xend = PC1, yend = PC2, color = color),
    linewidth = 0.55,
    alpha = 0.95,
    arrow = arrow(type = "closed", length = unit(0.20, "cm"))
  ) +
  
  # text indikátorů
  geom_text(
    aes(x = label_x, y = label_y,
        label = indicator_translated,
        angle = angle2,
        hjust = hjust,
        color = color),
    size = 3
  ) +
  
  # osy
  geom_vline(xintercept = 0, linewidth = 0.45, color = "black") +
  geom_hline(yintercept = 0, linewidth = 0.45, color = "black") +
  
  # popisky os — malé, těsně u os
  annotate(
    "text",
    x = xmax * 0.98,
    y = 0,
    label = "PC1",
    size = 2.8,
    hjust = 1,
    vjust = -0.3
  ) +
  annotate(
    "text",
    x = 0,
    y = ymax * 0.98,
    label = "PC2",
    size = 2.8,
    hjust = -0.3,
    vjust = 1,
    angle = 90
  ) +
  
  scale_color_identity() +
  coord_fixed(xlim = c(xmin, xmax), ylim = c(ymin, ymax), clip = "off") +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text  = element_blank(),
    axis.title = element_blank()
  )

pplot


# šipky 1barevné jak chtěl Honza

library(ggplot2)
library(data.table)
library(dplyr)

df <- as.data.table(loadings)

# --- výpočet úhlů pro rotaci textu ---
df[, raw_angle := atan2(PC2, PC1) * 180/pi]
df[, angle2 := ifelse(cos(raw_angle*pi/180) >= 0, raw_angle, raw_angle + 180)]
df[, hjust  := ifelse(cos(raw_angle*pi/180) >= 0, 0, 1)]

# --- odsazení textu od konce vektoru ---
df[, label_x := PC1 + 0.02 * sign(PC1)]
df[, label_y := PC2 + 0.02 * sign(PC2)]

# --- ořez grafu (jako předtím) ---
xr <- range(df$label_x)
yr <- range(df$label_y)
expand_x <- diff(xr) * 0.15
expand_y <- diff(yr) * 0.15

xmin <- xr[1] - expand_x
xmax <- xr[2] + expand_x
ymin <- yr[1] - expand_y
ymax <- yr[2] + expand_y

# --- JEDNA BARVA ---
arrow_col <- "#D45A6A"   # pastelová červená
text_col  <- "black"

pplot <- ggplot(df, aes(PC1, PC2)) +
  
  # vektory (šipky)
  geom_segment(
    aes(x = 0, y = 0, xend = PC1, yend = PC2),
    color = arrow_col,
    linewidth = 0.55,
    alpha = 0.95,
    arrow = arrow(type = "closed", length = unit(0.20, "cm"))
  ) +
  
  # text indikátorů
  geom_text(
    aes(x = label_x, y = label_y,
        label = indicator_translated,
        angle = angle2,
        hjust = hjust),
    size = 3,
    color = text_col
  ) +
  
  # osy
  geom_vline(xintercept = 0, linewidth = 0.45, color = "black") +
  geom_hline(yintercept = 0, linewidth = 0.45, color = "black") +
  
  # popis PC1/PC2
  annotate(
    "text",
    x = xmax * 0.98, y = 0,
    label = "PC1",
    size = 2.8,
    hjust = 1,
    vjust = -0.3
  ) +
  annotate(
    "text",
    x = 0, y = ymax * 0.98,
    label = "PC2",
    size = 2.8,
    hjust = -0.3,
    vjust = 1,
    angle = 90
  ) +
  
  coord_fixed(xlim = c(xmin, xmax),
              ylim = c(ymin, ymax),
              clip = "off") +
  
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text  = element_blank(),
    axis.title = element_blank()
  )

pplot




library(svglite)
ggsave(
  filename = "pca_loadings_sunburst.svg",
  plot     = p,
  device   = svglite,
  width    = 380/72,   # ≈ 7.36 in
  height   = 440/72,   # ≈ 8.33 in
  units    = "in"
)



# 530x600 export




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

# ==== Export the PCA input matrix (countries × indicators) ====
fixedyear_out <- fixedyear[, c(id_country, ind_cols), with = FALSE]
fwrite(fixedyear_out, "fixedyear_latest.csv")
