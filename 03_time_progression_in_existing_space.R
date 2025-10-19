# 03_project_trajectory_fill.R
# Projekce trajektorie jedné země do fixního PCA prostoru s LOCF+NOCB uvnitř země.
# Vstupy: pca_model_latest.rds (z minulého skriptu), qualityoflife_wide.csv
# Volitelné: pca_scores_latest_pc12.csv pro šedé pozadí

library(data.table)

country_to_plot <- "Germany"  # "Slovak Republic" # Slovak Republic # Czechia 
years_win <- 1995:2023

infile_data   <- "qualityoflife_wide.csv"
model_rds     <- "pca_model_latest.rds"
scores_bg_csv <- "pca_scores_latest_pc12.csv"   # nebo NULL
out_csv       <- sprintf("trajectory_%s_pc12_locf.csv", gsub("\\s+","_",country_to_plot))

# --- načti model a data ---
mdl <- readRDS(model_rds)          # list(pca, variables, center, scale)
vars <- mdl$variables              # přesně ty sloupce, na které je PCA natrénované
dt  <- fread(infile_data)

id_country <- "Country Name"
id_year    <- "Year"

# --- podmnožina země a let, vytvoř plné roky 1995–2023 ---
dsub <- dt[get(id_country) == country_to_plot, c(id_year, vars), with = FALSE]
setnames(dsub, id_year, "Year")
dsub <- merge(data.table(Year = years_win), dsub, by = "Year", all.x = TRUE)
setorder(dsub, Year)

# --- LOCF + NOCB per indikátor; pokud vše NA, nastav na modelový průměr (neutral) ---
for (v in vars) {
  vec <- dsub[[v]]
  if (all(is.na(vec))) {
    # indikátor pro danou zemi nikdy nemá hodnotu -> nastav na modelový průměr (raw units)
    dsub[[v]] <- mdl$center[v]
  } else {
    # vyplň uvnitř intervalu 1995–2023 dopředně i zpětně
    vec <- nafill(vec, type = "locf")
    vec <- nafill(vec, type = "nocb")
    dsub[[v]] <- vec
  }
}

# --- standardizace stejnými parametry jako PCA model ---
for (v in vars) {
  dsub[[v]] <- (dsub[[v]] - mdl$center[v]) / mdl$scale[v]
}

# --- projekce do PCA prostoru ---
X <- as.matrix(dsub[, ..vars])
proj <- X %*% mdl$pca$rotation
traj <- data.table(Year = dsub$Year, PC1 = proj[,1], PC2 = proj[,2])

fwrite(traj, out_csv)

# --- jednoduchý plot + volitelné pozadí ---
if (!is.null(scores_bg_csv) && file.exists(scores_bg_csv)) {
  bg <- fread(scores_bg_csv)
  plot(bg$PC1, bg$PC2, pch = 19, cex = 0.5, col = "grey75",
       xlab = paste0("PC1 (", round(100*summary(mdl$pca)$importance[2,1],1), "%)"),
       ylab = paste0("PC2 (", round(100*summary(mdl$pca)$importance[2,2],1), "%)"),
       main = sprintf("%s: trajectory in PCA space (LOCF+NOCB inside country)", country_to_plot))
} else {
  plot(NA, xlim = range(traj$PC1), ylim = range(traj$PC2),
       xlab = paste0("PC1 (", round(100*summary(mdl$pca)$importance[2,1],1), "%)"),
       ylab = paste0("PC2 (", round(100*summary(mdl$pca)$importance[2,2],1), "%)"),
       main = sprintf("%s: trajectory in PCA space (LOCF+NOCB)", country_to_plot))
}

abline(h = 0, v = 0, col = "grey85", lty = 3)
lines(traj$PC1, traj$PC2, lwd = 2)
points(traj$PC1, traj$PC2, pch = 19, cex = 0.6)
lab_idx <- unique(c(1, nrow(traj), seq(1, nrow(traj), by = 5)))
text(traj$PC1[lab_idx], traj$PC2[lab_idx], labels = traj$Year[lab_idx], cex = 0.7, pos = 3)

cat("Saved trajectory: ", out_csv, "\n")
