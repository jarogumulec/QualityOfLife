# ==== PCA loadings stability analysis 2013–2024 ====

library(data.table)
library(ggplot2)

pca_dir <- "PCAmodel"
years <- 2013:2024

# ---- 1) Load all loadings and combine ----
load_all <- lapply(years, function(y) {
  f <- file.path(pca_dir, sprintf("pca_loadings_%d_pc12.csv", y))
  dt <- fread(f)
  dt$Year <- y
  dt
})

loadings_all <- rbindlist(load_all)

# Ensure structure
# columns: indicator, PC1, PC2, Year
stopifnot(all(c("indicator","PC1","PC2","Year") %in% names(loadings_all)))

# ---- 2) Plot absolute loadings sorted (averages by year) ----
avg_abs <- loadings_all[, .(absPC1 = mean(abs(PC1), na.rm=TRUE),
                            absPC2 = mean(abs(PC2), na.rm=TRUE)), by=indicator]

avg_abs_melt <- melt(avg_abs, id.vars="indicator",
                     variable.name="Component", value.name="abs_loading")

ggplot(avg_abs_melt, aes(x=reorder(indicator, abs_loading), y=abs_loading, fill=Component)) +
  geom_bar(stat="identity", position="dodge") +
  coord_flip() +
  labs(title="Average absolute loadings 2013–2024",
       x="Indicator", y="Mean |loading|") +
  theme_minimal(base_size=11)

# ---- 3) Plot loadings vs year (e.g. PC1) ----
# keep only top ~20 by mean abs loading for readability
top20 <- avg_abs[order(-absPC1)][1:20, indicator]

ggplot(loadings_all[indicator %in% top20],
       aes(x=Year, y=PC1, color=indicator, group=indicator)) +
  geom_line(linewidth=0.7) +
  geom_point(size=1.2) +
  labs(title="PC1 loadings over time (top 20 indicators)",
       y="Loading (PC1)", x="Year") +
  theme_minimal(base_size=11) +
  theme(legend.position="right")


library(ggplot2)
library(data.table)

# Assume loadings_all already contains indicator, PC1, PC2, Year

# Compute total (absolute) loading strength per indicator & year
loadings_all[, total_loading := sqrt(PC1^2 + PC2^2)]

# ---- 3a) Average strength over 2013–2024 ----
avg_strength <- loadings_all[, .(mean_strength = mean(total_loading, na.rm = TRUE)), by = indicator]

ggplot(avg_strength, aes(x = reorder(indicator, mean_strength), y = mean_strength)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Average indicator loading strength (PC1–PC2 combined, 2013–2024)",
    x = "Indicator",
    y = "Mean √(PC1² + PC2²)"
  ) +
  theme_minimal(base_size = 11)



# Assuming loadings_all has columns: indicator, PC1, PC2, Year
loadings_all[, total_loading := sqrt(PC1^2 + PC2^2)]

# ---- 3b) Mean & SD of combined loadings across years ----
stab_total <- loadings_all[, .(
  mean_strength = mean(total_loading, na.rm = TRUE),
  sd_strength   = sd(total_loading, na.rm = TRUE)
), by = indicator]


# Scatter: mean vs SD (stability map) 
# to je dobrý ale chce to predstavivost co to vlastne rika
ggplot(stab_total, aes(x = mean_strength, y = sd_strength, label = indicator)) +
  geom_point(color = "gray30", size = 2) +
  ggrepel::geom_text_repel(size = 3, max.overlaps = Inf, box.padding = 0.3, point.padding = 0.3) +
  labs(
    title = "Indicator strength vs stability (2013–2024)",
    x = "Mean total loading (strength)",
    y = "SD of total loading (instability)"
  ) +
  geom_vline(xintercept = mean(stab_total$mean_strength, na.rm = TRUE), linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = mean(stab_total$sd_strength, na.rm = TRUE), linetype = "dashed", color = "gray50") +
  theme_minimal(base_size = 11)


ggplot(stab_total, aes(x = reorder(indicator, sd_strength), y = sd_strength)) +
  geom_bar(stat = "identity", fill = "tomato", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Indicator variability across years (SD of √(PC1² + PC2²), 2013–2024)",
    x = "Indicator",
    y = "SD of total loading (lower = more stable)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.y = element_text(size = 9),
    plot.title = element_text(size = 12, face = "bold")
  )






# ---- 4) Quantify indicator stability / redundancy ----
stab <- loadings_all[, .(
  mean_abs_PC1 = mean(abs(PC1), na.rm=TRUE),
  sd_abs_PC1   = sd(abs(PC1), na.rm=TRUE),
  mean_abs_PC2 = mean(abs(PC2), na.rm=TRUE),
  sd_abs_PC2   = sd(abs(PC2), na.rm=TRUE)
), by=indicator]

stab[, stability_index := mean_abs_PC1 / (sd_abs_PC1 + 1e-6)]

# Sort indicators by stability and print top/bottom
stab[order(-stability_index)][1:10]
stab[order(stability_index)][1:10]

# ---- 5) Suggest candidates for removal ----
# Low mean strength and/or high instability
stab[, instability := sd_abs_PC1 / (mean_abs_PC1 + 1e-6)]
noisy_indicators <- stab[instability > 0.3, indicator]


cat("Indicators suggested for exclusion (weak/unstable):\n")
print(noisy_indicators)

# ok tak to neco rika, dava pryc oscilujici+slabe.....
# nelibi se mi dat pryc terorismus (oscilujici), 