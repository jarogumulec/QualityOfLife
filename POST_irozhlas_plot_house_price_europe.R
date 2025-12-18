library(data.table)
library(ggplot2)
library(httpgd)

httpgd::hgd()
httpgd::hgd_browse()

dt <- fread("qualityoflife_merged.csv")

european_countries <- c("Austria", "Czech Republic", "Czechia", "Germany", "Hungary", "Poland", "Slovak Republic")

dt_europe <- dt[Country %in% european_countries & !is.na(`House_Price To Income Ratio`)]
dt_europe[, `House_Price To Income Ratio` := as.numeric(`House_Price To Income Ratio`)]
dt_europe[, Year := as.numeric(Year)]
dt_europe <- dt_europe[!is.na(`House_Price To Income Ratio`) & !is.na(Year)]

p <- ggplot(dt_europe, aes(x = Year, y = `House_Price To Income Ratio`, 
                           color = Country, group = Country)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  labs(title = "House Price To Income Ratio - Evropa", x = "Rok", y = "Ratio") +
  theme_minimal()

print(p)
