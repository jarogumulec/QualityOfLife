library(data.table)
library(ggplot2)
library(httpgd)

httpgd::hgd()
httpgd::hgd_browse()

dt <- fread("qualityoflife_merged.csv")

european_countries <- c("Italy", "Czechia", "Germany")

dt_europe <- dt[Country %in% european_countries & !is.na(`House_Price To Income Ratio`)]
dt_europe[, `House_Price To Income Ratio` := as.numeric(`House_Price To Income Ratio`)]
dt_europe[, Year := as.numeric(Year)]
dt_europe <- dt_europe[!is.na(`House_Price To Income Ratio`) & !is.na(Year)]

p1 <- ggplot(dt_europe, aes(x = Year, y = `House_Price To Income Ratio`, 
                           color = Country, group = Country)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  labs(title = "House Price To Income Ratio - Evropa", x = "Rok", y = "Ratio") +
  theme_minimal()
print(p1)

# Property Own Outright
dt_own <- dt[Country %in% european_countries & !is.na(`property_Own outright`)]
dt_own[, `property_Own outright` := as.numeric(`property_Own outright`)]
dt_own[, Year := as.numeric(Year)]
dt_own <- dt_own[!is.na(`property_Own outright`) & !is.na(Year)]

p2 <- ggplot(dt_own, aes(x = Year, y = `property_Own outright`, 
                         color = Country, group = Country)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  labs(title = "Property Own Outright - Evropa", x = "Rok", y = "%") +
  theme_minimal()
print(p2)

# Property Owner with Mortgage
dt_mortgage <- dt[Country %in% european_countries & !is.na(`Property_Owner with mortgage`)]
dt_mortgage[, `Property_Owner with mortgage` := as.numeric(`Property_Owner with mortgage`)]
dt_mortgage[, Year := as.numeric(Year)]
dt_mortgage <- dt_mortgage[!is.na(`Property_Owner with mortgage`) & !is.na(Year)]

p3 <- ggplot(dt_mortgage, aes(x = Year, y = `Property_Owner with mortgage`, 
                              color = Country, group = Country)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  labs(title = "Property Owner with Mortgage - Evropa", x = "Rok", y = "%") +
  theme_minimal()
print(p3)

# Infrastructure Quality
dt_infra <- dt[Country %in% european_countries & !is.na(`Infrastructure quality`)]
dt_infra[, `Infrastructure quality` := as.numeric(`Infrastructure quality`)]
dt_infra[, Year := as.numeric(Year)]
dt_infra <- dt_infra[!is.na(`Infrastructure quality`) & !is.na(Year)]

p4 <- ggplot(dt_infra, aes(x = Year, y = `Infrastructure quality`, 
                           color = Country, group = Country)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  labs(title = "Infrastructure Quality - Evropa", x = "Rok", y = "Score") +
  theme_minimal()
print(p4)

# GDP per capita
dt_gdp <- dt[Country %in% european_countries & !is.na(`GDP per capita`)]
dt_gdp[, `GDP per capita` := as.numeric(`GDP per capita`)]
dt_gdp[, Year := as.numeric(Year)]
dt_gdp <- dt_gdp[!is.na(`GDP per capita`) & !is.na(Year)]

p5 <- ggplot(dt_gdp, aes(x = Year, y = `GDP per capita`, 
                         color = Country, group = Country)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  labs(title = "GDP per capita (PPP) - Evropa", x = "Rok", y = "USD") +
  theme_minimal()
print(p5)

# Debt to GDP
dt_debt <- dt[Country %in% european_countries & !is.na(`debt_to_GDP`)]
dt_debt[, `debt_to_GDP` := as.numeric(`debt_to_GDP`)]
dt_debt[, Year := as.numeric(Year)]
dt_debt <- dt_debt[!is.na(`debt_to_GDP`) & !is.na(Year)]

p6 <- ggplot(dt_debt, aes(x = Year, y = `debt_to_GDP`, 
                          color = Country, group = Country)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  labs(title = "Debt to GDP - Evropa", x = "Rok", y = "%") +
  theme_minimal()
print(p6)

# Unemployment
dt_unemp <- dt[Country %in% european_countries & !is.na(`Unemployment`)]
dt_unemp[, `Unemployment` := as.numeric(`Unemployment`)]
dt_unemp[, Year := as.numeric(Year)]
dt_unemp <- dt_unemp[!is.na(`Unemployment`) & !is.na(Year)]

p7 <- ggplot(dt_unemp, aes(x = Year, y = `Unemployment`, 
                           color = Country, group = Country)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  labs(title = "Unemployment - Evropa", x = "Rok", y = "%") +
  theme_minimal()
print(p7)

# Fertility
dt_fert <- dt[Country %in% european_countries & !is.na(`Fertility`)]
dt_fert[, `Fertility` := as.numeric(`Fertility`)]
dt_fert[, Year := as.numeric(Year)]
dt_fert <- dt_fert[!is.na(`Fertility`) & !is.na(Year)]

p8 <- ggplot(dt_fert, aes(x = Year, y = `Fertility`, 
                          color = Country, group = Country)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  labs(title = "Fertility Rate - Evropa", x = "Rok", y = "Births per woman") +
  theme_minimal()
print(p8)

