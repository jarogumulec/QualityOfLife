
# "QUALITYOFLIFE" 
# 

# prerequisities ---------------

library("gplots")
library(readxl)
library("heatmap.plus")



# data load  ---------------

# sorking directory - modify accordingly
setwd("C:/Users/admin/OneDrive - MUNI/Kody/qualityoflife")


qualityoflife_country_compare_2021 <- read_excel("qualityoflife_country_compare_2021.xlsx")

# alternativne : more is better variant
# qualityoflife_country_compare_2021 <- read_excel("qualityoflife_country_compare_2021.xlsx",
#                                                 sheet = "morebetter")


# heatmap --------------------------------

# legend = paste(log2fc_fadu$primary.culture, exprese_kokultivace_indirectonly$cisplatin_irrad, exprese_kokultivace_indirectonly$affected_by, sep=" ")
legend = paste(qualityoflife_country_compare_2021$Zemì, sep=" ")


scaled <- scale(qualityoflife_country_compare_2021[,2:ncol(qualityoflife_country_compare_2021)])
input <-  as.matrix(t(scaled))


heatmap.2(input, trace="none", density="none",  col=bluered(255),
          scale="none",   Colv = T, dendrogram = "both",
          labCol = legend, key = F,  
          keysize = 1, margins = c(6,15))

# bez prirody a bez duplicity ceny mzda --------------------------------

scaled <- scale(qualityoflife_country_compare_2021[,c(2:5, 7:9)])
input <-  as.matrix(t(scaled))


heatmap.2(input, trace="none", density="none",  col=bluered(255),
          scale="none",   Colv = T, dendrogram = "both",
          labCol = legend, key = F,  
          keysize = 1)

# correlation heatmap ----------------------------

# as 