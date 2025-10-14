
# "QUALITYOFLIFE" 
# 

# prerequisities ---------------

library("gplots")
library(readxl)
library(mixOmics)

install.packages("BiocManager") 
BiocManager::install('mixOmics')

# data load  ---------------

# sorking directory - modify accordingly
setwd("C:/Users/admin/OneDrive - MUNI/Kody/qualityoflife")
setwd("~/OneDrive - MUNI/Kody/QualityOfLife")

qualityoflife_country_compare_2021 <- read_excel("qualityoflife_country_compare_2021.xlsx")


# alternativne : more is better variant
# qualityoflife_country_compare_2021 <- read_excel("qualityoflife_country_compare_2021.xlsx",
#                                                 sheet = "morebetter")


# heatmap --------------------------------

# legend = paste(log2fc_fadu$primary.culture, exprese_kokultivace_indirectonly$cisplatin_irrad, exprese_kokultivace_indirectonly$affected_by, sep=" ")
#legend = paste(qualityoflife_country_compare_2021$Zem?, sep=" ")
legend = paste(qualityoflife_country_compare_2021$Země, sep=" ")
lgnd = paste(qualityoflife_country_compare_2021$Země, sep=" ")
#lgnd = paste(qualityoflife_country_compare_2021$Zem?, sep=" ")


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

# PCA
principalcomp <- pca(qualityoflife_country_compare_2021[,2:ncol(qualityoflife_country_compare_2021)]
, ncomp = 2, center = T, scale = T)

plotIndiv(principalcomp, comp = c(1, 2), 
          ind.names = lgnd,
          legend = F, title = 'Quality of life, PCA')

plotVar(principalcomp, comp = c(1, 2), cex = 2.1,
          legend = F, title = 'Quality of life, PCA')

#biplot(principalcomp, cex = 0.7, xlabs = lgnd)

# identiciface jak dulezite ty faktory jsou
# scree plot přímo z mixOmics
plotLoadings(principalcomp, comp = 1, ndisplay = 10)
plotLoadings(principalcomp, comp = 2, ndisplay = 10)

# as 