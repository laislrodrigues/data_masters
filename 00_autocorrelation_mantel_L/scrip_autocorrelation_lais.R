## TESTANDO AUTOCORRELACAO ESPACIAL - RIQUEZA NATIVAS ENTRE AREAS ## 

install.packages("vegan")
library(vegan)
install.packages("ape") 
library(ape)
install.packages("ade4")
library(ade4)

setwd("C:/Users/Lais/Google Drive/3. MESTRADO 2017-2019/3. PROJETO/4. Analises/00_Analises_2019/00_autocorrelation_mantel_L")

getwd()

data <- read.table("table_mantel_rich_total.csv", header = TRUE, sep = ";")
data

#Para dados parametricos - Moran's


richness.dists <- as.matrix(dist(cbind(data$Long, data$Lat)))
richness.dists

richness.dists.inv <- 1/richness.dists
richness.dists.inv
diag(richness.dists.inv) <- 0

richness.dists.inv[1:5, 1:5]


Moran.I(data$Richness_total, richness.dists.inv, scaled = TRUE)

#____________________________________________________________________________#

# Mantel Test:

install.packages("vegan")
library(vegan)
install.packages("ape") 
library(ape)
install.packages("ade4")
library(ade4)

setwd("C:/Users/Lais/Google Drive/3. MESTRADO 2017-2019/3. PROJETO/4. Analises/00_Analises_2019/00_autocorrelation_mantel_L")

getwd()

data2 <- read.table("table_mantel_rich_total.csv", header = TRUE, sep = ";")
head(data2, n=20)

areas.dists <- dist(cbind(data2$Long, data2$Lat))
areas.dists

richness.dists2 <- dist(data2$Richness_total)
richness.dists2


as.matrix(areas.dists)[1:5, 1:5]
as.matrix(richness.dists2)[1:5, 1:5]

mantel.rtest(areas.dists, richness.dists2, nrepet = 9999)

plot(results <- mantel.rtest(areas.dists, richness.dists2), main = "Mantel's test")

results


#_____________________________________________________________________________________#
