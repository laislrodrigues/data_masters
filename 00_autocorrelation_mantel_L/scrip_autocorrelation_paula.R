##TESTANDO AUTOCORRELAÇÃO ESPACIAL DAS CÂMERAS CAETETUS

install.packages("ape") 
library(ape)
install.packages("ade4")
library(ade4)
data <- read.table("data_autocorrelation_test_2019_01_d23.txt", header = TRUE, sep = "\t")


#Para dados paramétricos - Moran's

traps_wlp_dists <- as.matrix(dist(cbind(data$long, data$lat)))
traps_wlp_dists_inv <- 1/traps_wlp_dists
diag(traps_wlp_dists_inv) <- 0
traps_wlp_dists_inv[1:5, 1:5]

#Detecção queixadas
Moran.I(data$det_wlp, traps_wlp_dists_inv)

#Detecção catetos
Moran.I(data$det_cp, traps_wlp_dists_inv)

#Para cutias
Moran.I(data$det_agouti, traps_wlp_dists_inv)

#para dados não paramétricos

#Para número de spp registradas por trap

traps_dists <- dist(cbind(data$long, data$lat))
spp_dist <- dist(data$n_spp)

biplot(mantel.rtest(traps_dists, spp_dist, nrepet = 9999))

#Ocorrências Queixadas
traps_dists <- dist(cbind(data$long, data$lat))
wlp_dist <- dist(data$wlp)

mantel.rtest(traps_dists, wlp_dist, nrepet = 9999)

#Ocorrências catetos

traps_dists <- dist(cbind(data$long, data$lat))
cp_dist <- dist(data$cp)

mantel.rtest(traps_dists, cp_dist, nrepet = 9999)

#Ocorrências cutias

traps_dists <- dist(cbind(data$long, data$lat))
agouti_dist <- dist(data$wlp)

mantel.rtest(traps_dists, agouti_dist, nrepet = 9999)

