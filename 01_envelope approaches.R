# -------------------------------------------------------------------------

### Modelagem de nicho ecológico: Teoria e prática 

# Algoritmos aula prática

# Luisa Maria Diele-Viegas & Thiago Cavalcante
# 
# -------------------------------------------------------------------------

#limpa a memoria do R
rm(list = ls())

#Carregando os pacotes
library(biomod2) 
library(rasterVis)

dev.off()
## Load the species and environmental datasets
mammals_data <- read.csv("data/tabular/species/mammals_and_bioclim_table.csv", row.names=1)
head(mammals_data)

#Vizualizando os dados de presença para a onça (Panthera onca)
level.plot(mammals_data$PantheraOnca, 
           XY=mammals_data[,c("X_WGS84", "Y_WGS84")], 
           color.gradient = "blue", 
           cex=0.3,show.scale=F, 
           title="Original data")


#Utilizando a função species range envelope (SRE) do pacote biomod2
#para reproduzir as análises do BIOCLIM
pred_BIOCLIM = sre(Response = mammals_data$PantheraOnca, 
                   Explanatory = mammals_data[,c("bio3", "bio4", "bio7")], 
                   NewData = mammals_data[,c("bio3", "bio4", "bio7")], 
                   Quant = 0)# the value defines the most extreme values for 
#each variable not to be taken into account for determining the tolerance 
#boundaries for the considered species.

pred_BIOCLIM_025 = sre(Response = mammals_data$PantheraOnca, 
                       Explanatory = mammals_data[,c("bio3", "bio4", "bio7")], 
                       NewData = mammals_data[,c("bio3", "bio4", "bio7")], 
                       Quant = 0.025)

pred_BIOCLIM_05 = sre(Response = mammals_data$PantheraOnca, 
                      Explanatory = mammals_data[,c("bio3", "bio4", "bio7")], 
                      NewData = mammals_data[,c("bio3", "bio4", "bio7")], 
                      Quant = 0.05)

dev.off()
par(mfrow=c(2,2))

level.plot(mammals_data$PantheraOnca, 
           XY=mammals_data[,c("X_WGS84", "Y_WGS84")], 
           color.gradient = "blue", 
           cex=0.3,show.scale=F, 
           title="Original data")

level.plot(pred_BIOCLIM, 
           XY=mammals_data[,c("X_WGS84", "Y_WGS84")], 
           color.gradient = "blue", 
           cex=0.3,show.scale=F, 
           title="BIOCLIM 100%")

level.plot(pred_BIOCLIM_025, 
           XY=mammals_data[,c("X_WGS84", "Y_WGS84")], 
           color.gradient = "blue", 
           cex=0.3,show.scale=F, 
           title="BIOCLIM 97.5%")

level.plot(pred_BIOCLIM_05, 
           XY=mammals_data[,c("X_WGS84", "Y_WGS84")], 
           color.gradient = "blue", 
           cex=0.3,show.scale=F, 
           title="BIOCLIM 95%")


##### Adicionando duas variável aos modelos (BIO 11 e 12) #####
#The more variables you put in, the more restrictive your model will be 
#(if non-colinear variables).
pred_BIOCLIM_v2 = sre(Response = mammals_data$PantheraOnca, 
                   Explanatory = mammals_data[,c("bio3", "bio4", "bio7", "bio11", "bio12")], 
                   NewData = mammals_data[,c("bio3", "bio4", "bio7", "bio11", "bio12")], 
                   Quant = 0)# the value defines the most extreme values for 
#each variable not to be taken into account for determining the tolerance 
#boundaries for the considered species.

pred_BIOCLIM_025_v2 = sre(Response = mammals_data$PantheraOnca, 
                       Explanatory = mammals_data[,c("bio3", "bio4", "bio7", "bio11", "bio12")], 
                       NewData = mammals_data[,c("bio3", "bio4", "bio7", "bio11", "bio12")], 
                       Quant = 0.025)

pred_BIOCLIM_05_v2 = sre(Response = mammals_data$PantheraOnca, 
                      Explanatory = mammals_data[,c("bio3", "bio4", "bio7", "bio11", "bio12")], 
                      NewData = mammals_data[,c("bio3", "bio4", "bio7", "bio11", "bio12")], 
                      Quant = 0.05)


dev.off()
par(mfrow=c(2,2))

level.plot(mammals_data$PantheraOnca, 
           XY=mammals_data[,c("X_WGS84", "Y_WGS84")], 
           color.gradient = "blue", 
           cex=0.3,show.scale=F, 
           title="Original data")

level.plot(pred_BIOCLIM_v2, 
           XY=mammals_data[,c("X_WGS84", "Y_WGS84")], 
           color.gradient = "blue", 
           cex=0.3,show.scale=F, 
           title="BIOCLIM 100% (5 variáveis)")

level.plot(pred_BIOCLIM_025_v2, 
           XY=mammals_data[,c("X_WGS84", "Y_WGS84")], 
           color.gradient = "blue", 
           cex=0.3,show.scale=F, 
           title="BIOCLIM 97.5% (5 variáveis)")

level.plot(pred_BIOCLIM_05_v2, 
           XY=mammals_data[,c("X_WGS84", "Y_WGS84")], 
           color.gradient = "blue", 
           cex=0.3,show.scale=F, 
           title="BIOCLIM 95% (5 variáveis)")

#Comparação 
dev.off()
par(mfrow=c(1,2))

level.plot(pred_BIOCLIM_05, 
           XY=mammals_data[,c("X_WGS84", "Y_WGS84")], 
           color.gradient = "blue", 
           cex=0.3,show.scale=F, 
           title="BIOCLIM 95% (3 variáveis climáticas)")


level.plot(pred_BIOCLIM_05_v2, 
           XY=mammals_data[,c("X_WGS84", "Y_WGS84")], 
           color.gradient = "blue", 
           cex=0.3,show.scale=F, 
           title="BIOCLIM 95% (5 variáveis climáticas)")

