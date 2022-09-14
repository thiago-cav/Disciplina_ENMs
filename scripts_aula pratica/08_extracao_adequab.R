# -------------------------------------------------------------------------
### Modelagem de nicho ecológico: Teoria e prática 
# Ajuste para extracao de % de presenca ausencia dentro de um poligono

# Prof. Luisa Maria Diele-Viegas 
# -------------------------------------------------------------------------

# limpa a memoria do R
rm(list = ls())

library(dplyr)
library(raster)
library(rgdal)
library(sp)

# diretorio de trabalho

path <- "G:/Meu Drive/Servicos Academicos/Aulas Particulares/Scripts"
setwd(path)
dir()


# Define o poligono que sera utilizado para extrair valores de adequabilidade.
# a área de interesse utilizada foi o Brasil.

br <- rnaturalearth::ne_countries(country = "Brazil", returnclass = "sf")

setwd(path)
setwd("05_consenso")

# importando o raster de consenso
ens <- dir(pattern = "presente", recursive = TRUE) %>% 
  stringr::str_subset(".tif$") %>% 
  raster::stack()


# AJUSTE DO RASTER PARA ESCALA 0-1

objeto <- ens[[1]]/maxValue(ens[[1]])


# definindo valores de adequabilidade termica dentro do poligono.
# no caso do exemplo, estamos recuperando os valores por pixel 
adeq_present_mean<-raster::extract(objeto, br, fun=mean) # fun=SD 
adeq_present<-raster::extract(objeto, br) 



setwd(path)
setwd("07_maps")

#salva os valores em csv
write.csv(adeq_present, file=paste0("adequability.csv"), sep=",")
write.csv(adeq_present_mean, file=paste0("adequability_mean.csv"), sep=",")



