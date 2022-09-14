# -------------------------------------------------------------------------
### Modelagem de nicho ecológico: Teoria e prática 
# Binarizacao e limite de corte

# Prof. Luisa Maria Diele-Viegas 
# -------------------------------------------------------------------------

# limpa a memoria do R
rm(list = ls())

# pacotes
library(landscapetools)
library(raster)
library(rgdal)
library(tidyverse)

# diretorio de trabalho

path <- "G:/Meu Drive/Servicos Academicos/Aulas Particulares/Scripts"
setwd(path)
dir()



#importando os dados 
# dados de ocorrencia filtrados
setwd("02_occ")
occ <- readr::read_csv("occ_spocc_filtros_taxonomico_data_espatial_oppc.csv")
occ# binarizacao - limite de corte 
# criando o diretorio de trabalho
setwd(path); dir.create("06_consenso_thr"); setwd("06_consenso_thr")

for(i in occ$species %>% unique){
  
  # buscando os dados de consenso
  # informacao 
  print(paste0("Binarizate weighted average to ", i))
  
  # diretorio de trabalho dos dados de consenso
  setwd(path); setwd("05_consenso")
  
  # importando os dados gerados
  ens_w <- dir(pattern = ".tif$") %>% 
    raster::stack()
  
  # extraindo valores considerando a distribuicao 
  thrs <- occ %>% 
    dplyr::filter(species == i) %>% 
    dplyr::select(lon, lat) %>% 
    raster::extract(ens_w, .)
  
  # limites de corte - iremos considerar um limite mínimo >0, de 30% e de 50%
  li_thrs <- list(
    lpt = min(thrs[thrs > 0]),
    p30 = quantile(thrs[thrs > 0], .3),
    p50 = quantile(thrs[thrs > 0], .5)
  )
  
  # define o diretorio de trabalho
  setwd(path); setwd("06_consenso_thr")
  
  for(j in li_thrs %>% length %>% seq){
    
    # exporta os dados obtidos 
    raster::writeRaster(x = ens_w >= li_thrs[[j]], 
                        filename = paste0("presente_thr_", names(li_thrs)[j], "_", i), 
                        format = "GTiff", 
                        options = c("COMPRESS=DEFLATE"), 
                        overwrite = TRUE)
    
  }

}

getwd()
# Estamos quase la! :)