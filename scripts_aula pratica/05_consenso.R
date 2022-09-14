# -------------------------------------------------------------------------
### Modelagem de nicho ecológico: Teoria e prática 
# Consenso por media ponderada

# Prof. Luisa Maria Diele-Viegas 
# -------------------------------------------------------------------------

# limpa a memoria do R
rm(list = ls())

# pacotes
library(landscapetools)
library(raster)
library(rgdal)
library(tidyverse)
library(vegan)

# diretorio de trabalho 

path <- "G:/Meu Drive/Servicos Academicos/Aulas Particulares/Scripts"
setwd(path)
dir()

# importando as avaliacoes

setwd("04_presente")
# listando os arquivos das avaliacoes
csv <- dir(pattern = "eval_", recursive = TRUE)
csv

# importando os diferentes modelos
eva <- purrr::map_dfr(csv, readr::read_csv)
eva

# consenso por media ponderada
# Utilizaremos como limite inferior AUCs com valores de 0.8

auc_limit <- .75

# algoritmos
alg <- eva$algorithm %>% unique
alg
# fazendo o consenso
for(i in eva$species %>% unique){
  setwd(path)
  setwd("04_presente")
  # informacao
  print(paste("Consenso de", i))
  
  # selecao de modelos = somente aqueles com AUC maior ou igual a 0.8
  eva_i <- eva %>% 
    dplyr::filter(species == i, 
                  auc >= auc_limit, 
                  algorithm %in% alg)
  
  # importando os modelos
  enm <- eva_i %>% 
    dplyr::select(file) %>% 
    dplyr::mutate(file = paste0(i, "/00_replicas/", file)) %>% 
    dplyr::pull() %>% 
    raster::stack()
  
  # AUC
  auc <- eva_i %>% 
    dplyr::select(auc) %>% 
    dplyr::mutate(auc = (auc - .5) ^ 2) %>% 
    dplyr::pull()
  
  # padronizacao 
  print("Pode demorar... mas vai dar bom!")
  enm_st <- enm %>% 
    values %>% 
    vegan::decostand("range", na.rm = TRUE)
  print("Não disse? Sucesso!")

  # consenso da media ponderada 
  ens <- enm[[1]]
  ens[] <- apply(enm_st, 1, function(x){sum(x * auc) / sum(auc)})
  
  # diretorio de trabalho 
  setwd(path)
  dir.create("05_consenso")
  setwd("05_consenso")
  
  # exporta o ensemble 
  raster::writeRaster(x = ens, 
                      filename = paste0("presente_", i), 
                      format = "GTiff", 
                      options = c("COMPRESS=DEFLATE"), 
                      overwrite = TRUE)
  
} 
getwd()
# Fimmmm! :)
