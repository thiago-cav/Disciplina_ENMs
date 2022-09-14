# -------------------------------------------------------------------------
### Modelagem de nicho ecológico: Teoria e prática 
# algoritmos

# Prof. Luisa Maria Diele-Viegas 
# -------------------------------------------------------------------------

# # limpa a memoria do R
rm(list = ls())

# pacotes
library(beepr)
library(dismo)
library(kernlab)
library(raster)
library(rgdal)
library(rnaturalearth)
library(sf)
library(tidyverse)


# diretorio de trabalho
path <- "G:/Meu Drive/Servicos Academicos/Aulas Particulares/Scripts"
setwd(path)
dir()

# importando os dados
# aqui vamos utilizar a planilha gerada no ultimo script, com os dados filtrados

setwd("02_occ")
occ <- readr::read_csv("occ_spocc_filtros_taxonomico_data_espatial_oppc.csv")
occ


# aqui a gente importa os rasters climáticos
setwd(path); setwd("03_var")
var <- dir(pattern = "tif$") %>% 
  raster::stack() %>% 
  raster::brick()
var

landscapetools::show_landscape(var$raster_br_res05_present_5km2_LA_bio_15 ) +
  geom_polygon(data = var$raster_br_res05_present_5km2_LA_bio_15 %>% raster::rasterToPolygons() %>% fortify, 
               aes(x = long, y = lat, group = group), fill = NA, color = "black", size = .1) +
  theme(legend.position = "none")


# # E agora...
# Que comecem os jogos! 

# diretorio de trabalho
setwd(path)
dir.create("04_presente")


# definindo os parametros a priori
replica <- 10 #numeros de replicas que serao usadas no modelo
partition <- .7 #definindo já aqui que serao 70% treino, 30% teste!

# algoritmos = vamos usar um loop para rodar todos de uma vez!
for(i in occ$species %>% unique){ # para cada especie
  setwd(path);  setwd("04_presente")
  # diretorio
  dir.create(i)
  setwd(i)
  
  # informacao importante!
  paste0("Vamos rodar essa budega! ", i, " in ", getwd()) %>% print
  
  # objeto para avaliacao 
  eval_species <- tibble::tibble()
  
  # selecionando dados de presenca e (pseudo)ausencia 
  # dados de presenca
  pr_specie <- occ %>% 
    dplyr::filter(species == i) %>% 
    dplyr::select(lon, lat) %>% 
    dplyr::mutate(id = seq(nrow(.)))
  
  # dados de pseudoausencia 
  pa_specie <- dismo::randomPoints(mask = var, n = nrow(pr_specie)) %>% 
    tibble::as_tibble() %>%
    dplyr::rename(lon = x, lat = y) %>% 
    dplyr::mutate(id = seq(nrow(.)))
  
  
  pa_specie_glm <- dismo::randomPoints(mask = var, n = 10000) %>% 
    tibble::as_tibble() %>%
    dplyr::rename(lon = x, lat = y) %>% 
    dplyr::mutate(id = seq(nrow(.)))
  
  
  # mudando o diretorio de trabalho novamente
  dir.create("00_replicas")
  setwd("00_replicas")
  
  # replicas
  for(r in replica %>% seq){	# numero de replicas do modelo 
    
    # objeto para a avaliacao 
    eval_algorithm <- tibble::tibble()
    
    # particionando os dados com base na nossa selecao 
    #(70% treino, 30% teste)
    
    # dados de presenca
    pr_sample_train <- pr_specie %>% 
      dplyr::sample_frac(partition) %>% 
      dplyr::select(id) %>% 
      dplyr::pull()
    
    # dados de pseudo ausencia
    pa_sample_train <- pa_specie %>% 
      dplyr::sample_frac(partition) %>% 
      dplyr::select(id) %>% 
      dplyr::pull()
    
    # dados de pseudo ausencia
    pa_sample_train_glm <- pa_specie_glm %>% 
      dplyr::sample_frac(partition) %>% 
      dplyr::select(id) %>% 
      dplyr::pull()
    gc()
    # dados de treino e teste !
    # treino
    train <- dismo::prepareData(x = var, 
                                p = pr_specie %>% 
                                  dplyr::filter(id %in% pr_sample_train) %>% 
                                  dplyr::select(lon, lat), 
                                b = pa_specie %>% 
                                  dplyr::filter(id %in% pa_sample_train) %>% 
                                  dplyr::select(lon, lat)) %>% na.omit
    
    # teste
    test <- dismo::prepareData(x = var, 
                               p = pr_specie %>% 
                                 dplyr::filter(!id %in% pr_sample_train) %>% 
                                 dplyr::select(lon, lat), 
                               b = pa_specie %>% 
                                 dplyr::filter(!id %in% pa_sample_train) %>% 
                                 dplyr::select(lon, lat)) %>% na.omit
    
    
    train_glm <- dismo::prepareData(x = var, 
                                    p = pr_specie %>% 
                                      dplyr::filter(id %in% pr_sample_train) %>% 
                                      dplyr::select(lon, lat), 
                                    b = pa_specie %>% 
                                      dplyr::filter(id %in% pa_sample_train_glm) %>% 
                                      dplyr::select(lon, lat)) %>% na.omit
    
    # teste
    test_glm <- dismo::prepareData(x = var, 
                                   p = pr_specie %>% 
                                     dplyr::filter(!id %in% pr_sample_train) %>% 
                                     dplyr::select(lon, lat), 
                                   b = pa_specie %>% 
                                     dplyr::filter(!id %in% pa_sample_train_glm) %>% 
                                     dplyr::select(lon, lat)) %>% na.omit
    
    gc()
    
    # Ajuste do modelo
    # informacao
    print(paste("Models fitting to", i, "replica", r, "of", replica))
    
    # Agora vamos de diferentes algoritmos!
    
    # Envelope climatico - Somente dados de presenca!
    BIO <- dismo::bioclim(x = train %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb))
    
    # Distancia ambiental - somente dados de presenca!
    # DOM <- dismo::domain(x = train %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb))
    # MAH <- dismo::mahal(x = train %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb))
    
    # Regressao - Modelo linear generalizado - dados de presenca e (pseudo)ausencia! 
    GLM <- glm(formula = pb ~ ., family = binomial(link = "logit"), data = train_glm) 
    #family=c("binomial","gaussian","poisson")
    
    # SVM (machine learning) - Dados de presença e plano de fundo
    SVM <- kernlab::ksvm(x = pb ~ ., data = train)
    # Lista com todos os algoritmos :)
    fit <- list(bioclim = BIO,  glm = GLM, svm = SVM) # domain = DOM, mahalanobis = MAH #
    gc()
    # Previsoes (mais uma vez no loop)
    for(a in seq(fit)){
      
      # informacao 
      print(paste("Model predict algorithm", fit[a] %>% names))
      
      # previsao do modelo 
      model_predict <- dismo::predict(var, fit[[a]], progress = "text")
      
      # exporta os valores da previsao do modelo 
      raster::writeRaster(x = model_predict, 
                          filename = paste0("enm_", i, "_", fit[a] %>% names,
                                            "_r", ifelse(r < 10, paste0("0", r), r)), 
                          format = "GTiff", 
                          options = c("COMPRESS=DEFLATE"), 
                          overwrite = TRUE)
      
      # avaliacao do modelo
      eval <- dismo::evaluate(p = test %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb), 
                              a = test %>% dplyr::filter(pb == 0) %>% dplyr::select(-pb), 
                              model = fit[[a]])
      gc()
      # indices de avaliacao 
      id_eval_spec_sens <- which(eval@t == dismo::threshold(eval, "spec_sens"))
      tss_spec_sens <- eval@TPR[id_eval_spec_sens] + eval@TNR[id_eval_spec_sens] - 1
      
      # dados da avaliacao 
      eval_data <- tibble::tibble(species = i, 
                                  replica = r, 
                                  algorithm = fit[a] %>% names, 
                                  thr_max_spec_sens = dismo::threshold(eval, "spec_sens"),
                                  tss_spec_sens = tss_spec_sens,
                                  auc = eval@auc, 
                                  file = paste0("enm_", i, "_", fit[a] %>% names,
                                                "_r", ifelse(r < 10, 
                                                             paste0("0", r), r), ".tif"))
      
      # combina a avaliacao dos modelos 
      eval_algorithm <- dplyr::bind_rows(eval_algorithm, eval_data)
      
    } 
    
    # combina as avaliacoes
    eval_species <- dplyr::bind_rows(eval_species, eval_algorithm)
    
  }
  
  # exporta as avaliacoes numa nova pasta 
  setwd("..")
  
  dir.create("01_evaluation")
  setwd("01_evaluation")
  dir.create("00_raw")
  setwd("00_raw")
  
  readr::write_csv(eval_species, paste0("eval_", i, ".csv"))
  
  # diretorio de trabalho
  setwd(".."); setwd(".."); setwd("..") 
  
  # notifica que a analise acabou porque nao somos obrigados a ficar conferindo toda hora :)
  #beepr::beep(8)
  
} 

# Arrasamos em dobro! Fim :)


