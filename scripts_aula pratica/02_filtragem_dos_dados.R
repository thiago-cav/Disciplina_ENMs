# -------------------------------------------------------------------------
### Modelagem de nicho ecológico: Teoria e prática 
# Limpeza/filtragem dos dados 

# Prof. Luisa Maria Diele-Viegas 
# -------------------------------------------------------------------------


#limpa a memoria do R
  rm(list = ls())
  
  library(spocc)
  library(spThin)
  library(dismo)
  library(rgeos)
  library(ENMeval)
  library(wallace)
  library(ggplot2)
  source(system.file('shiny/funcs', 'functions.R', package = 'wallace'))
  

  
  # estabelece o diretorio de trabalho
path <- "G:/Meu Drive/Servicos Academicos/Aulas Particulares/Scripts"
setwd(path)
  dir()
  # seleciona a planilha com a ocorrencia das especies
  results <- read.csv("all_data.csv", header=T)
  summary(results)
  
  # exclui linhas onde Lat ou Lon estao ausentes (NA)
  occ_data_na <- results %>% 
    tidyr::drop_na(lon, lat)
  summary(occ_data_na)
  
# limpeza de coordenadas! 
  # primeiro marca os pontos problematicos 
  flags_spatial <- CoordinateCleaner::clean_coordinates(
    x = occ_data_na, 
    species = "species",
    lon = "lon", 
    lat = "lat",
    tests = c("capitals", # 10km raio ao redor de capitais
              "centroids", # 1km raio ao redor de centroides de paises e provincias
              "duplicates", # duplicatas
              "equal", # coordenadas iguais
              "gbif", # raio ao redor da sede da GBIF
              "institutions", # raio ao redor de instituicoes de pesquisa em biodiversidade
              "seas", # pontos no mar
              "urban", # pontos dentro de areas urbanas
              "validity", # ponto de fora do sistema de coordenadas
              "zeros" # zeros e pontos onde lat = lon 
    )
  )
  
  # resultado da marcacao de 'pontos problematicos'
  #' TRUE = coordenadas 'limpas'
  #' FALSE = coordenadas potencialmente problematicas
  
  flags_spatial %>% head
  summary(flags_spatial)
  
  # excluir os pontos marcados como problematicos
  occ_data_tax_date_spa <- occ_data_na %>% 
    dplyr::filter(flags_spatial$.summary == TRUE)
  occ_data_tax_date_spa
  
  # resumo dos dados
  occ_data_na$species %>% table
  occ_data_tax_date_spa$species %>% table
  

  # 
  
  
  # definindo o diretorio onde os rasters estao 
  setwd(path); setwd("03_var")
  
  # importar os rasters 
  var_id <- raster::raster("raster_br_res05_present_5km2_LA_bio_2.tif")
  var_id
  
  var_id[!is.na(var_id)] <- raster::cellFromXY(var_id, raster::rasterToPoints(var_id)[, 1:2])
  landscapetools::show_landscape(var_id) +
    geom_polygon(data = var_id %>% raster::rasterToPolygons() %>% fortify, 
                 aes(x = long, y = lat, group = group), fill = NA, color = "black", size = .1) +
    theme(legend.position = "none")
  
  # associa os dados de distribuicao apos limpeza com os rasters climaticos 
  # selecionados e filtra a distribucao com base nos dados taxonomicos (sp)
  occ_data_tax_date_spa_oppc <- occ_data_tax_date_spa %>% 
    dplyr::mutate(oppc = raster::extract(var_id, dplyr::select(., lon, lat))) %>% 
    #dplyr::distinct(species, oppc, .keep_all = TRUE) %>%  
    dplyr::filter(!is.na(oppc)) %>% 
    dplyr::add_count(species) %>% 
    dplyr::arrange(species)
  occ_data_tax_date_spa_oppc
  
  # verifica 
  table(occ_data_tax_date_spa$species)
  table(occ_data_tax_date_spa_oppc$species)
  
  occ_data_tax_date_spa$species %>% table
  occ_data_tax_date_spa_oppc$species %>% table
  
  #exporta a planilha para uma nova pasta no diretorio de trabalho
  # cria a pasta a ser utilizada
  setwd(path)
  dir.create("02_occ")
  setwd("02_occ")
  
  # exporta a planilha em csv
  readr::write_csv(occ_data_tax_date_spa_oppc, paste0("occ_spocc_filtros_taxonomico_data_espatial_oppc.csv"))
  
  
  # Fim do script 2 :)