# -------------------------------------------------------------------------
### Modelagem de nicho ecológico: Teoria e prática 
# Download das variáveis abioticas

# Prof. Luisa Maria Diele-Viegas 
# -------------------------------------------------------------------------

#limpa a memoria do R
rm(list = ls())

# le os pacotes a sere utilizados
library(landscapetools)
library(psych)
library(raster)
library(rgdal)
library(rnaturalearth)
library(tidyverse)
library(wesanderson)

# estabelece o diretorio de trabalho
path <- "G:/Meu Drive/Servicos Academicos/Aulas Particulares/Scripts"

setwd(path)
dir()

# cria uma pasta nova no seu diretorio 
dir.create("03_var")
setwd("03_var")

# ajusta a extensao a ser trabalhada
# o shapefile disponibilizado e do Brasil. voce pode usar o que preferir. 
# readOGR()  função para ler um shapefile
# https://www.r-graph-gallery.com/

br <- rnaturalearth::ne_countries(country = "Brazil", returnclass = "sf")
br

plot(br)

# plot
ggplot() +
  geom_sf(data = br) +
  theme_bw()

# rasters climáticos: utilizaremos os bioclimaticos do worldclim

setwd("G:/Meu Drive/Servicos Academicos/Aulas Particulares/Scripts/00_var")
tif <- dir(pattern = "tif$")
tif

# importar os rasters e agrupar em um stack

var <- raster::stack(tif)
var

# plot para ver se ta tudo ok
plot(var$raster_br_res05_present_5km2_LA_bio_1)

# cortar o raster de acordo com a extensao que vamos trabalhar

var_br <- raster::crop(x = var, y = br) %>% 
  raster::mask(mask = br)
var_br

plot(var_br$raster_br_res05_present_5km2_LA_bio_1)

# ajuste da resolucao apos o corte

raster::res(var_br)[1] # grau decimal
raster::res(var_br)[1]/(30/3600) # km x km

# fator de agregacao 
#resolucao atual:
res_actual <- raster::res(var_br)[1]
res_actual
#resolucao que a gente quer:
res_adjust <- 0.5 # = 60km2
res_adjust


#fator de agregacao:
agg_fac <- res_adjust/res_actual
agg_fac

# agregacao 
var_br_05 <- raster::aggregate(var_br, fact = agg_fac)
var_br_05

# nova resolucao
raster::res(var_br_05)[1]
raster::res(var_br_05)[1]/(30/3600)

plot(var_br_05$present_5km2_LA_bio_1)
# plot da variavel bio_1 (= annual Mean Temperature)

landscapetools::show_landscape(var_br_05$raster_br_res05_present_5km2_LA_bio_1) +
  geom_polygon(data = var_br_05$raster_br_res05_present_5km2_LA_bio_1 %>% raster::rasterToPolygons() %>% fortify, 
               aes(x = long, y = lat, group = group), fill = NA, color = "black", size = .1) +
  theme(legend.position = "none")



setwd(path)

# salva o raster cortado e na nova resolução numa nova pasta 
dir.create("00_var")
setwd("00_var")
raster::writeRaster(x = var_br_05, 
                    filename = paste0("raster_br_res05_", names(var_br)), 
                    bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), 
                    format = "GTiff", 
                    overwrite = TRUE)

setwd("..")

dir(pattern = ".tif") %>% 
  unlink()

# Avaliar a correlação entre as variáveis climáticas 
# cria uma nova pasta para a análise de correlacao

dir.create("01_correlation") 
setwd("01_correlation")
getwd()

# extrai os valores
var_da <- var_br_05 %>% 
  raster::values() %>% 
  tibble::as_tibble() %>% 
  tidyr::drop_na()
var_da

# verifica se os dados estao corretos
head(var_da)
dim(var_da)

# correlacao - utilizaremos aqui o metodo de Spearman 
cor_table <- corrr::correlate(var_da, method = "spearman") 
cor_table
        
# cria uma tabela com o resultado da correlacao 
cor_table_summary <- cor_table %>% 
  corrr::shave() %>%
  corrr::fashion()
cor_table_summary

# e exporta a tabela 
readr::write_csv(cor_table_summary, "correlacao.csv")


# selecao das variaveis correlacionadas 
fi_06 <- cor_table %>% 
  corrr::as_matrix() %>% 
  caret::findCorrelation(cutoff = .75, names = TRUE, verbose = TRUE)
fi_06


var_da_cor06 <- var_da %>% 
  dplyr::select(-fi_06)
var_da_cor06

# verifica as variaveis correlacionadas
var_da_cor06 %>% 
  corrr::correlate(method = "spearman") %>% 
  corrr::as_matrix() %>% 
  caret::findCorrelation(cutoff = .75, names = TRUE, verbose = TRUE)

# grafico de correlacao 
tiff("correlacao_plot.tiff", wi = 30, he = 25, 
     un = "cm", 
     res = 300, 
     comp = "lzw")
pairs.panels(x = var_da_cor06 %>% dplyr::sample_n(1e3),
             method = "spearman",
             pch = 20, 
             ellipses = FALSE, 
             density = FALSE, 
             stars = TRUE, 
             hist.col = "gray",
             digits = 2,
             rug = FALSE,
             breaks = 10,
             ci = TRUE)
dev.off()

# a partir da analise feita, seleciona as variaveis que nao se correlacionam 
# para prosseguir com a modelagem


# diretorio de trabalho
setwd(path)
setwd("00_var")

#### copia as variaveis para a pasta var_03 ###
paste0("raster_br_res05_",colnames(var_da_cor06),".tif")%>%  
  file.copy(., "G:/Meu Drive/Servicos Academicos/Aulas Particulares/Scripts/03_var")


#### CONSIDERANDO OS RASTERS DO CMIP6 PARA O FUTURO ####
### PROBLEMA: ESTA ORGANIZADO EM BANDAS. POR ISSO, O CODIGO  PARA COPIAR O 
### RASTER DA PASTA 00 PARA A PASTA 03 NAO FUNCIONA DIREITO E OS RASTERS
### COPIADOS PARA A PASTA 03 NAO SAO OS SELECIONADOS PELA ANALISE DE CORRELACAO.
### PARA RESOLVER ISSO DE UMA MANEIRA RELATIVAMENTE RAPIDA, VOCE PODE
### SIMPLESMENTE SALVAR OS RASTERS SELECIONADOS UM A UM:

# setwd("03_var")
# writeRaster(var_br_05[[3]], "wc14_br_res05g_miroc_ssp370_2070_prec_3.tif")

# altere o número dentro dos colchetes de acordo com a posição da banda selecionada dentro do seu raster. para ver as posições, use:

# names(var_br_05)


# fim da parte 1 :)

