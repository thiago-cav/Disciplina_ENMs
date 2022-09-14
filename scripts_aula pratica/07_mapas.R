# -------------------------------------------------------------------------
### Modelagem de nicho ecológico: Teoria e prática 
# mapas! 

# Prof. Luisa Maria Diele-Viegas 
# -------------------------------------------------------------------------

# limpa a memoria do R
rm(list = ls())

# pacotes 
library(ggspatial)
library(raster)
library(rgdal)
library(rnaturalearth)
library(tidyverse)

# diretorio de trabalho

path <- "G:/Meu Drive/Servicos Academicos/Aulas Particulares/Scripts"
setwd(path)
dir()


# importa os dados de distribuicao filtrados 
setwd("02_occ")
occ <- readr::read_csv("occ_spocc_filtros_taxonomico_data_espatial_oppc.csv")
occ

# Define nosso shapefile
# o shapefile disponibilizado e dos biomas brasileiros. voce pode usar o que preferir. 

br <- rnaturalearth::ne_countries(country = "Brazil", returnclass = "sf")
br
ggplot(br) + geom_sf() + theme_bw()

# nomeia a priori os plots que serao gerados 
na <- c("presente", "limite minimo", "p30", "p50")
na

# Plotando os graficos!

for(i in occ$species %>% unique){
  
  # informacao 
  sp <- i %>% stringr::str_to_title() %>% stringr::str_replace("_", " ")
  print(sp)
  
  # consenso dos modelos 
  setwd(path)
  
  
  # importando os dados de consenso
  ens <- dir(pattern = "presente_", recursive = TRUE) %>% 
    stringr::str_subset(".tif$") %>% 
    raster::stack()
  
  # criando o diretorio onde os mapas serao salvos
  setwd(path)
  dir.create("07_maps")
  setwd("07_maps")
  
  # gerando os mapas!
      for(j in ens %>% raster::nlayers() %>% seq){

          map <- ggplot() +
          geom_raster(data = raster::rasterToPoints(ens[[j]]) %>% tibble::as_tibble() %>% 
                        dplyr::rename(ens = names(ens[[j]])),
                      aes(x, y, fill = ens)) +
          geom_sf(data = br, fill = NA, color = "gray30") +
          geom_point(data = occ, aes(lon, lat, group = species %>%  str_to_title() %>% sub("_", " ", .)), 
                     size = .8, alpha = .7) +
          scale_color_manual(values = "black", guide = guide_legend(order = 1)) +
          scale_fill_gradientn(colours = rev(wesanderson::wes_palette("Zissou1", n = 100, type = "continuous"))) +
          # coord_sf(xlim = c(-90, -25)) +
          labs(x = "Longitude", y = "Latitude", fill = "Adequabilidade", color = "Ocorrencia", 
               title = bquote(bold(bolditalic(.(sp)) - .(na[j])))) +
          annotation_scale(location = "br", width_hint = .3) +
          annotation_north_arrow(location = "br", which_north = "true", 
                                 pad_x = unit(0, "cm"), pad_y = unit(.8, "cm"),
                                 style = north_arrow_fancy_orienteering) +
          theme_bw() +
          theme(title = element_text(size = 12, face = "bold"),
                legend.title = element_text(size = 8, face = "bold"),
                legend.background = element_rect(fill = "white",
                                                 size = 0.3, 
                                                 linetype = "solid", 
                                                 colour = "black"),
                axis.title = element_text(size = 15, face = "plain"))#,
                #legend.position = c(.1, .15))
        map
        
        # exportando os mapas
        ggsave(paste0("present_species",names(ens[[j]]), ".tiff"), map, wi = 20, he = 20, un = "cm", dpi = 300, comp = "lzw")
        
      }
    
  }
  

### Fim!