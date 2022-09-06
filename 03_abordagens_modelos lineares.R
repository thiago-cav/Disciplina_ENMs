# -------------------------------------------------------------------------

### Modelagem de nicho ecológico: Teoria e prática

# Algoritmos aula prática

# Luisa Maria Diele-Viegas & Thiago Cavalcante

# Scripts criados por Thiago Cavalcante
# Baseados em:
# Guisan, A., Thuiller, W., & Zimmermann, N. E. (2017).
# Habitat suitability and distribution models: with applications in R.
# United Kingdom: Cambridge University Press.
# -------------------------------------------------------------------------

#limpando a memória do R
rm(list = ls())

#instalando os pacotes para o exercício
install.packages("ggplot2")
install.packages("gam")

#carregando os pacotes
library(biomod2)
library(mapview)
library(raster)
library(ggplot2)
library(gam)

#carregando os dados que criamos no script 02 com a ocorrência e os valores
#das variáveis climáticas para Lagothrix flavicauda
new_dataframe <- read.csv("data/new_dataframe.csv", stringsAsFactors = FALSE)
head(new_dataframe)
dim(new_dataframe)


##### Modelos lineares generalizados #####
#Generalized Linear Models (GLMs)

#GLM1 assume uma relação linear com os preditores
glm1 <- glm(occ ~ 1 + bio3 + bio7 + bio11 + bio19,
          data = new_dataframe, family = "binomial")

#GLM2 assume relações quadráticas (i.e. não-simétricas, unimodais ou sigmoidais)
glm2 <- glm(occ ~ 1 + poly(bio3, 2) + poly(bio7, 2) + poly(bio11, 2) +
              poly(bio19, 2),
            data = new_dataframe, family = "binomial")

??poly

#Projetando de volta para o espaço geográfico

par(mfrow = c(1, 3))
#Dados originais
level.plot(new_dataframe$occ, XY = new_dataframe[,c("lon","lat")],
           color.gradient = "red", cex = 1.8, show.scale = FALSE,
           title = "Dados originais")

#Modelo 1
level.plot(fitted(glm1), XY = new_dataframe[,c("lon","lat")],
           color.gradient = "red", cex = 1.8, show.scale = FALSE,
           title = "Modelo 1")

#Modelo 2
level.plot(fitted(glm2), XY = new_dataframe[,c("lon","lat")],
           color.gradient = "red", cex = 1.8, show.scale = FALSE,
           title = "Modelo 2")


#transformando essas informações para o formato raster para "melhorar" a
#vizualização
##### GLM 1 #####
coords <- new_dataframe[,c("lon","lat")]
coords$suit <- fitted(glm1)
head(coords)

# will need to rename colnames for raster
colnames(coords) <- c('x', 'y', 'vals')

df<-coords

#Carregando as variáveis ambientais para o Peru
peru_stack <-
  stack(paste0("data/peru_predictors_stack.gri"))

#criando um raster vazio na extensão do Peru
r_obj <- raster(peru_stack[[1]])


#definindo a resolução
res(r_obj)
res(r_obj) <- 0.8

# use rasterize to create desired raster
r_glm1 <- rasterize(x=df[, 1:2], # dados lon-lat
                    y=r_obj, # raster vazio
                    field=df[, 3], # valors para preencher o raster
                    fun=mean) # função para agregar pela média

##### GLM 2 #####
coords <- new_dataframe[,c("lon","lat")]
coords$suit <- fitted(glm2)
head(coords)

# will need to rename colnames for raster
colnames(coords) <- c('x', 'y', 'vals')

df<-coords

#criando um raster vazio na extensão do Peru
r_obj <- raster(peru_stack[[1]])

#definindo a resolução
res(r_obj)
res(r_obj) <- 0.8

# use rasterize to create desired raster
r_glm2 <- rasterize(x=df[, 1:2], # dados lon-lat
                    y=r_obj, # raster vazio
                    field=df[, 3], # valors para preencher o raster
                    fun=mean) # função para agregar pela média

#Transformando o dataframe em um objeto espacial para vizualização
dataframe <- read.csv("data/dataframe.csv", stringsAsFactors = FALSE)
sp <- SpatialPoints(cbind(dataframe$lon, dataframe$lat),
                    proj4string = CRS("+init=epsg:4326"))

mapview(r_glm1, alpha = 0.5,
        layer.name = "GLM com funções lineares") +
  mapview(r_glm2, alpha = 0.5,
          layer.name = "GLM com funções quadráticas") +
  mapview(sp)


##### Curvas de resposta das espécies #####

## Obtendo as curvas de resposta
??response.plot2

rp <- response.plot2(models = c('glm1','glm2'),
                     Data = new_dataframe[,c("bio3", "bio7", "bio11", "bio19")],
                     show.variables = c("bio3",  "bio7", "bio11", "bio19"),
                     fixed.var.metric = 'mean', plot = FALSE,
                     use.formal.names = TRUE)


## Definindo um tema customizado para os plots
rp.gg.theme <- theme(legend.title = element_blank(),
                     axis.text.x = element_text(angle = 90, vjust = .5),
                     panel.background = element_rect(fill = NA, colour = "gray70"),
                     strip.background = element_rect(fill = NA, colour = "gray70"),
                     panel.grid.major = element_line(colour = "grey90"),
                     legend.key = element_rect(fill = NA, colour = "gray70"),
                     axis.title.y = element_text(size = 13))


## Plotando
gg.rp <- ggplot(rp, aes(x = expl.val, y = pred.val, lty = pred.name)) +
  geom_line() + ylab("Probabilidade de ocorrência") + xlab("") +
  rp.gg.theme +
  facet_grid(~ expl.name, scales = 'free_x')

print(gg.rp)


##### Modelo aditivos generalizados (GAM) #####
if(is.element("package:mgcv", search())) detach("package:mgcv")
## Confirmando que o pacote mgcv não está carregado para evitar conflitos

#formula
gam_mod1 = gam(occ ~ s(bio3,2) + s(bio7,2) + s(bio11,2) + s(bio19,2),
           data = new_dataframe, family="binomial")


#curvas de resposta
rp <- response.plot2(models = c('glm1','glm2','gam_mod1'),
                     Data = new_dataframe[,c("bio3", "bio7", "bio11", "bio19")],
                     show.variables = c("bio3",  "bio7", "bio11", "bio19"),
                     fixed.var.metric = 'mean', plot = FALSE,
                     use.formal.names = TRUE)


## Plotando
gg.rp <- ggplot(rp, aes(x = expl.val, y = pred.val, col = pred.name)) +
  geom_line(size=0.8) + ylab("Probabilidade de ocorrência") + xlab("") +
  rp.gg.theme +
  facet_grid(~ expl.name, scales = 'free_x')

print(gg.rp)


#projetando no espaço geográfico
par(mfrow = c(1, 2))
#GLM
level.plot(fitted(glm2), XY = new_dataframe[,c("lon","lat")],
           color.gradient = "grey", cex = 1.8, show.scale = FALSE,
           title = "GLM")

#GAM
level.plot(fitted(gam1), XY = new_dataframe[,c("lon","lat")],
           color.gradient = "grey", cex = 1.8, show.scale = FALSE,
           title = "GAM")



##### GAM 1 #####
coords <- new_dataframe[,c("lon","lat")]
coords$suit <- fitted(gam1)
head(coords)

# will need to rename colnames for raster
colnames(coords) <- c('x', 'y', 'vals')

df<-coords

#criando um raster vazio na extensão do Peru
r_obj <- raster(peru_stack[[1]])

#definindo a resolução
res(r_obj)
res(r_obj) <- 0.8

# use rasterize to create desired raster
r_gam1 <- rasterize(x=df[, 1:2], # dados lon-lat
                    y=r_obj, # raster vazio
                    field=df[, 3], # valors para preencher o raster
                    fun=mean) # função para agregar pela média


#Vizualização
mapview(r_glm1, alpha = 0.5,
        layer.name = "GLM com funções lineares") +
  mapview(r_glm2, alpha = 0.5,
          layer.name = "GLM com funções quadráticas") +
  mapview(r_gam1, alpha = 0.5,
          layer.name = "GAM") +
  mapview(sp)


#Bonus
#criando um ensemble model (da maneira mais simples possível) com o GAM e o GLM
stack <- raster::stack(r_glm2, r_gam1)
mean <- raster::calc(stack, fun = mean, na.rm = T)

mapview(r_glm1, alpha = 0.5,
        layer.name = "GLM com funções lineares") +
  mapview(r_glm2, alpha = 0.5,
          layer.name = "GLM com funções quadráticas") +
  mapview(r_gam1, alpha = 0.5,
          layer.name = "GAM") +
  mapview(mean, alpha = 0.5,
          layer.name = "Ensemble model")+
  mapview(sp)

