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
install.packages("adehabitatHS")
install.packages("pROC")
install.packages("rasterize")


#carregando os pacotes
library(biomod2)
library(mapview)
library(raster)
library(adehabitatHS)
library(pROC)
library(rasterize)

#carregando os dados de ocorrência e climático para Lagothrix flavicauda
#Ver detalhes no arquivo PDF Metadados no diretório principal
dataframe <- read.csv("data/dataframe.csv", stringsAsFactors = FALSE)
View(dataframe)
names(dataframe)

#Carregando as variáveis ambientais para o Peru
peru_stack <-
  stack(paste0("data/peru_predictors_stack.gri"))

#Preparando os dados

set.seed(2121)

# Amostrando 1000 localidades aleatórias dentro da nossa área de estudo (Peru)
background <- sampleRandom(peru_stack, size=1000, cells=FALSE, sp=TRUE)
??sampleRandom

#visualizando
par(mfrow=c(1,1))
plot(peru_stack[[1]])
plot(background, add = TRUE)

class(background)#checando a classe do objeto

#Adicionando a coluna com o valor 0 (ausência/background)
background <- as.data.frame(background)
background$occ <- 0
background$species <- "Lagothrix.flavicauda"
head(background)

#renomeando as colunas
names(background)[20] <- 'lon'
names(background)[21] <- 'lat'

head(background)

#padronizando as casas decimais entre os dataframes
head(dataframe)
x <- round(dataframe[,c(4:22)], digits=0)
y <- dataframe[,c(1:3)]
dataframe <- cbind(x,y)

#adicionando a coluna occ (presença)
dataframe$occ <- 1

head(dataframe)
head(background)

View(dataframe)
View(background)

#Combindando os dois dataframes
new_dataframe <- rbind(dataframe, background)

head(new_dataframe)
View(new_dataframe)

#checando as dimensões da tabela
dim(new_dataframe)

# # E agora...
## Como diz a Luisa
# Que comecem os jogos!

##### ENFA: Ecological Niche Factor Analysis #####

#ENFA inicia aplicando uma análise de PCA nas variáveis ambientais para
#encontrar os eixos principais de variação:

#explorando a função
??dudi.pca

pc <- dudi.pca(new_dataframe[,c(4:7)],scannf = FALSE, nf = 3)


#Agora os dados estão prontos para serem usados no método ENFA:
??enfa

en <- enfa(pc, new_dataframe$occ, scannf = FALSE)

#O plot scatterniche representa o ambiente utilizado pela espécie em relação
#ao disponivel no ambiente "global" na nossa área de estudo (neste caso, o PEru)
scatterniche(en$li, new_dataframe$occ, pts = TRUE)


#Outras produtos importantes das análises de ENFA:
s.arrow(cor(pc$tab, en$li))#PCA biplot
scatter(en)
en#Enfa object


#Projetando de volta para o espaço geográfico
level.plot(new_dataframe$occ, XY = new_dataframe[,c("lon","lat")],
           color.gradient = "red", cex = 0.5, show.scale = FALSE,
           title = "Dados originais")

level.plot(en$li[,1], XY = new_dataframe[,c("lon","lat")],
           color.gradient = "red", cex = 0.5, show.scale = FALSE,
           title = "ENFA")


#transformando essas informações para o formato raster para "melhorar" a
#visualização
coords <- new_dataframe[,c("lon","lat")]
coords$suit <- en$li[,1]
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
r_data <- rasterize(x=df[, 1:2], # dados lon-lat
                    y=r_obj, # raster vazio
                    field=df[, 3], # valors para preencher o raster
                    fun=mean) # função para agregar pela média


plot(r_data)

#Transformando o dataframe em um objeto espacial para visualização
sp <- SpatialPoints(cbind(dataframe$lon, dataframe$lat),
                    proj4string = CRS("+init=epsg:4326"))

#palette de cores
pal = mapviewPalette("mapviewSpectralColors")

mapview(r_data, alpha = 0.8, col.regions = pal,
        layer.name = "Adequabilidade do Habitat") +
  mapview(sp)


#Transformando em mapa binário
roc_enfa <- roc(new_dataframe$occ, en$li[,1])#Aqui nós usamos uma função do
#pacote pRoc que equilibra a porcentagem dos dados de presença e background
#assumindo eue o background representa áreas não adequadas para a espécie
threshold_enfa <- coords(roc_enfa, "best", ret = c("threshold"))
threshold_enfa

pred <-  r_data > 0.7658827
plot(pred)
