# -------------------------------------------------------------------------

### Modelagem de nicho ecológico: Teoria e prática

# Algoritmos aula prática

# Luisa Maria Diele-Viegas & Thiago Cavalcante
#
# -------------------------------------------------------------------------

#limpando a memória do R
rm(list = ls())

#instalando os pacotes para o exercício
install.packages("biomod2")
install.packages("mapview")
install.packages("raster")

#carregando os pacotes
library(biomod2)
library(mapview)
library(raster)

#carregando os dados de ocorrência de Lagothrix flavicauda
#(ver Figura 1 e Metadados na pasta)
spdat <- read.csv("lagothrix_flavicauda.csv", stringsAsFactors = FALSE)
head(spdat)

#Carregando as variáveis ambientais para o Peru
peru_stack <-
  stack(paste0("data/peru_predictors_stack.gri"))

head(spdat)

#Transformando o dataframe em um objeto espacial para vizualização
sp <- SpatialPoints(cbind(spdat$lon, spdat$lat),
                    proj4string = CRS("+init=epsg:4326"))

#Plot das variáveis ambientais
plot(peru_stack[[1:6]])
#ver https://www.worldclim.org/data/bioclim.html para detalhes

#Conferindo tudo
#selecionando o shapefile 1 do raster stack (Bio1)
plot(peru_stack[[1]], main = "BIO1 + Ocorrências")
plot(sp, add=TRUE, col="red", pch =1, cex = 1.5)

#Estruturando os dados para usar a função "sre" do pacote biomod2
#Essa função reproduz a análise do BIOCLIM original

#Nós vamos usar as nossas variáveis para criar um raster onde todas as celulas
#que tem occorrência da espécie possuam o valor 1 (presença) e todo o restante
#seja considerado 0 (ausência). Esta será nossa variável resposta (response).

#detalhes:
??sre

#Vamos também selecionar 3 variáveis climáticas que nós consideramos que podem
#afetar a distribuição da nossa espécie
#Exemplo:
#BIO1 = Annual Mean Temperature
#BIO12 = Annual Precipitation

#Selecionando somente as variáveis que estamos interessados
names(peru_stack)
myExpl <- peru_stack[[c("bio1", "bio12")]]

#selecionando as colunas com as coordenadas
myRespXY <- spdat[c("lon","lat")]

#criando o mapa de presença e "ausência" da espécie
#reclassificando todas as células do raster do Peru para 0
myResp <-
  raster::reclassify(
    subset(myExpl, 1, drop = TRUE), c(-Inf, Inf, 0)
  )

#Dando o valor de 1 (presença) para as localidades onde a espécie ocorre
myResp[cellFromXY(myResp,myRespXY)] <- 1

#add our points
plot(myResp)
points(myRespXY, col = "red")

#Utilizando a função species range envelope (SRE) do pacote biomod2
#para reproduzir as análises do BIOCLIM
sre.100 <-
  sre(
    Response = myResp,
    Explanatory = myExpl,
    NewData=myExpl,
    Quant = 0
  )


sre.095 <-
  sre(
    Response = myResp,
    Explanatory = myExpl,
    NewData=myExpl,
    Quant = 0.025
  )# the value defines the most extreme values for
#each variable not to be taken into account for determining the tolerance
#boundaries for the considered species.


sre.090 <-
  sre(
    Response = myResp,
    Explanatory = myExpl,
    NewData=myExpl,
    Quant = 0.05
  )


dev.off()
##' visualise results
par(mfrow=c(2,2),mar=c(6, 5, 5, 3))
plot(myResp, main = "Occurrence points")
plot(sp, add=TRUE, col="red", pch =1, cex = 1.5)
plot(sre.100, main="BIOCLIM 100%")
plot(sre.095, main="BIOCLIM 97.5%")
plot(sre.090, main="BIOCLIM 95%")

mapview(sre.100)+
  mapview(sp)

#Vamos repetir o processo adicionando uma variável
#BIO4 = Temperature Seasonality (standard deviation ×100)
#BIO9 = Mean Temperature of Driest Quarter
#BIO14 = Precipitation of Driest Month
#Selecionando somente as variáveis que estamos interessados
names(peru_stack)
myExpl <- peru_stack[[c("bio1","bio4","bio9", "bio12", "bio14")]]

#selecionando as colunas com as coordenadas
myRespXY <- spdat[c("lon","lat")]

#converting the study area to zero (abscence)
#reclassificando todas as células do raster do Peru para 0
myResp <-
  raster::reclassify(
    subset(myExpl, 1, drop = TRUE), c(-Inf, Inf, 0)
  )

#Dando o valor de 1 (presença) para as localidades onde a espécie ocorre
myResp[cellFromXY(myResp,myRespXY)] <- 1

#add our points
plot(myResp)
points(myRespXY, col = "red")

#Utilizando a função species range envelope (SRE) do pacote biomod2
#para reproduzir as análises do BIOCLIM
sre.100_v2 <-
  sre(
    Response = myResp,
    Explanatory = myExpl,
    NewData=myExpl,
    Quant = 0
  )

#The quants argument determines the threshold at which the data will be taken
#into account for calibration : the default of 0.05 induces that the 5% most
#extreme values will be avoided for each variable on each side of its
#distribution along the gradient. So it in fact takes 5% away at each end of
#the variables distribution, giving a total of 10% of data not considered.

sre.095_v2 <-
  sre(
    Response = myResp,
    Explanatory = myExpl,
    NewData=myExpl,
    Quant = 0.025
  )# the value defines the most extreme values for
#each variable not to be taken into account for determining the tolerance
#boundaries for the considered species.


sre.090_v2 <-
  sre(
    Response = myResp,
    Explanatory = myExpl,
    NewData=myExpl,
    Quant = 0.05
  )


dev.off()
##' visualise results
par(mfrow=c(1,2))
plot(sre.100, main="BIOCLIM 100% v1")
plot(sre.100_v2, main="BIOCLIM 100% v2")
plot(sp, add=TRUE, col="red", pch =1, cex = 1.5)

plot(sre.095, main="BIOCLIM 97.5% v1")
plot(sre.095_v2, main="BIOCLIM 97.5% v2")
plot(sp, add=TRUE, col="red", pch =1, cex = 1.5)

plot(sre.090, main="BIOCLIM 95% (X variáveis)")
plot(sre.090_v2, main="BIOCLIM 95% (X variáveis)")
