# -------------------------------------------------------------------------
### Modelagem de nicho ecológico: Teoria e prática 
# Pacotes utilizados

# Prof. Luisa Maria Diele-Viegas 
# -------------------------------------------------------------------------


# Dados de ocorrencia 
# visualização e manipulacao
if(!require(tidyverse)) install.packages("tidyverse", dependencies = TRUE)
if(!require(lubridate)) install.packages("lubridate", dependencies = TRUE)

# download de ocorrencias
if(!require(spocc)) install.packages("spocc", dependencies = TRUE)

# limpeza taxonomica
if(!require(taxize)) install.packages("taxize", dependencies = TRUE)

# limpeza espacial
if(!require(CoordinateCleaner)) install.packages("CoordinateCleaner", dependencies = TRUE)
if(!require(sf)) install.packages("sf", dependencies = TRUE)

# manipulacao e visualização de variaveis
if(!require(ggspatial)) install.packages("ggspatial", dependencies = TRUE)
if(!require(landscapetools)) install.packages("landscapetools", dependencies = TRUE)
if(!require(raster)) install.packages("raster", dependencies = TRUE)
if(!require(rgdal)) install.packages("rgdal", dependencies = TRUE)
if(!require(devtools)) install.packages("devtools", dependencies = TRUE)
if(!require(wesanderson)) devtools::install_github("karthik/wesanderson")

# limites geograficos
if(!require(rnaturalearth)) install.packages("rnaturalearth", dependencies = TRUE)

# correlacao e selecao de modelos
if(!require(corrr)) install.packages("corrr", dependencies = TRUE)
if(!require(caret)) install.packages("caret", dependencies = TRUE)
if(!require(psych)) install.packages("psych", dependencies = TRUE)

# algoritmos
# bioclim, domain, e mahalanobis
if(!require(dismo)) install.packages("dismo", dependencies = TRUE)

# SVM
if(!require(kernlab)) install.packages("kernlab", dependencies = TRUE)

# Random Forest
if(!require(randomForest)) install.packages("randomForest", dependencies = TRUE)

# Consenso de modelos
if(!require(vegan)) install.packages("vegan", dependencies = TRUE)

# som de notificacao
if(!require(beepr)) install.packages("beepr", dependencies = TRUE)

# Fim! 