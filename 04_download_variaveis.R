# Download 2.5 res worldclim V 2.1
P_url<- "https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_2.5m_bio.zip"
download.file(P_url, destfile="wc2.1_2.5m_bio.zip")
unzip("wc2.1_2.5m_bio.zip")

# Download 2.5 res Elevation
P_url<- "https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_2.5m_elev.zip"
download.file(P_url, destfile="wc2.1_2.5m_elev.zip")
unzip("wc2.1_2.5m_elev.zip")

#identificar todos os arquivos de formato tif dentro da pasta
tif <- dir(pattern = "tif$")
tif

# importar os rasters e agrupar em um stack
var <- raster::stack(tif)

#plotar um dos rasters
plot(var$wc2.1_2.5m_bio_10)
