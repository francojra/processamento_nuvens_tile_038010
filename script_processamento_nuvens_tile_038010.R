# Classificação de nuvens para o tile 038010 --------------------------------------------------------------------------------------------------------------
# Data: 28/10/25 ------------------------------------------------------------------------------------------------------------------------------------------

# 1) Leitura das Bibliotecas Utilizadas

library(sits)
library(terra)
library(sf)
library(st)

# 2) Leitura do Cubo Desejado

nuvem_rm4 = sits_cube(source = "BDC",
                      collection = "SENTINEL-2-16D",
                      tiles = c("038010"),
                      bands = c("CLOUD"),
                      start_date = "2024-08-12",
                      end_date = "2024-12-31"
)

# 3) Utilizando o pacote TERRA para produzir um Spatial Raster de uma data específica do cubo para a banda CLOUD - imagem principal

nuvem_038010_principal <- sits_as_terra(nuvem_rm4,
                                        date = "2024-10-15",
                                        tile = "038010",
                                        bands = "CLOUD"          
)
