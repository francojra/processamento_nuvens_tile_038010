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

# Projetando para garantir que esteja no crs SIRGAS 2000

nuvem_038010_sirgas_principal <- project(nuvem_038010_principal, "EPSG:4674")

# 4) Criando a máscara da imagem principal

## Criar máscara (TRUE onde r == 3. Cloud Shadows, 8. Cloud Medium Probability, 
## 9. Cloud High Probability, 10. Thin Cirrus, 11. Snow)
## https://brazil-data-cube.github.io/specifications/bands/SCL.html

mask_v1 <- nuvem_038010_sirgas_principal %in% c(3, 8, 9, 10, 11)

# Converter para 0/NA - 0 = máscara, NA = fora

mask_v1 <- classify(mask_v1, cbind(0, NA))

# Converter para polígonos

mask_pol_v1 <- as.polygons(mask_v1)

# Limpar microproblemas

mask_pol_v1 <- terra::buffer(mask_pol_v1, 0)

# Máscara final - validando as geometrias

mask_valid <- makeValid(mask_pol_v1)

# Salvar como shapefile

writeVector(mask_valid, "mask_038010_principal.shp", overwrite = TRUE)

# 5) Utilizando o pacote TERRA para produzir um Spatial Raster de uma data específica do cubo para a banda CLOUD - imagem secundária

nuvem_038010_secundaria <- sits_as_terra(nuvem_rm4,
                                         date = "2024-08-12",
                                         tile = "038010",
                                         bands = "CLOUD"          
                                         
)
