# Classificação de nuvens para o tile 041012 --------------------------------------------------------------------------------------------------------------
# Data: 15/10/25 ------------------------------------------------------------------------------------------------------------------------------------------

# 1) Leitura das Bibliotecas Utilizadas

library(sits)
library(terra)
library(sf)
library(st)

# 2) Leitura do Cubo desejado

nuvem_rm1 = sits_cube(source = "BDC",
                      collection = "SENTINEL-2-16D",
                      tiles = "041012",
                      bands = c("CLOUD"),
                      start_date = "2024-07-27",
                      end_date = "2024-12-31"
)

# 3) Utilizando o pacote TERRA para produzir um Spatial Raster de uma data específica do cubo para a banda CLOUD - imagem principal

nuvem_041012_principal <- sits_as_terra(nuvem_rm1,
                                        date = "2024-09-29",
                                        tile = "041012",
                                        bands = "CLOUD"          
)

# Projetando para garantir que esteja no crs SIRGAS 2000

nuvem_041012_sirgas_principal <- project(nuvem_041012_principal, "EPSG:4674")

# 4) Criando a máscara da imagem principal

## Criar máscara (TRUE onde r == 3. Cloud Shadows, 8. Cloud Medium Probability, 
## 9. Cloud High Probability, 10. Thin Cirrus, 11. Snow)
## https://brazil-data-cube.github.io/specifications/bands/SCL.html

mask_v1 <- nuvem_041012_sirgas_principal %in% c(3, 8, 9, 10, 11)

# Converter para 0/NA - 0 = máscara, NA = fora

mask_v1 <- classify(mask_v1, cbind(0, NA))

# Converter para polígonos

mask_pol_v1 <- as.polygons(mask_v1)

# Limpar microproblemas

mask_pol_v1 <- terra::buffer(mask_pol_v1, 0)

# Máscara final - validando as geometrias

mask_valid <- makeValid(mask_pol_v1)

# Salvar como shapefile

writeVector(mask_valid, "mask_041012_principal.shp", overwrite = TRUE)

# 5) Utilizando o pacote TERRA para produzir um Spatial Raster de uma data específica do cubo para a banda CLOUD - imagem secundária

nuvem_041012_secundaria <- sits_as_terra(nuvem_rm1,
                                         date = "2024-08-12",
                                         tile = "041012",
                                         bands = "CLOUD"          
)

# Projetando para garantir que esteja no crs SIRGAS 2000

nuvem_041012_sirgas_secundaria <- project(nuvem_041012_secundaria, "EPSG:4674")

# 6) Criando a máscara da imagem secundária

## Criar máscara (TRUE onde r == 3. Cloud Shadows, 8. Cloud Medium Probability, 
## 9. Cloud High Probability, 10. Thin Cirrus, 11. Snow)

mask_v2 <- nuvem_041012_sirgas_secundaria %in% c(3, 8, 9, 10, 11)

# Converter para 1/NA - 0 = máscara, NA = fora

mask_v2 <- classify(mask_v2, cbind(0, NA))

# Converter para polígonos

mask_pol_v2 <- as.polygons(mask_v2)

# Limpar microproblemas

mask_pol_v2 <- terra::buffer(mask_pol_v2, 0)

# Máscara final - validando as geometrias

mask_valid_v2 <- makeValid(mask_pol_v2)

# Salvar como shapefile

writeVector(mask_valid_v2, "mask_041012_secundaria.shp", overwrite = TRUE)

# 9) Realizando a comparação da interseção entre os shapefiles de nuvem da principal com a secundária e gerando o resultado final da máscara de nuvem

# Leitura dos shapefiles da principal e secundária

shp_principal <- st_read("mask_041012_principal.shp")
shp_secundaria <- st_read("mask_041012_secundaria.shp")

shp_principal <- vect(shp_principal)
shp_secundaria <- vect(shp_secundaria)

# Conferir se os dois shapefiles estão no mesmo CRS

if (st_crs(shp_principal) != st_crs(shp_secundaria)) {
  shp2 <- st_transform(shp_secundaria, st_crs(shp_principal))
}

# Agora calcular a interseção

intersecao_nuvem <- intersect(shp_principal, shp_secundaria)

# Ler shapefile original

nuvens <- intersecao_nuvem

# Criar uma cópia (para preservar o original na união final)

nuvens_copia <- nuvens

# Reprojetar para UTM (necessário para buffer em metros)

nuvens <- project(nuvens, "EPSG:31983")
nuvens_copia <- project(nuvens_copia, "EPSG:31983")

# Buffer duplo (expandir e retrair)

nuvens_buf <- buffer(nuvens, width = 50)
nuvens_buf <- buffer(nuvens_buf, width = -50)

# Unir com a cópia original (union + dissolve)

nuvens_union <- rbind(nuvens_copia, nuvens_buf)
nuvens_union <- makeValid(nuvens_union) # corrige geometrias inválidas
nuvens_final <- aggregate(nuvens_union) # dissolver bordas internas

# Salvar resultado final

writeVector(nuvens_final, "intersec_mask_041012.shp", overwrite = TRUE)

## Finalização no QGIS: 

## 1 - Transformar o shape para partes simples, 
## 2 - Fazer o cálculo da área e retirar os menores de 1 ha 
## 3 - Reprojetar e validar as geometrias

###########################################################################################################################################################
