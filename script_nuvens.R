### Passo a Passo Script - Máscara de Nuvem

# 1) Leitura das Bibliotecas Utilizadas
library(sits)
library(terra)
library(sf)
library(st)

# 2) Leitura do Cubo desejado

nuvem_rm1 = sits_cube(source = "BDC",
                      collection = "SENTINEL-2-16D",
                      tiles = c("037011", "037012", "037013", "038012",
                                "038013", "039012", "039013", "039014",
                                "040011", "040012", "040013", "040014",
                                "041011", "041012", "041013", "041014",
                                "041015", "042012", "042013", "042014"),
                      bands = c("CLOUD"),
                      start_date = "2024-07-27",
                      end_date = "2024-12-31"
)

nuvem_rm2 = sits_cube(source = "BDC",
                      collection = "SENTINEL-2-16D",
                      tiles = c("037016", "037017", "037018", "037019",
                                "038015", "038016", "038017", "038018",
                                "039015", "039016", "039017", "039018",
                                "040015", "040016", "040017"),
                      bands = c("CLOUD"),
                      start_date = "2024-06-09",
                      end_date = "2024-12-31"
)

nuvem_rm3 = sits_cube(source = "BDC",
                      collection = "SENTINEL-2-16D",
                      tiles = c("032014", "032015", "033013", "033014",
                                "033015", "034013", "034014", "034015",
                                "035013", "035014", "036013", "036014",
                                "036015", "037014", "037015", "038014"),
                      bands = c("CLOUD"),
                      start_date = "2024-07-27",
                      end_date = "2024-12-31"
)

nuvem_rm4 = sits_cube(source = "BDC",
                      collection = "SENTINEL-2-16D",
                      tiles = c("034012", "035008", "035009", "035010",
                                "035011", "035012", "036009", "036010",
                                "036011", "036012", "037009", "037010",
                                "038009", "038010", "038011", "039010",
                                "039011"),
                      bands = c("CLOUD"),
                      start_date = "2024-08-12",
                      end_date = "2024-12-31"
)

nuvem_rm5 = sits_cube(source = "BDC",
                      collection = "SENTINEL-2-16D",
                      tiles = c("033016", "033018", "034016", "034017",
                                "034018", "035015", "035016", "035017"),
                      bands = c("CLOUD"),
                      start_date = "2024-07-01",
                      end_date = "2024-12-31"
)

nuvem_rm6 = sits_cube(source = "BDC",
                      collection = "SENTINEL-2-16D",
                      tiles = c("033019", "033020", "033022", "033023",
                                "034019", "034020", "034021", "034022",
                                "035018", "035019", "035020", "035021",
                                "035022", "036016", "036017", "036018",
                                "036019", "036020", "036021"),
                      bands = c("CLOUD"),
                      start_date = "2024-06-09",
                      end_date = "2024-12-31"
)

nuvem_rm7 = sits_cube(source = "BDC",
                      collection = "SENTINEL-2-16D",
                      tiles = c("031016", "031017", "031018", "031019",
                                "031020", "031021", "031022", "031023",
                                "032016", "032017", "032018", "032019",
                                "032020", "032021", "032022", "032023",
                                "033017", "033021"),
                      bands = c("CLOUD"),
                      start_date = "2024-08-28",
                      end_date = "2024-12-31"
)
## saveRDS e readRDS 
# 3) Utilizando o pacote TERRA para produzir um Spatial Raster de uma data específica do cubo para a banda CLOUD - imagem principal

nuvem_041014_principal <- sits_as_terra(nuvem_rm1,
                                        date = "2024-11-16",
                                        tile = "041014",
                                        bands = "CLOUD"          
)
# Projetando para garantir que esteja no crs SIRGAS 2000
nuvem_041014_sirgas_principal <- project(nuvem_041014_principal, "EPSG:4674")

# 4) Criando a máscara da imagem principal

# Criar máscara (TRUE onde r == 3. Cloud Shadows, 8. Cloud Medium Probability, 9. Cloud High Probability, 10. Thin Cirrus, 11. Snow)
## https://brazil-data-cube.github.io/specifications/bands/SCL.html
mask_v1 <- nuvem_041014_sirgas_principal %in% c(3, 8, 9, 10, 11)

# Converter para 0/NA - 0 = máscara, NA = fora
mask_v1 <- classify(mask_v1, cbind(0, NA))

# Converter para polígonos
mask_pol_v1 <- as.polygons(mask_v1)

# Limpar microproblemas
mask_pol_v1 <- terra::buffer(mask_pol_v1, 0)

# Máscara final - validando as geometrias
mask_valid <- makeValid(mask_pol_v1)

# Salvar como shapefile
writeVector(mask_valid, "XXXXXXXXXXXXXX/mask_041014_principal.shp", overwrite = TRUE)

# 5) Utilizando o pacote TERRA para produzir um Spatial Raster de uma data específica do cubo para a banda CLOUD - imagem secundária

nuvem_041014_secundaria <- sits_as_terra(nuvem_rm1,
                                         date = "2024-08-28",
                                         tile = "041014",
                                         bands = "CLOUD"          
)

# Projetando para garantir que esteja no crs SIRGAS 2000
nuvem_041014_sirgas_secundaria <- project(nuvem_041014_secundaria, "EPSG:4674")

# 6) Criando a máscara da imagem secundária

# Criar máscara (TRUE onde r == 3. Cloud Shadows, 8. Cloud Medium Probability, 9. Cloud High Probability, 10. Thin Cirrus, 11. Snow)
mask_v2 <- nuvem_041014_sirgas_secundaria %in% c(3, 8, 9, 10, 11)

# Converter para 1/NA - 0 = máscara, NA = fora
mask_v2 <- classify(mask_v2, cbind(0, NA))

# Converter para polígonos
mask_pol_v2 <- as.polygons(mask_v2)

# Limpar microproblemas
mask_pol_v2 <- terra::buffer(mask_pol_v2, 0)

# Máscara final - validando as geometrias
mask_valid_v2 <- makeValid(mask_pol_v2)

# Salvar como shapefile
writeVector(mask_valid_v2, "XXXXXXXXXXXXXX/mask_041014_secundaria.shp", overwrite = TRUE)

# 9) Realizando a comparação da interseção entre os shapefiles de nuvem da principal com a secundária e gerando o resultado final da máscara de nuvem

# Leitura dos shapefiles da principal e secundária
shp_principal <- st_read("XXXXXXXXXXXXXX/mask_041014_principal.shp")
shp_secundaria <- st_read("XXXXXXXXXXXXXX/mask_041014_secundaria.shp")

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

# Salvar resultado
writeVector(nuvens_final, "XXXXXXXXXXXXXX/intersec_final.shp", overwrite = TRUE)

## Finalizei no QGIS: 
## 1 - Transformar o shape para partes simples, 
## 2 - Fazer o cálculo da área e retirar os menores de 1 ha 
## 3 - Reprojetar e validar as geometrias

###########################################################################################################################################################

## Fazer os shapefiles separados

### PRINCIPAL
# Ler shapefile original
nuvens_principal <- mask_pol_v1

# Criar uma cópia (para preservar o original na união final)
nuvens_principal_copia <- mask_pol_v1

# Buffer duplo (expandir e retrair)
buffer_principal <- buffer(nuvens_principal, width = 50)
buffer_principal <- buffer(buffer_principal, width = -50)

# Unir com a cópia original (union + dissolve)
nuvens_union_principal <- rbind(buffer_principal, nuvens_principal_copia)
nuvens_union_principal <- makeValid(nuvens_union_principal) # corrige geometrias inválidas
nuvens_final_principal <- aggregate(nuvens_union_principal) # dissolver bordas internas

# Salvar resultado
writeVector(nuvens_final, "XXXXXXXXXXXXXX/buffer_principal.shp", overwrite = TRUE)

### SECUNDÁRIA
# Ler shapefile original
nuvens_secundaria <- mask_pol_v2

# Criar uma cópia (para preservar o original na união final)
nuvens_secundaria_copia <- mask_pol_v2

# Buffer duplo (expandir e retrair)
buffer_secundaria <- buffer(nuvens_secundaria, width = 50)
buffer_secundaria <- buffer(buffer_secundaria, width = -50)

# Unir com a cópia original (union + dissolve)
nuvens_union_secundaria <- rbind(buffer_secundaria, nuvens_secundaria_copia)
nuvens_union_secundaria <- makeValid(nuvens_union_secundaria) # corrige geometrias inválidas
nuvens_final_secundaria <- aggregate(nuvens_union_secundaria) # dissolver bordas internas

# Salvar resultado
writeVector(nuvens_final, "XXXXXXXXXXXXXX/buffer_secundaria.shp", overwrite = TRUE)