# 0.4 Create district_geo shapefile ----
# Creates shapefile with ALL communes of Chile for Earth Engine extraction
# Run this script before 0.4 Extraction_Temperature_NDVI_EarthEngine_mun.py

## Settings ----
source("02_Code/0.1 Settings.R")
source("02_Code/0.2 Packages.R")
source("02_Code/0.3 Functions.R")

# Get ALL district of Chile 
district_chile <- mapa_comunas %>%
  mutate(codigo_comuna = as.numeric(codigo_comuna)) |> 
  mutate(codigo_region = as.numeric(codigo_region)) |> 
  mutate(geometry_id = row_number() - 1) |> 
  select(geometry_id, codigo_comuna, codigo_region, geometry)

district_data <- chilemapas::codigos_territoriales |> 
  select(codigo_region, codigo_comuna, nombre_region, nombre_comuna) |> 
  mutate(codigo_comuna = as.numeric(codigo_comuna)) |> 
  mutate(codigo_region = as.numeric(codigo_region)) 

glimpse(district_chile)
glimpse(district_data)

district_chile <- district_chile |> 
  left_join(district_data, by = c("codigo_region", "codigo_comuna")) |> 
  select(geometry_id, codigo_region, nombre_region, codigo_comuna, nombre_comuna,  geometry)

glimpse(district_chile)
class(district_chile)

# Ensure CRS is WGS84 (required for Earth Engine)
dis_geo_shp <- st_as_sf(district_chile)
st_crs(dis_geo_shp)

# Save shapefile and RData
st_write(dis_geo_shp, "01_Input/district_geo/district_geo.shp", delete_layer = TRUE)
save(district_chile, file = "01_Input/data_analysis/district_geo.RData")

