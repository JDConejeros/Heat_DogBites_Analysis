# 1.0 Process data -----

## Settings ----
source("02_Code/0.1 Settings.R")
source("02_Code/0.2 Packages.R")
source("02_Code/0.3 Functions.R")

# Input folder 
inp <- "01_Input/"
out <- "data_analysis/"

## 1 Open data ----

dogs <- rio::import(paste0(inp, "dogs_bites/", "bites_2019-2024.xlsx")) |> 
  clean_names()

temp <- rio::import(paste0(inp, "climate/", "temperature_daily_district.csv")) |> 
  clean_names()

ndvi <- rio::import(paste0(inp, "climate/", "ndvi_daily_district.csv")) |> 
  clean_names()

pov <- rio::import(paste0(inp, "poverty/", "casen_2017.dta")) |> 
  clean_names()

geo <- rio::import(paste0(inp, "data_analysis/", "district_geo.Rdata")) 

## 2 Process Dogs Bite data ----

glimpse(dogs)

dogs <- dogs |> 
  select(fecha_mordedura, comuna_ocurrio_mordedura, lugar_mordedura2, tipo_mordedura, sexo, edad_num, 
         grupo_comuna_subdere, glosa_grupo_comuna_subdere, 
         total_perros_suma_uc, poblacion_2022      
  ) |>
  mutate(
    date = as.Date(fecha_mordedura, format = "%Y-%m-%d"),
    year = year(date),
    month = month(date),
    day = day(date),
    day_year = lubridate::yday(date),
    day_month = lubridate::day(date),
    day_week = lubridate::wday(date, label = TRUE, abbr = FALSE),
    weekends = if_else(day_week %in% c("Saturday", "Sunday"), 1, 0),
    season = case_when(
      (month == 12 & day_month >= 21) | (month %in% c(1, 2)) | (month == 3 & day_month <= 20) ~ "Summer",
      (month == 3 & day_month >= 21) | (month %in% c(4, 5)) | (month == 6 & day_month <= 20) ~ "Autumn",
      (month == 6 & day_month >= 21) | (month %in% c(7, 8)) | (month == 9 & day_month <= 20) ~ "Winter",
      (month == 9 & day_month >= 21) | (month %in% c(10, 11)) | (month == 12 & day_month <= 20) ~ "Spring"
    ),
    district = comuna_ocurrio_mordedura,
    place = if_else(lugar_mordedura2 == "Dentro de Vivienda", "Indoor", if_else(lugar_mordedura2 == "Lugar Público", "Outdoor", NA_character_)),
    bite_type = if_else(tipo_mordedura == "Única", "Unique", if_else(tipo_mordedura == "Múltiple", "Multiple", NA_character_))
  )


## 3 Process Temp data ----

glimpse(temp)

temp <- temp |> 
  mutate(date = as.Date(date)) |> 
  dplyr::select(geometry_id, date, temperature_2m, temperature_2m_min, temperature_2m_max)

## 4 Process NDVI data ----

glimpse(ndvi)

ndvi <- ndvi |> 
  mutate(date = as.Date(date)) |> 
  dplyr::select(geometry_id, date, ndvi)

## 5. Data climates variables ----

start_date <- as.Date("2019-01-01")
end_date <- as.Date("2025-12-31")

glimpse(geo)

clim <- geo |> 
  select(geometry_id, codigo_region, codigo_comuna) |> 
  mutate(date = list(seq.Date(start_date, end_date, by = "day"))) |> 
  unnest(cols = c(date)) |> 
  left_join(temp, by = c("geometry_id", "date")) |> 
  left_join(ndvi, by = c("geometry_id", "date"))

# Impute ndvi data 

green <- clim |> 
  dplyr::select(codigo_comuna, date, ndvi) |>
  group_by(codigo_comuna) |>
  mutate(
    ndvi_kalman = na_kalman(
      ndvi,
      model = "StructTS",
      smooth = TRUE
    )
  ) |>
  ungroup()

glimpse(green)

# Join with clim data 
clim <- clim |> 
  left_join(green |> dplyr::select(codigo_comuna, date, ndvi_kalman), by = c("codigo_comuna", "date")) 

# Add spatial information 
clim <- clim |> 
  left_join(geo, by = c("geometry_id", "codigo_region", "codigo_comuna"))

clim <- clim |> 
  relocate(nombre_region, .after = codigo_region) |> 
  relocate(nombre_comuna, .after = codigo_comuna)

clim <- clim |> 
  select(-geometry)

# Test results 
test <- clim |> 
  group_by(codigo_region, nombre_region, codigo_comuna, nombre_comuna) |> 
  summarise(
    mean_temp = mean(temperature_2m, na.rm = TRUE),
    min_temp = mean(temperature_2m_min, na.rm = TRUE),
    max_temp = mean(temperature_2m_max, na.rm = TRUE),
    na_mean = sum(is.na(temperature_2m)),
    mean_ndvi = mean(ndvi_kalman, na.rm = TRUE),
    na_ndvi = sum(is.na(ndvi_kalman)),
          )

export(test, paste0(inp, out, "temp_ndvi_summary_district.xlsx"))

save(clim, file=paste0(inp, out, "Temp_ndvi_2019_2025", ".RData"))

## 6 Process Poverty data ----

glimpse(pov)

pov <- pov |> 
  dplyr::select(comuna, expc, varstrat, varunit, qautr, pobreza, pobreza_multi_4d) |> 
  drop_na()

# Create quintile dummy variables from qautr (quintil autodeclarado)
quintiles_dummy <- as.data.frame(make_dummies(pov$qautr, prefix = "q_"))
colnames(quintiles_dummy) <- paste0("q", 1:5)
pov <- bind_cols(pov, quintiles_dummy)

pobreza_multi_dummy <- as.data.frame(make_dummies(pov$pobreza_multi_4d, prefix = "p_"))
colnames(pobreza_multi_dummy) <- c("no_pob", "pob_multi")
pov <- bind_cols(pov, pobreza_multi_dummy)

pobreza_ing_dummy <- as.data.frame(make_dummies(pov$pobreza, prefix = "p_"))
colnames(pobreza_ing_dummy) <- c("pob_extreme", "pob", "no_pob")
pov <- bind_cols(pov, pobreza_ing_dummy)
pov <- pov |> dplyr::select(!starts_with("no_pob"))

glimpse(pov)

# Complex survey design for CASEN 2017
# Use varstrat as strata (not estrato)
# Use varunit as id
design <- svydesign(id = ~varunit, strata = ~varstrat, weights = ~expc, data = pov)

# Quintile Income: % of persons in each income quintile by district
quintil <- svyby(~q1 + q2 + q3 + q4 + q5 + pob_multi + pob + pob_extreme,
                 ~comuna, design, svymean, na.rm = TRUE) |> 
  mutate(across(starts_with("q"), ~ . * 100)) |> 
  mutate(across(starts_with("pob"), ~ . * 100)) |> 
  dplyr::select(comuna, q1, q2, q3, q4, q5, pob_multi, pob, pob_extreme) %>%
  mutate(quintil = apply(dplyr::select(., q1, q2, q3, q4, q5), 1, function(x) names(which.max(x))))

# Add year and save CASEN 2017 data
pov_mun <- quintil 
glimpse(pov_mun)

# Save data 
save(pov_mun, file=paste0(inp, out, "Poverty_casen_RM_2017", ".RData"))

