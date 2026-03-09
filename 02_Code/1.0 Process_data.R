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

## 2 Process Dogs Bite data ----

glimpse(dogs)

## 3 Process Temp data ----

glimpse(temp)

## 4 Process NDVI data ----

glimpse(ndvi)

## 5 Process Poverty data ----

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

