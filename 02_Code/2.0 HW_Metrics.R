# 2.0 HW metrics -----

## Settings ----
source("02_Code/0.1 Settings.R")
source("02_Code/0.2 Packages.R")
source("02_Code/0.3 Functions.R")

# Input folder 
inp <- "01_Input/"
out <- "data_analysis/"

## 1 Open data ----
clime <- rio::import(paste0(inp, "data_analysis/", "Temp_ndvi_2019_2025.RData")) 
glimpse(clime)

## 2. Temp reference period: 2019-2025 ----
detect_HW # base function 

ref_temp <- clime |> 
  group_by(codigo_comuna) |> 
  summarise(t30=30,
            t31=31,
            t32=32,
            t33=33,
            t34=34,
            p90_tmean=quantile(temperature_2m, probs = 0.90, digits = 2, na.rm = TRUE),
            p95_tmean=quantile(temperature_2m, probs = 0.95, digits = 2, na.rm = TRUE),
            p99_tmean=quantile(temperature_2m, probs = 0.99, digits = 2, na.rm = TRUE),
            p90_tmax=quantile(temperature_2m_max, probs = 0.90, digits = 2, na.rm = TRUE),
            p95_tmax=quantile(temperature_2m_max, probs = 0.95, digits = 2, na.rm = TRUE),
            p99_tmax=quantile(temperature_2m_max, probs = 0.99, digits = 2, na.rm = TRUE)) |> 
  ungroup()

export(ref_temp, paste0(inp, out, "Temp_reference_2019_2025.xlsx"))

clime <- clime |> 
  left_join(ref_temp, by="codigo_comuna")

glimpse(clime)

## Add heat wave metrics ----

hw_data <- detect_HW(clime) |> 
  select("codigo_comuna", "date", contains("HW_"))

summary(hw_data)

clime_hw <- clime |> 
  left_join(hw_data, by=c("codigo_comuna", "date"))

glimpse(clime_hw)
summary(clime_hw)

save(clime_hw, file=paste0(inp, out, "Temp_ndvi_hw_2019_2025", ".RData"))