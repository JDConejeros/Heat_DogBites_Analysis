# 1.0 Process data -----

## Settings ----
source("02_Code/0.1 Settings.R")
source("02_Code/0.2 Packages.R")
source("02_Code/0.3 Functions.R")

# Input folder 
inp <- "01_Input/"

## 1 Open data ----

dogs <- rio::import(paste0(inp, "dogs_bites/", "2024-datos-mordedores.xlsx")) |> 
  clean_names()

temp <- rio::import(paste0(inp, "clime/", "temperature_daily_district.csv")) |> 
  clean_names()

ndvi <- rio::import(paste0(inp, "clime/", "ndvi_daily_district.csv")) |> 
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