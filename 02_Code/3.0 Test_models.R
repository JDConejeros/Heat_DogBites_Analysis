# 3.0 Test models -----

## Settings ----
source("02_Code/0.1 Settings.R")
source("02_Code/0.2 Packages.R")
source("02_Code/0.3 Functions.R")

# Input folder 
inp <- "01_Input/"
out <- "data_analysis/"

## 1 Open data ----

load(paste0(inp, out, "Dogs_Bites_2019_2025.RData"))
load(paste0(inp, out, "Temp_ndvi_hw_2019_2025.RData"))
temp <- clime_hw |> dplyr::select(-geometry_id)
load(paste0(inp, out, "Poverty_casen_2017.RData"))
pov <- pov_mun

## 2 Join data ----

bites <- dogs |> 
  group_by(cod_dis, date, place, sex, age_group) |> 
  summarise(n_bites = n(), .groups = "drop") 

bites_cov <- dogs |> 
  dplyr::select(cod_dis, district, class_mun, pop) |>
  distinct()

temp <- temp |> 
  dplyr::select(!any_of(c("geometry_id", "codigo_region", "nombre_region", "nombre_comuna")))

data <- bites |> 
  mutate(
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
    season = factor(season, levels = c("Summer", "Autumn", "Winter", "Spring")),
    covid = if_else(date >= as.Date("2020-03-11") & date <= as.Date("2023-08-31"), 1, 0)
  ) |> 
  left_join(bites_cov, by = "cod_dis") |> 
  left_join(pov, by = c("cod_dis" = "comuna")) |> 
  left_join(temp, by = c("cod_dis" = "codigo_comuna", "date"))

# Prepare for modeling: offset and drop NA in key vars
data <- data |> 
  mutate(offset_pop = log(pop)) |> 
  filter(pop > 0) |> 
  drop_na(sex, age_group, class_mun, n_bites)

glimpse(data)

## 3 Temperature and heat wave metrics ----

temp_metrics <- c(
  # Continuous temperature
  #"temperature_2m",
  #"temperature_2m_min",
  "temperature_2m_max"
  # Fixed thresholds (continuous - t30 to t34 are ref values, use HW dummies)
  # Heat wave day indicators
  #"HW_day_30C", "HW_day_31C", "HW_day_32C", "HW_day_33C", "HW_day_34C",
  #"HW_day_p90", "HW_day_p95", "HW_day_p99",
  #"HW_day_mean_p90", "HW_day_mean_p95", "HW_day_mean_p99",
  # 2-day consecutive
  #"HW_30C_2d", "HW_31C_2d", "HW_32C_2d", "HW_33C_2d", "HW_34C_2d",
  #"HW_p90_2d", "HW_p95_2d", "HW_p99_2d",
  #"HW_p90_mean_2d", "HW_p95_mean_2d", "HW_p99_mean_2d",
  # 3-day consecutive
  #"HW_30C_3d", "HW_31C_3d", "HW_32C_3d", "HW_33C_3d", "HW_34C_3d",
  #"HW_p90_3d", "HW_p95_3d", "HW_p99_3d",
  #"HW_p90_mean_3d", "HW_p95_mean_3d", "HW_p99_mean_3d",
  # 4-day consecutive
  #"HW_30C_4d", "HW_31C_4d", "HW_32C_4d", "HW_33C_4d", "HW_34C_4d",
  #"HW_p90_4d", "HW_p95_4d", "HW_p99_4d",
  #"HW_p90_mean_4d", "HW_p95_mean_4d", "HW_p99_mean_4d"
)

# Keep only metrics that exist in data
temp_metrics <- temp_metrics[temp_metrics %in% names(data)]

## 4 Negative binomial models ----

# Base formula: FE commune + FE temporal + controls + offset
# Covariates: sex, age_group, class_mun, commune info (q1, q2, q3, q4; q5 ref)
# Temporal FE: factor(year) + factor(month)
base_formula <- "n_bites ~ sex + age_group + q1 + q2 + q3 + q4 + factor(cod_dis) + offset(offset_pop)"

models_nb <- list()

tic()
for (m in temp_metrics) {
  f <- as.formula(paste0(base_formula, " + ", m))
  nm <- paste0("model_", m)
  tryCatch({
    fit <- glm.nb(f, data = data)
    models_nb[[nm]] <- broom::tidy(fit, exponentiate = TRUE, conf.int = TRUE)
  }, error = function(e) {
    models_nb[[nm]] <<- tibble(term = NA_character_, note = as.character(e))
  })
}
toc()

save(models_nb, file = paste0(inp, out, "Models_NB_IRR_2019_2025.RData"))

model1 <- glmmTMB(
  n_bites ~ 
    temperature_2m_max + 
    ndvi_kalman +
    place +
    sex +
    age_group +
    season + 
    class_mun +
    offset(log(pop)) +
    (1 | district),
  family = poisson(link = "log"),
  data = data # filter(data, place == "Outdoor") 
)

summary(model1)
tidy(model1, exponentiate = TRUE, conf.int = TRUE)
exp(cbind(IRR = fixef(model1)$cond, confint(model1)))

######

model2 <- glmmTMB(
  n_bites ~ 
    temperature_2m_max + 
    ndvi_kalman +
    place +
    sex +
    age_group +
    season + 
    class_mun +
    offset(log(pop)) +
    (1 | district),
  family = nbinom2(),
  data = data # filter(data, place == "Outdoor") 
)

summary(model2)
tidy(model2, exponentiate = TRUE, conf.int = TRUE)
exp(cbind(IRR = fixef(model2)$cond, confint(model2)))

######

model3 <- glmmTMB(
  n_bites ~ 
    temperature_2m + 
    sex +
    age_group +
    ndvi +
    offset(log(pop))  +
    (1 | district) + 
    (1 | year),
  family = poisson(link = "log"),
  data = data
)

summary(model3)
tidy(model3, exponentiate = TRUE, conf.int = TRUE)
exp(cbind(IRR = fixef(model3)$cond, confint(model3)))

######

model4 <- glmmTMB(
  n_bites ~ 
    temperature_2m +
    sex +
    age_group +
    ndvi +
    offset(log(pop))  +
    (1 | district) + 
    (1 | year),
  family = nbinom2(),
  data = data
)

summary(model4)
tidy(model4, exponentiate = TRUE, conf.int = TRUE)
exp(cbind(IRR = fixef(model4)$cond, confint(model4)))


######

model5 <- glmmTMB(
  n_bites ~ 
    HW_p95_3d +
    sex +
    age_group +
    ndvi +
    offset(log(pop))  +
    (1 | district) + 
    (1 | year),
  family = nbinom2(),
  data = filter(data, place == "Outdoor")
)

summary(model5)
tidy(model5, exponentiate = TRUE, conf.int = TRUE)
exp(cbind(IRR = fixef(model5)$cond, confint(model5)))
