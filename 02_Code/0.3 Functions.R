# 0.3 Functions -----

# 1. Make dummies variables function -----
make_dummies <- function(v, prefix = '') {
  s <- sort(unique(v))
  d <- outer(v, s, function(v, s) 1L * (v == s))
  colnames(d) <- paste0(gsub(".*\\$", "", deparse(substitute(v))), prefix, s)
  d
}

# 2. Detect Heat Waves function -----
detect_HW <- function(data){
# Sort the data by date to ensure sequence
  data <- data |> arrange(date)
  
# Create columns for each heat wave criterion with consecutive days
data <- data |> 
  mutate(
    # Day with temperature_2m_max > ref
    HW_day_30C = as.integer(temperature_2m_max > t30),
    HW_day_31C = as.integer(temperature_2m_max > t31),
    HW_day_32C = as.integer(temperature_2m_max > t32),
    HW_day_33C = as.integer(temperature_2m_max > t33),
    HW_day_34C = as.integer(temperature_2m_max > t34),
    HW_day_p90 = as.integer(temperature_2m_max > p90_tmax),
    HW_day_p95 = as.integer(temperature_2m_max > p95_tmax),
    HW_day_p99 = as.integer(temperature_2m_max > p99_tmax),
    HW_day_mean_p90 = as.integer(temperature_2m > p90_tmean),
    HW_day_mean_p95 = as.integer(temperature_2m > p95_tmean),
    HW_day_mean_p99 = as.integer(temperature_2m > p99_tmean),
    
    # 2 days HW consecutive with run length encoding (RLE)
    HW_30C_2d = +(lag(HW_day_30C, 1) + HW_day_30C >= 2),
    HW_31C_2d = +(lag(HW_day_31C, 1) + HW_day_31C >= 2),
    HW_32C_2d = +(lag(HW_day_32C, 1) + HW_day_32C >= 2),
    HW_33C_2d = +(lag(HW_day_33C, 1) + HW_day_33C >= 2),
    HW_34C_2d = +(lag(HW_day_34C, 1) + HW_day_34C >= 2),
    HW_p90_2d = +(lag(HW_day_p90, 1) + HW_day_p90 >= 2),
    HW_p95_2d = +(lag(HW_day_p95, 1) + HW_day_p95 >= 2),
    HW_p99_2d = +(lag(HW_day_p99, 1) + HW_day_p99 >= 2),

    HW_p90_mean_2d = +(lag(HW_day_mean_p90, 1) + HW_day_mean_p90 >= 2),
    HW_p95_mean_2d = +(lag(HW_day_mean_p95, 1) + HW_day_mean_p95 >= 2),
    HW_p99_mean_2d = +(lag(HW_day_mean_p99, 1) + HW_day_mean_p99 >= 2),

    # 3 days HW consecutive with run length encoding (RLE)
    HW_30C_3d = +(lag(HW_day_30C, 2) + lag(HW_day_30C, 1) + HW_day_30C >= 3),
    HW_31C_3d = +(lag(HW_day_31C, 2) + lag(HW_day_31C, 1) + HW_day_31C >= 3),
    HW_32C_3d = +(lag(HW_day_32C, 2) + lag(HW_day_32C, 1) + HW_day_32C >= 3),
    HW_33C_3d = +(lag(HW_day_33C, 2) + lag(HW_day_33C, 1) + HW_day_33C >= 3),
    HW_34C_3d = +(lag(HW_day_34C, 2) + lag(HW_day_34C, 1) + HW_day_34C >= 3),
    HW_p90_3d = +(lag(HW_day_p90, 2) + lag(HW_day_p90, 1) + HW_day_p90 >= 3),
    HW_p95_3d = +(lag(HW_day_p95, 2) + lag(HW_day_p95, 1) + HW_day_p95 >= 3),
    HW_p99_3d = +(lag(HW_day_p99, 2) + lag(HW_day_p99, 1) + HW_day_p99 >= 3),

    HW_p90_mean_3d = +(lag(HW_day_mean_p90, 2) + lag(HW_day_mean_p90, 1) + HW_day_mean_p90 >= 3),
    HW_p95_mean_3d = +(lag(HW_day_mean_p95, 2) + lag(HW_day_mean_p95, 1) + HW_day_mean_p95 >= 3),
    HW_p99_mean_3d = +(lag(HW_day_mean_p99, 2) + lag(HW_day_mean_p99, 1) + HW_day_mean_p99 >= 3),

    # 4 days HW consecutive with run length encoding (RLE)
    HW_30C_4d = +(lag(HW_day_30C, 3) + lag(HW_day_30C, 2) + lag(HW_day_30C, 1) + HW_day_30C >= 4),
    HW_31C_4d = +(lag(HW_day_31C, 3) + lag(HW_day_31C, 2) + lag(HW_day_31C, 1) + HW_day_31C >= 4),
    HW_32C_4d = +(lag(HW_day_32C, 3) + lag(HW_day_32C, 2) + lag(HW_day_32C, 1) + HW_day_32C >= 4),
    HW_33C_4d = +(lag(HW_day_33C, 3) + lag(HW_day_33C, 2) + lag(HW_day_33C, 1) + HW_day_33C >= 4),
    HW_34C_4d = +(lag(HW_day_34C, 3) + lag(HW_day_34C, 2) + lag(HW_day_34C, 1) + HW_day_34C >= 4),
    HW_p90_4d = +(lag(HW_day_p90, 3) + lag(HW_day_p90, 2) + lag(HW_day_p90, 1) + HW_day_p90 >= 4),
    HW_p95_4d = +(lag(HW_day_p95, 3) + lag(HW_day_p95, 2) + lag(HW_day_p95, 1) + HW_day_p95 >= 4),
    HW_p99_4d = +(lag(HW_day_p99, 3) + lag(HW_day_p99, 2) + lag(HW_day_p99, 1) + HW_day_p99 >= 4),

    HW_p90_mean_4d = +(lag(HW_day_mean_p90, 3) + lag(HW_day_mean_p90, 2) + lag(HW_day_mean_p90, 1) + HW_day_mean_p90 >= 4),
    HW_p95_mean_4d = +(lag(HW_day_mean_p95, 3) + lag(HW_day_mean_p95, 2) + lag(HW_day_mean_p95, 1) + HW_day_mean_p95 >= 4),
    HW_p99_mean_4d = +(lag(HW_day_mean_p99, 3) + lag(HW_day_mean_p99, 2) + lag(HW_day_mean_p99, 1) + HW_day_mean_p99 >= 4)
  ) |> 
  
  # Replace NA at beginning of series
  mutate(across(contains("HW_"), ~replace_na(., 0)))

return(data)
}

