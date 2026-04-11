# App-level settings used to avoid hardcoded values in server logic.
# Update these values to tune behavior without touching code.

app_settings <- list(
  delhi_areas = c(
    "Central Delhi", "North Delhi", "South Delhi", "East Delhi",
    "West Delhi", "New Delhi", "South West Delhi", "North East Delhi"
  ),
  synthetic = list(
    enabled = TRUE,
    sample_frac = 0.08,
    noise_sd = 0.08,
    max_time_shift_hours = 72,
    seeds = list(
      traffic = 42,
      air_quality = 43,
      energy = 44
    )
  ),
  prediction = list(
    wind_speed_fallback = 10,
    precipitation_fallback = 0
  )
)
