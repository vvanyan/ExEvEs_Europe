
## Phase 1 – Data Preparation for ExEvEs Analysis

source("./source/libs.R") # I created a source file name lib which includes all required libraries
#for our analysis by source(./source/libs.R) you will install and load all required libraries
# and no need to load library every time in each .R scripts load data 
source("./source/global_variables.R")

evap_med <- readRDS("data/tabular/evap_1980_2023_med.rds")

evap_copy <- copy(evap_med)[order(lon, lat)]
evap_copy[, grid_id := .GRP, by = .(lat, lon)]
lookup <- unique(evap_copy[, .(grid_id, lon, lat)])
exeves <- detect_exeve(evap_med)
exeves <- merge(exeves, lookup, by = "grid_id", all.x = TRUE)

#Filter data between 1984 to 2023
exeves[, year  := year(date)]
exeves[, month := month(date)]
exeves <- exeves[year >= 1984 & year <= 2023]
# drop all pixels falling below 34 ( our focus is EUMediterranean region)
## (A) Annual Evaporation Totals (per pixel, then spatial mean)

annual_evap <- exeves[
  ,
  .(evap_yr = sum(value, na.rm = TRUE)),
  by = .(lon, lat, year)
]
ev_days <- exeves[evap_event == TRUE & !is.na(event_id)]
annual_exeve_amount <- ev_days[
  ,
  .(exeve_yr = sum(value, na.rm = TRUE)),
  by = .(lon, lat, year)
]
annual_exeve_freq <- ev_days[
  ,
  .(exeve_frq = uniqueN(event_id)),
  by = .(lon, lat, year)
]
event_stats <- ev_days[
  ,
  .(
    duration = .N,
    event_amount = sum(value, na.rm = TRUE),
    event_intensity = sum(value, na.rm = TRUE) / .N  # mm/day
  ),
  by = .(lon, lat, year, event_id)
]
annual_event_means <- event_stats[
  ,
  .(
    m_duration_exeve = mean(duration, na.rm = TRUE),
    mean_intensity = mean(event_intensity, na.rm = TRUE)
  ),
  by = .(lon, lat, year)
]
annual_stats <- Reduce(
  function(x, y) merge(x, y, by = c("lon", "lat", "year"), all = TRUE),
  list(
    annual_evap,
    annual_exeve_amount,
    annual_exeve_freq,
    annual_event_means
  )
)
annual_stats[, exeve_ratio:=exeve_yr/evap_yr]
saveRDS(annual_stats, "data/processed/annual_stats_med.rds")
