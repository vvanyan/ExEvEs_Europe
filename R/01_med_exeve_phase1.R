
## Phase 1 – Data Preparation for ExEvEs Analysis

source("./source/libs.R") # I created a source file name lib which includes all required libraries
#for our analysis by source(./source/libs.R) you will install and load all required libraries
# and no need to load library every time in each .R scripts load data 
source("./source/global_variables.R")

evap_med <-
  tabular("data/raw/gleam-v4-1a_e_mm_medland_198001_202312_025_daily.nc") # this wont be run in windows

evap_copy <- copy(evap_med)[order(lon, lat)]
evap_copy[, grid_id := .GRP, by = .(lat, lon)]
lookup <- unique(evap_copy[, .(grid_id, lon, lat)])
exeves <- detect_exeve(evap_med)
exeves <- merge(exeves, lookup, by = "grid_id", all.x = TRUE)

#Filter data between 1984 to 2023
exeves[, year  := year(date)]
exeves[, month := month(date)]
exeves <- exeves[year >= 1984 & year <= 2023]

## Period split: early vs recent
exeves[
  ,
  period := fifelse(
    year <= 2003, "up_to_2003",
    fifelse(year >= 2004, "after_2003", NA_character_)
  )
]

# drop all pixels falling below 34 ( our focus is EUMediterranean region)
exeves<- exeves[lat>34]
## (A) Annual Evaporation Totals (per pixel, then spatial mean)

## Annual sums per pixel and per year
annual_pixel_summary <- exeves[
  ,
  .(
    all_days_sum      = sum(value, na.rm = TRUE),
    extreme_sum       = sum(value[evap_event == TRUE], na.rm = TRUE),
    non_extreme_sum   = sum(value[is.na(evap_event) | evap_event == FALSE],
                            na.rm = TRUE)
  ),
  by = .(lon,lat, year)
]

## Spatial mean across pixels for each year
annual_summary <- annual_pixel_summary[
  ,
  .(
    all_days_mean_sum    = mean(all_days_sum, na.rm = TRUE),
    extreme_mean_sum     = mean(extreme_sum, na.rm = TRUE),
    non_extreme_mean_sum = mean(non_extreme_sum, na.rm = TRUE)
  ),
  by = year
]

## Long format for plotting
annual_summary_long <- melt(
  annual_summary,
  id.vars      = "year",
  variable.name = "condition",
  value.name    = "evaporation"
)

annual_summary_long[
  ,
  condition := factor(
    condition,
    levels = c(
      "all_days_mean_sum",
      "extreme_mean_sum",
      "non_extreme_mean_sum"
    ),
    labels = c("All Days", "Extreme", "Non-Extreme")
  )
]
## (B) Annual Evaporation Intensity (per pixel, then spatial mean)

## Annual mean per pixel and year
annual_pixel_intensity <- exeves[
  ,
  .(
    all_days_mean      = mean(value, na.rm = TRUE),
    extreme_mean       = mean(value[evap_event == TRUE], na.rm = TRUE),
    non_extreme_mean   = mean(value[is.na(evap_event) | evap_event == FALSE],
                              na.rm = TRUE)
  ),
  by = .(lon,lat, year)
]

## Spatial mean of annual means
annual_intensity_summary <- annual_pixel_intensity[
  ,
  .(
    all_days_mean_intensity    = mean(all_days_mean, na.rm = TRUE),
    extreme_mean_intensity     = mean(extreme_mean, na.rm = TRUE),
    non_extreme_mean_intensity = mean(non_extreme_mean, na.rm = TRUE)
  ),
  by = year
]

## Long format for plotting
annual_intensity_long <- melt(
  annual_intensity_summary,
  id.vars       = "year",
  variable.name = "condition",
  value.name    = "evaporation"
)

annual_intensity_long[
  ,
  condition := factor(
    condition,
    levels = c(
      "all_days_mean_intensity",
      "extreme_mean_intensity",
      "non_extreme_mean_intensity"
    ),
    labels = c("All Days", "Extreme", "Non-Extreme")
  )
]

## (C) Spatial Change in Extreme Evaporation (Ratio map)

## Sum of ExEvE evaporation per pixel and period
extreme_spatial <- exeves[
  evap_event == TRUE & !is.na(period),
  .(
    value = sum(value, na.rm = TRUE)
  ),
  by = .(lon, lat, period)
]

## Wide: one row per pixel, columns for up_to_2003 and after_2003
extreme_wide <- dcast(
  extreme_spatial,
  lon + lat  ~ period,
  value.var = "value"
)

## Replace missing with 0 (no extremes in that period)
extreme_wide[is.na(up_to_2003),  up_to_2003  := 0]
extreme_wide[is.na(after_2003),  after_2003  := 0]

## Difference and ratio: Ratio = 1 + (after_2003 - up_to_2003)/up_to_2003
extreme_wide[
  ,
  diff_value := after_2003 - up_to_2003
]

extreme_wide[
  ,
  ratio := fifelse(
    up_to_2003 > 0,
    1 + diff_value / up_to_2003,
    NA_real_
  )
]

## (D) Monthly Change in Extreme Evaporation (ratio per month)
monthly_summary <- exeves[
  evap_event == TRUE & !is.na(period),
  .(
    value = sum(value, na.rm = TRUE)
  ),
  by = .(lon, lat, month, period)
]

monthly_wide <- dcast(
  monthly_summary,
  lon + lat + month ~ period,
  value.var = "value"
)

monthly_wide[is.na(up_to_2003), up_to_2003 := 0]
monthly_wide[is.na(after_2003), after_2003 := 0]

monthly_wide[
  ,
  diff_value := after_2003 - up_to_2003
]

monthly_wide[
  ,
  ratio := fifelse(
    up_to_2003 > 0,
    1 + diff_value / up_to_2003,
    NA_real_
  )
]

monthly_ratio_summary <- monthly_wide[
  ,
  .(
    mean_ratio = mean(ratio, na.rm = TRUE)
  ),
  by = month
]

## Save all outputs
saveRDS(annual_summary_long,      "data/processed/annual_summary_med.rds")
saveRDS(annual_intensity_long,    "data/processed/annual_intensity_med.rds")
saveRDS(extreme_wide,             "data/processed/extreme_ratio_annual_med.rds")
saveRDS(monthly_ratio_summary,    "data/processed/monthly_ratio_med.rds")
