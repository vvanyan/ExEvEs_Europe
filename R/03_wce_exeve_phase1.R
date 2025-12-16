source("./source/libs.R")
source("./source/global_variables.R")
evap_wce_dt<-readRDS("data/processed/evap_wce_dt.rds")
exeve_wce_dt<-readRDS("data/processed/exeve_wce_dt.rds")
evap_copy <- copy(evap_wce_dt)[order(lon, lat)]
evap_copy[, grid_id := .GRP, by = .(lat, lon)]
lookup <- unique(evap_copy[, .(grid_id, lon, lat)])
saveRDS(lookup,"data/processed/lookup_wce.rds")
lookup<-readRDS("data/processed/lookup_wce.rds")
exeve_raw <- readRDS("data/processed/exeve_wce.rds")
exeves <- merge(exeve_raw, lookup, by = "grid_id", all.x = TRUE)
saveRDS(exeves,"data/processed/exeves_merge_wce.rds")


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

# setting the coordinates
exeves<- exeves[lon>=-10 & lon<=40 & lat>=45 & lat<=62]
saveRDS(exeves,"data/processed/exeves_merge_wce_filtered.rds")
exeves<- readRDS("data/processed/exeves_merge_wce_filtered.rds")
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

saveRDS(annual_summary_long,      "data/processed/annual_summary_wce_trialfix.rds")

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
saveRDS(annual_intensity_long,    "data/processed/annual_intensity_wce_trialfix.rds")
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
saveRDS(extreme_wide,             "data/processed/extreme_ratio_annual_wce_trialfix.rds")
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
saveRDS(monthly_ratio_summary,    "data/processed/monthly_ratio_wce_trial.rds")


range(exeves$lon, na.rm=TRUE)
range(exeves$lat, na.rm=TRUE)
range(extreme_wide$lon, na.rm=TRUE)
range(extreme_wide$lat, na.rm=TRUE)
