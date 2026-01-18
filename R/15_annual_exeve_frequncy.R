source("./source/libs.R") # I created a source file name lib which includes all required libraries
#for our analysis by source(./source/libs.R) you will install and load all required libraries
# and no need to load library every time in each .R scripts load data 
source("./source/global_variables.R")

annual_stats_neu <- readRDS("data/processed/annual_stats_neu.rds")
annual_stats_med <- readRDS("data/processed/annual_stats_med.rds")
annual_stats_wce <- readRDS("data/processed/annual_stats_wce.rds")


masks <- pRecipe::pRecipe_masks()
world_borders <- map_data("world")


# Keep land pixels + needed columns
masks_land <- masks[land_mask == "land", .(lon, lat, land_cover_short_class, KG_class)]

# ============================================================
# 1) Combine annual stats into ONE data.table
# ============================================================
annual_stats_all <- rbindlist(
  list(
    annual_stats_neu[, region := "NEU"],
    annual_stats_med[, region := "MED"],
    annual_stats_wce[, region := "WCE"]
  ),
  use.names = TRUE,
  fill = TRUE
)

# ============================================================
# 2) Ratio per pixel (by region) + join classes
# ============================================================
ratio_dt <- annual_stats_all[
  ,
  .(
    up_to_2003 = mean(exeve_frq[year <= 2003], na.rm = TRUE),
    after_2003 = mean(exeve_frq[year >  2003], na.rm = TRUE)
  ),
  by = .(region, lon, lat)
]

ratio_dt[is.nan(up_to_2003), up_to_2003 := NA_real_]
ratio_dt[is.nan(after_2003), after_2003 := NA_real_]

ratio_dt[
  ,
  ratio_annual := fifelse(
    is.na(up_to_2003) | up_to_2003 == 0 | is.na(after_2003),
    NA_real_,
    after_2003 / up_to_2003
  )
]

ratio_dt <- merge(ratio_dt, masks_land, by = c("lon", "lat"), all.x = TRUE)

ratio_dt <- ratio_dt[
  !is.na(land_cover_short_class) &
    !(land_cover_short_class %in% c("Other", "Water"))
]

# ============================================================
# 3) One shared q01–q99 (needed for a single faceted plot)
# ============================================================
lims_all <- ratio_dt[!is.na(ratio_annual),
                     as.numeric(quantile(ratio_annual, probs = c(0.01, 0.99), na.rm = TRUE))
]
q01_all <- lims_all[1]
q99_all <- lims_all[2]


# ============================================================
ratio_lab <- expression(Ratio ~ (1 + Delta/baseline))

ratio_dt[, region := factor(region, levels = c("NEU", "WCE", "MED"))]
ratio_neu<- ratio_dt[region=="NEU"]

p_ratio_neu<- ggplot() +
  geom_raster(data = ratio_neu, aes(lon, lat, fill = ratio_annual)) +
  geom_path(data = world_borders ,aes(long, lat, group = group),
            linewidth = 0.25, color = "black") +
  scale_fill_distiller(
    palette = "RdYlBu", direction = -1,
    limits = c(q01_all, q99_all), oob = squish,
    na.value = "grey92",
    name = ratio_lab
  ) +
  scale_x_continuous(
    breaks = function(x) pretty(x, n = 5),
    labels = function(x) sprintf("%d°%s", abs(x), ifelse(x < 0, "W", "E")),
    expand = c(0, 0), limits = c(min(ratio_neu$lon, na.rm = TRUE), max(ratio_neu$lon, na.rm = T))
  ) +
  scale_y_continuous(
    breaks = function(x) pretty(x, n = 5),
    labels = function(x) sprintf("%d°%s", abs(x), ifelse(x < 0, "S", "N")),
    expand = c(0, 0),  limits = c(min(ratio_neu$lat), max(ratio_neu$lat))
  ) +
  labs(x = NULL, y = "Latitude")+
  theme_classic(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.line = element_blank()
  )

ratio_wce<- ratio_dt[region=="WCE"]

p_ratio_wce<- ggplot() +
  geom_raster(data = ratio_wce, aes(lon, lat, fill = ratio_annual)) +
  geom_path(data = world_borders ,aes(long, lat, group = group),
            linewidth = 0.25, color = "black") +
  scale_fill_distiller(
    palette = "RdYlBu", direction = -1,
    limits = c(q01_all, q99_all), oob = squish,
    na.value = "grey92",
    name = ratio_lab
  ) +
  scale_x_continuous(
    breaks = function(x) pretty(x, n = 5),
    labels = function(x) sprintf("%d°%s", abs(x), ifelse(x < 0, "W", "E")),
    expand = c(0, 0), limits = c(min(ratio_wce$lon, na.rm = TRUE), max(ratio_wce$lon, na.rm = T))
  ) +
  scale_y_continuous(
    breaks = function(x) pretty(x, n = 5),
    labels = function(x) sprintf("%d°%s", abs(x), ifelse(x < 0, "S", "N")),
    expand = c(0, 0),  limits = c(min(ratio_wce$lat, na.rm = T), max(ratio_wce$lat, na.rm = T))
  ) +
  labs(x = NULL, y = "Latitude")+
  theme_classic(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.line = element_blank()
  )

ratio_med<- ratio_dt[region=="MED"]

p_ratio_med<- ggplot() +
  geom_raster(data = ratio_med, aes(lon, lat, fill = ratio_annual)) +
  geom_path(data = world_borders ,aes(long, lat, group = group),
            linewidth = 0.25, color = "black") +
  scale_fill_distiller(
    palette = "RdYlBu", direction = -1,
    limits = c(q01_all, q99_all), oob = squish,
    na.value = "grey92",
    name = ratio_lab
  ) +
  scale_x_continuous(
    breaks = function(x) pretty(x, n = 5),
    labels = function(x) sprintf("%d°%s", abs(x), ifelse(x < 0, "W", "E")),
    expand = c(0, 0), limits = c(min(ratio_med$lon, na.rm = TRUE), max(ratio_med$lon, na.rm = T))
  ) +
  scale_y_continuous(
    breaks = function(x) pretty(x, n = 5),
    labels = function(x) sprintf("%d°%s", abs(x), ifelse(x < 0, "S", "N")),
    expand = c(0, 0),  limits = c(min(ratio_med$lat, na.rm = T), max(ratio_med$lat, na.rm = T))
  ) +
  labs(x = "Longitude", y = "Latitude")+
  theme_classic(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.line = element_blank()
  )

ggarrange(p_ratio_neu, p_ratio_wce, p_ratio_med, ncol = 1, nrow = 3, align = "v", labels = "auto", label.x = 0,
          label.y = 1.01 ,hjust = -0.5,
          vjust = 1.5, common.legend = TRUE, legend = "right", font.label = list(size=14))


ggsave("results/figure/annual_exeve_frequncy_ratio.pdf", dpi = 600, height = 11.69, width = 11.69, units = "in", bg="white")

