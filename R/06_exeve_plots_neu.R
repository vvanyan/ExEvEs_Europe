
source("./source/libs.R") 

source("./source/global_variables.R")  # a similar thing to libs.R 

annual_summary_long   <- readRDS("data/processed/annual_summary_neu.rds")
annual_intensity_long <- readRDS("data/processed/annual_intensity_neu.rds")
extreme_wide          <- readRDS("data/processed/extreme_ratio_annual_neu.rds")
monthly_ratio_summary <- readRDS("data/processed/monthly_ratio_neu.rds")

## Period break for vertical line
period_break <- 2003.5
## (A) Line plot – Annual total evaporation
gg_severity <- ggplot(
  annual_summary_long,
  aes(x = year, y = evaporation, color = condition)
) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_vline(
    xintercept = period_break,
    linetype   = "dashed",
    size       = 0.8
  ) +
  scale_x_continuous(breaks = seq(min(annual_summary_long$year),
                                  max(annual_summary_long$year),
                                  by = 4)) +
  xlab("Year") +
  ylab("Evaporation (mm/year)") +
  scale_color_manual(
    values = c("#0571b0", "#ca0020", "#f4a582"),
    name   = "Conditions"
  ) +
  ggtitle("A) Annual Mean Total Evaporation") +
  base_theme
## (B) Line plot – Annual intensity
gg_intensity <- ggplot(
  annual_intensity_long,
  aes(x = year, y = evaporation, color = condition)
) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_vline(
    xintercept = period_break,
    linetype   = "dashed",
    size       = 0.8
  ) +
  scale_x_continuous(breaks = seq(min(annual_intensity_long$year),
                                  max(annual_intensity_long$year),
                                  by = 4)) +
  xlab("Year") +
  ylab("Evaporation (mm/day)") +
  scale_color_manual(
    values = c("#0571b0", "#ca0020", "#f4a582"),
    name   = "Conditions"
  ) +
  ggtitle("B) Annual Mean Evaporation Intensity") +
  base_theme

## (C) Spatial map – Ratio of extreme evaporation
q_limits <- quantile(
  extreme_wide$ratio,
  probs = c(0.01, 0.99),
  na.rm = TRUE
)

min_lim <- q_limits[1]
max_lim <- q_limits[2]

spatial_plot <- ggplot() +
  geom_tile(
    data = extreme_wide,
    aes(x = lon, y = lat, fill = pmin(pmax(ratio, min_lim), max_lim))  # squish values
  ) +
  scale_fill_gradient2(
    low      = "#2166ac",
    mid      = "white",
    high     = "#b2182b",
    midpoint = 1,
    limits   = c(min_lim, max_lim),
    name     = expression(Ratio~(1 + Delta/baseline))
  ) +
  borders("world", colour = "grey30", size = 0.4) +
  coord_cartesian(
    xlim = c(min(extreme_wide$lon), max(extreme_wide$lon)),
    ylim = c(min(extreme_wide$lat), max(extreme_wide$lat))
  ) +
  scale_x_continuous(labels = function(x) paste0(x, "°")) +
  scale_y_continuous(labels = function(x) paste0(x, "°")) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("C) Spatial Ratio of Extreme Evaporation") +
  base_theme

## (D) Polar plot – Monthly ratio of extreme evaporation
monthly_plot <- ggplot(
  monthly_ratio_summary,
  aes(x = factor(month), y = mean_ratio, fill = mean_ratio)
) +
  geom_col(width = 1, color = "grey30") +
  coord_polar(start = -pi / 12) +
  scale_x_discrete(labels = month.abb) +
  scale_fill_gradient2(
    low      = "#2166ac",
    mid      = "white",
    high     = "#b2182b",
    midpoint = 1,
    name     = expression(Ratio~(1 + Delta/baseline)),
    guide = guide_colorbar(
      barwidth      = 12,
      barheight     = 0.6,
      title.position = "top"
    )
  ) +
  labs(y = "Ratio", x = NULL) +
  ggtitle("D) Monthly Ratio of Extreme Evaporation") +
  base_theme +
  theme(
    axis.text.y = element_blank(),
    axis.ticks  = element_blank()
  )

## Combine panels and save
gg_1 <- plot_grid(
  gg_severity,
  gg_intensity,
  ncol   = 2,
  labels = NULL,
  align  = "hv"
)

gg_2 <- plot_grid(
  spatial_plot,
  monthly_plot,
  ncol   = 2,
  labels = NULL,
  align  = "hv"
)

p_all <- plot_grid(
  gg_1,
  gg_2,
  nrow        = 2,
  rel_heights = c(1, 1.2)
)

ggsave(
  filename = "results/figure/wce_summary.pdf",
  plot     = p_all,
  width    = 16,
  height   = 12,
  dpi      = 600
)
