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
ratio_lab <- expression(Ratio ~ (1 + Delta/baseline))

ratio_dt[, region := factor(region, levels = c("NEU", "WCE", "MED"))]

ratio_box_kg <- ratio_dt[!is.na(ratio_annual) & !is.na(KG_class)]

# ---------------- NEU ----------------
kg_neu <- ratio_box_kg[region == "NEU"]
p_kg_neu <- ggplot(kg_neu, aes(x = KG_class, y = ratio_annual, fill=KG_class)) +
  geom_boxplot(outlier.shape = NA, linewidth = 0.3, outliers = FALSE) +
  coord_flip()+
  labs(x = NULL, y = ratio_lab) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.line = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank()
  )
kg_wce <- ratio_box_kg[region == "WCE"]
kg_wce <- kg_wce[!is.na(KG_class)]
p_kg_wce <- ggplot(kg_wce, aes(x = KG_class, y = ratio_annual, fill=KG_class)) +
  geom_boxplot(outlier.shape = NA, linewidth = 0.3, outliers = FALSE) +
  coord_flip()+
  labs(x = NULL, y = ratio_lab) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.line = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank()
  )

kg_med <- ratio_box_kg[region == "MED"]
kg_med <- kg_med[!is.na(KG_class)]
p_kg_med <- ggplot(kg_med, aes(x = KG_class, y = ratio_annual, fill=KG_class)) +
  geom_boxplot(outlier.shape = NA, linewidth = 0.3, outliers = FALSE) +
  coord_flip()+
  labs(x = NULL, y = ratio_lab) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.line = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank()
  )
# ---- 1) global levels + colors (you already have these) ----
kg_levels <- c("Cfb","Csa","Csb","Cfc","BSk","BSh","Cfa","BWk","BWh","Dfc","ET","Dfb","Dsb","Dfa","Dsa","Dsc")
kg_cols   <- setNames(palette.colors(length(kg_levels), palette = "Polychrome 36"), kg_levels)
kg_fill_scale <- scale_fill_manual(values = kg_cols, drop = FALSE, name = "KG classes")

# ---- 2) legend donor (forces ALL keys to exist) ----
legend_dt <- data.table(
  KG_class = factor(kg_levels, levels = kg_levels),
  y = 1
)
p_legend <- ggplot(legend_dt, aes(x = KG_class, y = y, fill = KG_class)) +
  geom_col() +
  kg_fill_scale +
  theme_classic() +
  theme(legend.position = "right")

leg_kg <- ggpubr::get_legend(p_legend)
p_kg_neu <- p_kg_neu + kg_fill_scale
p_kg_wce <- p_kg_wce + kg_fill_scale
p_kg_med <- p_kg_med + kg_fill_scale
p_kg_neu <- p_kg_neu + theme(plot.margin = margin(12, 12, 12, 12))
p_kg_wce <- p_kg_wce + theme(plot.margin =  margin(12, 12, 12, 12))
p_kg_med <- p_kg_med + theme(plot.margin =  margin(12, 12, 12, 12))

p1<- ggarrange(
  p_kg_neu, p_kg_wce, p_kg_med,
  ncol = 3, nrow = 1, align = "h",
  labels = "auto", label.x = 0,
  hjust = -0.5, vjust = 1.5,
  legend = "right", common.legend = TRUE,
  legend.grob = leg_kg
)

ratio_box_lc <- ratio_dt[
  !is.na(ratio_annual) &
    !is.na(land_cover_short_class) &
    !(land_cover_short_class %in% c("Other", "Water"))
]

# ---------------- NEU ----------------
lc_neu <- ratio_box_lc[region == "NEU"]
p_lc_neu <- ggplot(lc_neu, aes(x = land_cover_short_class, y = ratio_annual, fill = land_cover_short_class)) +
  geom_boxplot(outlier.shape = NA, linewidth = 0.3, outliers = FALSE) +
  coord_flip() +
  labs(x = NULL, y = ratio_lab) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.line = element_blank(),
    axis.text.y = element_blank()
  )

# ---------------- WCE ----------------
lc_wce <- ratio_box_lc[region == "WCE"]
p_lc_wce <- ggplot(lc_wce, aes(x = land_cover_short_class, y = ratio_annual, fill = land_cover_short_class)) +
  geom_boxplot(outlier.shape = NA, linewidth = 0.3, outliers = FALSE) +
  coord_flip() +
  labs(x = NULL, y = ratio_lab) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.line = element_blank(),
    axis.text.y = element_blank()
  )

# ---------------- MED ----------------
lc_med <- ratio_box_lc[region == "MED"]
p_lc_med <- ggplot(lc_med, aes(x = land_cover_short_class, y = ratio_annual, fill = land_cover_short_class)) +
  geom_boxplot(outlier.shape = NA, linewidth = 0.3, outliers = FALSE) +
  coord_flip() +
  labs(x = NULL, y = ratio_lab) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.line = element_blank(),
    axis.text.y = element_blank()
  )

# ---- 1) global levels + colors (ALL land-cover classes across all regions) ----
lc_levels <- sort(unique(ratio_box_lc$land_cover_short_class))
lc_cols   <- setNames(palette.colors(length(lc_levels), palette = "Polychrome 36"), lc_levels)
lc_fill_scale <- scale_fill_manual(values = lc_cols, drop = FALSE, name = "Land cover")

# ---- 2) legend donor (forces ALL keys to exist) ----
legend_lc_dt <- data.table(
  land_cover_short_class = factor(lc_levels, levels = lc_levels),
  y = 1
)

p_lc_legend <- ggplot(legend_lc_dt, aes(x = land_cover_short_class, y = y, fill = land_cover_short_class)) +
  geom_col() +
  lc_fill_scale +
  theme_classic() +
  theme(legend.position = "right")

leg_lc <- ggpubr::get_legend(p_lc_legend)

# ---- 3) add same scale + margins to all plots ----
p_lc_neu <- p_lc_neu + lc_fill_scale + theme(plot.margin = margin(12, 12, 12, 12))
p_lc_wce <- p_lc_wce + lc_fill_scale + theme(plot.margin = margin(12, 12, 12, 12))
p_lc_med <- p_lc_med + lc_fill_scale + theme(plot.margin = margin(12, 12, 12, 12))

# ---- 4) arrange ----
p2<- ggarrange(
  p_lc_neu, p_lc_wce, p_lc_med,
  ncol = 3, nrow = 1, align = "h",
  labels = c("d","e","f"), label.x = 0,
  hjust = -0.5, vjust = 1.5,
  legend = "right", common.legend = TRUE,
  legend.grob = leg_lc
)


ggarrange(
  p1, p2,
  ncol = 1, nrow = 2, align = "h"
)
ggsave("results/figure/annual_exeve_frequency_ratio_kg_lc.pdf", dpi = 600, height = 11.69, width = 11.69, units = "in", bg="white")

