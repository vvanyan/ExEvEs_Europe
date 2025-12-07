## Base theme
base_theme <- theme_minimal(base_size = 14) +
  theme(
    axis.title   = element_text(size = 14, face = "bold"),
    axis.text    = element_text(size = 12),
    plot.title   = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    legend.position = "bottom",
    panel.grid   = element_line(size = 0.2, color = "grey85"),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
  )

