# ============================================================
# NDVI vs Meteorology Monthly Plots
# 2 Curves per Graph | Auto-save
# ============================================================

# -----------------------------
# 1. Install & Load Packages
# -----------------------------
packages <- c("ggplot2", "dplyr", "readr", "lubridate", "scales")

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
  library(p, character.only = TRUE)
}

# -----------------------------
# 2. Paths (EDIT HERE)
# -----------------------------
input_csv <- "D:/NASA_Power_Kanha_Metdata_2020_2021/Data_Table/data_meteorology/se_kanha_ndvi_meteorology.csv"
plot_dir  <- "D:/NASA_Power_Kanha_Metdata_2020_2021/Plots/se_kanha_plots"

if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

# -----------------------------
# 3. Read Data
# -----------------------------
df <- read_csv(input_csv)

df <- df %>%
  mutate(date = as.Date(date))

# -----------------------------
# 4. Columns to Plot
# -----------------------------
ndvi_col <- "ndvi_sg_scaled"

met_cols <- c("T2M", "PPT", "RAD", "RH")

met_labels <- c(
  T2M = "Temperature (°C)",
  PPT = "Precipitation (mm)",
  RAD = "Solar Radiation (MJ/m²/day)",
  RH  = "Relative Humidity (%)"
)

# -----------------------------
# 5. Plot Function (REFERENCE-STYLE)
# -----------------------------
plot_ndvi_vs_met <- function(data, met_col) {

  scale_factor <- max(data[[ndvi_col]], na.rm = TRUE) /
                  max(data[[met_col]],  na.rm = TRUE)

  ggplot(data, aes(x = date)) +

    # NDVI
    geom_line(aes(y = .data[[ndvi_col]], color = "NDVI"),
              linewidth = 1) +
    geom_point(aes(y = .data[[ndvi_col]], color = "NDVI"),
               size = 1.2) +

    # Meteorology (scaled)
    geom_line(aes(y = .data[[met_col]] * scale_factor,
                  color = met_col),
              linewidth = 1) +
    geom_point(aes(y = .data[[met_col]] * scale_factor,
                   color = met_col),
               size = 1.2) +

    scale_y_continuous(
      name = "NDVI",
      sec.axis = sec_axis(~ . / scale_factor,
                          name = met_labels[[met_col]])
    ) +

    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b (%Y)"
    ) +

    scale_color_manual(
      values = c("NDVI" = "blue", met_col = "red"),
      labels = c("NDVI", met_labels[[met_col]])
    ) +

    labs(
      title = paste("NDVI vs", met_labels[[met_col]]),
      subtitle = "South East Kanha",
      x = "Time",
      color = "Variable"
    ) +

    theme_minimal(base_size = 13) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top"
    )
}

# -----------------------------
# 6. Generate & Save Plots
# -----------------------------
for (met in met_cols) {

  p <- plot_ndvi_vs_met(df, met)

  out_file <- file.path(
    plot_dir,
    paste0("se_kanha_NDVI_vs_", met, ".png")
  )

  ggsave(
    filename = out_file,
    plot = p,
    width = 11,
    height = 5,
    dpi = 300
  )
}
