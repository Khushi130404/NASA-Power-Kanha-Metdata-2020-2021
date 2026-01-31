# ============================================================
# NDVI Phenology + Meteorology (NASA POWER)
# ROI Shapefile + Median Statistics
# ============================================================

# -----------------------------
# 1. Install & Load Packages
# -----------------------------
packages <- c("sf", "jsonlite", "dplyr", "tidyr",
              "lubridate", "readr", "purrr")
for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
lapply(packages, library, character.only = TRUE)

# IMPORTANT: Disable s2 to avoid geometry errors
sf::sf_use_s2(FALSE)

# -----------------------------
# 2. File Paths (EDIT HERE)
# -----------------------------
roi_shp_path  <- "D:\\NASA_Power_Kanha_Metdata_2020_2021\\ROI\\Grassland_2\\kanha_grass_2.shp"
ndvi_csv_path <- "D:\\NASA_Power_Kanha_Metdata_2020_2021\\Data_Table\\data_ndvi\\gl2_kanha_table_5day_interpolated_SG.csv"
output_folder <- "D:\\NASA_Power_Kanha_Metdata_2020_2021\\Data_Table\\data_meteorology"

# Create output folder if it doesn't exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# -----------------------------
# 3. Read ROI & Get Centroid
# -----------------------------
roi <- st_read(roi_shp_path, quiet = TRUE)

# Ensure WGS84 (lat/lon)
roi <- st_transform(roi, 4326)

# Fix invalid geometries
roi <- st_make_valid(roi)

# Compute centroid safely (NO union)
centroid <- st_centroid(roi)
coords <- st_coordinates(centroid)

lat <- coords[2]
lon <- coords[1]

cat("Using ROI centroid:\n",
    "Latitude :", lat, "\n",
    "Longitude:", lon, "\n")

# -----------------------------
# 4. Read NDVI CSV
# -----------------------------
ndvi <- read_csv(ndvi_csv_path, show_col_types = FALSE)

ndvi <- ndvi %>%
  mutate(date = as.Date(date)) %>%
  select(date, ndvi_sg_scaled)

# -----------------------------
# 5. NASA POWER API URL
# -----------------------------
start_date <- "20200201"
end_date   <- "20210630"

power_url <- paste0(
  "https://power.larc.nasa.gov/api/temporal/daily/point?",
  "parameters=T2M,PRECTOTCORR,ALLSKY_SFC_SW_DWN,RH2M",
  "&community=AG",
  "&latitude=", lat,
  "&longitude=", lon,
  "&start=", start_date,
  "&end=", end_date,
  "&format=JSON"
)

# -----------------------------
# 6. Download POWER Data
# -----------------------------
power_raw <- fromJSON(power_url)
met <- power_raw$properties$parameter

met_df <- tibble(
  date = as.Date(names(met$T2M), "%Y%m%d"),
  T2M  = as.numeric(met$T2M),
  PPT  = as.numeric(met$PRECTOTCORR),
  RAD  = as.numeric(met$ALLSKY_SFC_SW_DWN),
  RH   = as.numeric(met$RH2M)
)

# -----------------------------
# 7. Convert to 5-Day Aggregates
# -----------------------------
met_5d <- met_df %>%
  arrange(date) %>%
  mutate(date_5d = floor_date(date, unit = "5 days")) %>%
  group_by(date_5d) %>%
  summarise(
    T2M = median(T2M, na.rm = TRUE),
    PPT = sum(PPT, na.rm = TRUE),
    RAD = median(RAD, na.rm = TRUE),
    RH  = median(RH,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(date = date_5d)

# -----------------------------
# 8. Nearest-Date Matching (±5 Days)
# -----------------------------
match_nearest_met <- function(ndvi_date, met_data, max_days = 5) {
  
  met_sub <- met_data %>%
    mutate(day_diff = abs(as.numeric(date - ndvi_date))) %>%
    filter(day_diff <= max_days) %>%
    arrange(day_diff)
  
  if (nrow(met_sub) == 0) {
    return(tibble(
      date = ndvi_date,
      T2M = NA_real_,
      PPT = NA_real_,
      RAD = NA_real_,
      RH  = NA_real_
    ))
  }
  
  nearest <- met_sub[1, ]
  
  tibble(
    date = ndvi_date,
    T2M = nearest$T2M,
    PPT = nearest$PPT,
    RAD = nearest$RAD,
    RH  = nearest$RH
  )
}

met_matched <- map_dfr(
  ndvi$date,
  match_nearest_met,
  met_data = met_5d,
  max_days = 5
)

# -----------------------------
# 9. Merge NDVI + Meteorology
# -----------------------------
final_df <- ndvi %>%
  left_join(met_matched, by = "date")

# -----------------------------
# 10. Save Final CSV
# -----------------------------
output_csv <- file.path(
  output_folder,
  "gl2_kanha_ndvi_meteorology.csv"
)

write_csv(final_df, output_csv)

cat("Final CSV saved at:\n", output_csv, "\n")

# -----------------------------
# 11. Quick Sanity Plot
# -----------------------------
plot(final_df$date, final_df$ndvi_sg_scaled,
     type = "l", col = "darkgreen",
     xlab = "Date", ylab = "NDVI (SG)",
     main = "NDVI Phenology (5-Day)")
