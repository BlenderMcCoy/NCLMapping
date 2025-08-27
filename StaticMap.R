create_static_ncl_map <- function(data_file, map_title, metric_name, metric_type = c("number", "percentage")) {
  # Load required libraries
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(stringr)
  
  metric_type <- match.arg(metric_type)
  
  # Define North Central London boroughs
  ncl_boroughs <- c("Barnet", "Camden", "Enfield", "Haringey", "Islington")
  
  # Create temp directory for boundary files
  temp_dir <- tempdir()
  zip_path <- file.path(temp_dir, "london_boundaries.zip")
  extract_dir <- file.path(temp_dir, "boundaries")
  dir.create(extract_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Download and extract boundary files
  download.file(
    url = "https://data.london.gov.uk/download/38460723-837c-44ec-b9f0-1ebe939de89a/2a5e50ac-c22e-4d68-89e2-85f1e0ff9057/LB_LSOA2021_shp.zip",
    destfile = zip_path,
    mode = "wb",
    quiet = TRUE
  )
  unzip(zip_path, exdir = extract_dir)
  
  # Find the LB_shp directory
  lb_shp_dir <- file.path(extract_dir, "LB_shp")
  if (!dir.exists(lb_shp_dir)) {
    all_dirs <- list.dirs(extract_dir, recursive = TRUE)
    lb_shp_dir <- all_dirs[grepl("LB_shp", basename(all_dirs))][1]
  }
  
  # Read and combine all NCL borough shapefiles
  ncl_lsoa_list <- list()
  for (borough in ncl_boroughs) {
    shp_files <- list.files(lb_shp_dir, pattern = "\\.shp$", full.names = TRUE)
    borough_file <- shp_files[grepl(borough, shp_files, ignore.case = TRUE)]
    if (length(borough_file) > 0) {
      borough_sf <- st_read(borough_file[1], quiet = TRUE)
      borough_sf$Borough <- borough
      ncl_lsoa_list[[borough]] <- borough_sf
    }
  }
  
  ncl_lsoa_geom <- bind_rows(ncl_lsoa_list)
  
  # Create borough boundaries
  ncl_boroughs_geom <- ncl_lsoa_geom %>%
    group_by(Borough) %>%
    summarise(geometry = st_union(geometry), .groups = 'drop')
  
  # Load user data
  user_data <- read_csv(data_file, show_col_types = FALSE)
  if (!all(c("LSOA_code", "value") %in% names(user_data))) {
    names(user_data)[1:2] <- c("LSOA_code", "value")
  }
  
  # Identify LSOA code column in shapefile
  lsoa_code_col <- names(ncl_lsoa_geom)[grepl("lsoa.*cd|code", names(ncl_lsoa_geom), ignore.case = TRUE)][1]
  
  # Join data with geometries
  ncl_lsoa_geom[[lsoa_code_col]] <- as.character(ncl_lsoa_geom[[lsoa_code_col]])
  user_data$LSOA_code <- as.character(user_data$LSOA_code)
  
  lsoa_with_data <- ncl_lsoa_geom %>%
    left_join(user_data, by = setNames("LSOA_code", lsoa_code_col))
  
  # Decide legend label
  if (metric_type == "percentage") {
    legend_label <- paste0(str_wrap(metric_name, width = 20), " (%)")
    scale_fun <- scale_fill_viridis_c(
      option = "viridis", 
      labels = scales::label_percent(accuracy = 1),
      name = legend_label,
      na.value = "gray90"
    )
  } else {
    legend_label <- str_wrap(metric_name, width = 20)
    scale_fun <- scale_fill_viridis_c(
      option = "mako", 
      labels = scales::label_number(),
      name = legend_label,
      na.value = "gray90"
    )
  }
  
  # Create the static map
  gg <- ggplot() +
    geom_sf(data = lsoa_with_data, aes(fill = value), color = NA, size = 0.1) +
    geom_sf(data = ncl_boroughs_geom, fill = NA, color = "white", size = 1) +
    scale_fun +
    theme_void() +
    labs(title = map_title) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold", margin = margin(b = 10)),
      legend.position = "right"
    )
  
  # Clean up
  unlink(temp_dir, recursive = TRUE)
  
  return(gg)
}

# Usage examples:
# static_map <- create_static_ncl_map("data/example_data.csv", "My Map", "My Metric", metric_type = "number")
 static_map <- create_static_ncl_map("data/data.csv", "NCL Population Segmentation", "Proportion of patients segmented as healthy", metric_type = "percentage")
print(static_map)
