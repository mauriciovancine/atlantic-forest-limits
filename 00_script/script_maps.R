#' ---
#' title: atlantic forest limits
#' author: mauricio vancine
#' date: 2023-10-12
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(sf)
library(nngeo)
library(tmap)
library(geobr)
library(rnaturalearth)

# options
sf::sf_use_s2(FALSE)

# import data -------------------------------------------------------------

# sa
sa <- rnaturalearth::ne_countries(continent = "South America", scale = 10)
sa

# brazil
br <- geobr::read_state(year = 2020)
br

# atlantic forest
tm_shape(br) +
  tm_polygons()

# atlantic forest limit files
atlantic_forest_limits_files <- dir(path = ".", pattern = "_limit.gpkg", recursive = TRUE, full.names = TRUE)
atlantic_forest_limits_files

# import
atlantic_forest_limits <- data.frame()
for(i in seq_along(atlantic_forest_limits_files)){
  
  atlantic_forest_limits_files_name <- atlantic_forest_limits_files[i] %>% 
    basename() %>% 
    sub("_limit.gpkg", "", .)
  
  atlantic_forest_limits_i <- sf::st_read(atlantic_forest_limits_files[i]) %>% 
    dplyr::mutate(id = i,
                  # type = atlantic_forest_limits_files_type,
                  name = atlantic_forest_limits_files_name,
                  area_ha = st_area(.)/1e4) %>% 
    dplyr::select(id, name, area_ha)
  
  atlantic_forest_limits <- rbind(atlantic_forest_limits, atlantic_forest_limits_i)
  
}
atlantic_forest_limits

# export
sf::st_write(atlantic_forest_limits, "01_atlantic_forest_limits/atlantic_forest_limits_unique.gpkg", delete_dsn = TRUE)

for (i in seq_len(nrow(atlantic_forest_limits))) {
  
  # extrai a linha i como um sf com 1 feição
  atlantic_forest_limits_i <- atlantic_forest_limits[i, ]
  
  # nome da camada
  atlantic_forest_limits_i_name <- atlantic_forest_limits_i$name
  
  # escreve no mesmo gpkg
  sf::st_write(atlantic_forest_limits_i, 
               "01_atlantic_forest_limits/atlantic_forest_limits_multiple.gpkg", 
               layer = atlantic_forest_limits_i_name, 
               append = TRUE)
  
}

# maps --------------------------------------------------------------------

# import
atlantic_forest_limits <- sf::st_read("01_atlantic_forest_limits/atlantic_forest_limits_unique.gpkg")
atlantic_forest_limits

# map
for(i in seq_len(nrow(atlantic_forest_limits))){
  
    map <- tm_shape(atlantic_forest_limits[i, ], bbox = atlantic_forest_limits[12, ]) +
    tm_fill(fill = "gray50") +
    tm_shape(sa) +
    tm_borders(col = "gray") +
    tm_shape(br) +
    tm_borders() +
    tm_credits(letters[i], position = c("right", "top"), size = 2, fontface = "bold")

    tmap::tmap_save(tm = map, width = 20, height = 25, units = "cm", dpi = 300,
                    filename = paste0("/home/mude/data/googledrive/01_manuscritos/articles/02_atlanticr/figures/fig01", letters[i], ".png"))
    
  }

# end ---------------------------------------------------------------------
