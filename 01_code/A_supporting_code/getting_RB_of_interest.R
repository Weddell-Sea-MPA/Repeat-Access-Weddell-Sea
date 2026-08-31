shape = st_read("/Users/flbell001/Documents/01_projects/02_ongoing_projects/update_ice_accessibility/03_original_data/research_blocks/research_blocksPolygon.shp")

shape_4_filtered = shape |>
  filter(GAR_Long_L == "48.6_4") |>
  select(GAR_Long_L)

st_write(shape_4_filtered, dsn = "./03_original_data/research_blocks/RB_48_6_4.shp", append = FALSE)

tm_shape(shape_4_filtered)+
  tm_polygons()+
  tm_graticules()+
  tm_layout(legend.outside = TRUE)

shape_5_filtered = shape |>
  filter(GAR_Long_L == "48.6_5") |>
  select(GAR_Long_L)

st_write(shape_5_filtered, dsn = "./03_original_data/research_blocks/RB_48_6_5.shp", append = FALSE)

tm_shape(shape_5_filtered)+
  tm_polygons()+
  tm_graticules()+
  tm_layout(legend.outside = TRUE)
