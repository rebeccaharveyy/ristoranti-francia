required_packages <- c("sf", "ggplot2", "readr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

data_dir <- "data"
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

restaurants_url <- "https://raw.githubusercontent.com/holtzy/R-graph-gallery/master/DATA/data_on_french_states.csv"
communes_url <- "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/communes.geojson"

restaurants_path <- file.path(data_dir, "ristoranti_francia.csv")
communes_path <- file.path(data_dir, "france_communes.geojson")

download.file(restaurants_url, restaurants_path, mode = "wb")
download.file(communes_url, communes_path, mode = "wb")

# Il CSV ha un delimitatore ';' e una prima colonna indice senza nome.
restaurants_raw <- readr::read_delim(
  restaurants_path,
  delim = ";",
  skip = 1,
  col_names = FALSE,
  show_col_types = FALSE,
  quote = "\""
)

if (ncol(restaurants_raw) == 8) {
  names(restaurants_raw) <- c("row_id", "reg", "dep", "depcom", "dciris", "an", "typequ", "nb_equip")
} else if (ncol(restaurants_raw) == 7) {
  names(restaurants_raw) <- c("reg", "dep", "depcom", "dciris", "an", "typequ", "nb_equip")
  restaurants_raw$row_id <- NA_integer_
} else {
  stop("Formato CSV inatteso: numero di colonne non riconosciuto.")
}

restaurants_raw$depcom <- as.character(restaurants_raw$depcom)
restaurants_raw$typequ <- as.character(restaurants_raw$typequ)
restaurants_raw$nb_equip <- as.numeric(restaurants_raw$nb_equip)

restaurants_filtered <- restaurants_raw[restaurants_raw$typequ == "A504", c("depcom", "nb_equip")]
restaurants_by_commune <- stats::aggregate(
  nb_equip ~ depcom,
  data = restaurants_filtered,
  FUN = function(x) sum(x, na.rm = TRUE)
)
names(restaurants_by_commune)[names(restaurants_by_commune) == "nb_equip"] <- "n_ristoranti"

communes_sf <- sf::st_read(communes_path, quiet = TRUE)
communes_sf <- sf::st_transform(communes_sf, 4326)

communes_with_restaurants <- merge(
  communes_sf,
  restaurants_by_commune,
  by.x = "code",
  by.y = "depcom",
  all.x = TRUE
)
communes_with_restaurants$n_ristoranti[is.na(communes_with_restaurants$n_ristoranti)] <- 0

# Calcolo centroidi in CRS metrico per evitare warning su lon/lat.
communes_proj <- sf::st_transform(communes_with_restaurants, 2154)
commune_points_proj <- sf::st_centroid(communes_proj)
commune_points <- sf::st_transform(commune_points_proj, 4326)
coords <- sf::st_coordinates(commune_points)
commune_points$lon <- coords[, 1]
commune_points$lat <- coords[, 2]

# Definizione pratica del Sud della Francia: latitudine <= 45.
communes_sud <- commune_points[commune_points$lat <= 45 & commune_points$n_ristoranti > 0, ]
communes_sud_df <- sf::st_drop_geometry(communes_sud)
communes_map_sud <- communes_with_restaurants[
  sf::st_coordinates(sf::st_transform(sf::st_centroid(sf::st_transform(communes_with_restaurants, 2154)), 4326))[, 2] <= 45,
]

if (nrow(communes_sud) == 0) {
  stop("Nessun ristorante trovato nel Sud con i criteri correnti.")
}

p <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = communes_map_sud,
    fill = "grey95",
    color = "white",
    linewidth = 0.1
  ) +
  ggplot2::stat_bin_2d(
    data = communes_sud_df,
    ggplot2::aes(
      x = lon,
      y = lat,
      weight = n_ristoranti,
      fill = after_stat(count)
    ),
    bins = 120,
    alpha = 0.75,
    na.rm = TRUE
  ) +
  ggplot2::coord_sf(xlim = c(-6, 8), ylim = c(41, 46), expand = FALSE) +
  ggplot2::scale_fill_viridis_c(option = "magma", direction = 1, name = "Densita") +
  ggplot2::labs(
    title = "Mappa di densita dei ristoranti nel Sud della Francia",
    subtitle = "Dataset: data_on_french_states.csv (A504) + communes.geojson",
    x = "Longitudine",
    y = "Latitudine"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_line(color = "grey85", linewidth = 0.2),
    legend.position = "right"
  )

print(p)
ggplot2::ggsave(filename = file.path(data_dir, "mappa_densita_ristoranti_sud_francia.png"), plot = p, width = 10, height = 7, dpi = 300)
