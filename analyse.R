#!/usr/bin/env Rscript

options(tikzDefaultEngine = "luatex")

library(ggplot2)
library(scales)
library(tikzDevice)
library(glue)
library(finch)
library(sf)
library(spatstat)

year_min <- 2020
mtl_name <- "Communauté-Urbaine-de-Montréal"
data_dir <- "./data"
output_dir <- "./figures"

canada_shapefile <- glue("{data_dir}/gadm41_CAN.gpkg")
darwin <- glue("{data_dir}/0002115-251009101135966.zip")

asclepias_name <- "Asclepias syriaca"
danaus_name <- "Danaus plexippus"

use_tikz <- TRUE

init <- function(name) {
  if (use_tikz) {
    tikz(glue("{output_dir}/{name}.tex"),
      width = 7, height = 7, standAlone = FALSE
    )
  }
}

end <- function() {
  if (use_tikz) {
    dev.off()
  }
}

french_long_label <- function(x) {
  sapply(x, function(lon) {
    lon_str <- gsub("\\.", ",", format(round(abs(lon), 1), nsmall = 1))
    if (lon < 0) {
      paste0(lon_str, "°O")
    } else {
      paste0(lon_str, "°E")
    }
  })
}

french_lat_label <- function(x) {
  sapply(x, function(lat) {
    lat_str <- gsub("\\.", ",", format(round(abs(lat), 2), nsmall = 2))
    if (lat < 0) {
      paste0(lat_str, "°S")
    } else {
      paste0(lat_str, "°N")
    }
  })
}

darwin <- dwca_read(darwin, read = TRUE)
occ <- darwin$data$occurrence.txt

occ$decimalLongitude <- as.numeric(occ$decimalLongitude)
occ$decimalLatitude <- as.numeric(occ$decimalLatitude)
occ$year <- as.numeric(occ$year)

occ <- occ[
  !is.na(occ$decimalLongitude) &
    !is.na(occ$decimalLatitude) &
    !is.na(occ$year) &
    (occ$year >= year_min),
]

# Convert occurrence data to sf object
occ <- st_as_sf(
  occ,
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326 # WGS84
)

occ <- occ[
  occ$verbatimScientificName %in% c(asclepias_name, danaus_name),
]

occ_proj <- st_transform(occ, 32188) # NAD83
tmp_coords <- st_coordinates(occ_proj)

dup_locs <- duplicated(tmp_coords)
occ_proj <- occ_proj[!dup_locs, ]
coords <- st_coordinates(occ_proj)

asclepias_proj <- subset(occ_proj, verbatimScientificName == asclepias_name)
danaus_proj <- subset(occ_proj, verbatimScientificName == danaus_name)

canada <- st_read(canada_shapefile, layer = "ADM_ADM_2")
mtl <- canada[canada$NAME_2 == mtl_name, ]

mtl_proj <- st_transform(mtl, 32188) # NAD83
mtl_win <- as.owin(mtl_proj)

combined <- ppp(coords[, "X"], coords[, "Y"],
  window = mtl_win,
  marks = factor(occ_proj$verbatimScientificName)
)

k <- Kcross(combined, i = asclepias_name, j = danaus_name, correction = "iso")

k_df <- as.data.frame(k)
# Convert from m^2 to km^2
k_df$theo <- k_df$theo / 1e6
k_df$iso <- k_df$iso / 1e6


i <- which(k_df$r >= 1000)[1]
# Subset all rows up to that index
k_df_close <- k_df[1:i, ]

init("montreal_map")
ggplot() +
  geom_sf(data = mtl, fill = "lightgrey", color = "black", alpha = 0.3) +
  geom_sf(
    data = asclepias_proj,
    aes(color = verbatimScientificName), size = 0.5
  ) +
  geom_sf(data = danaus_proj, aes(color = verbatimScientificName), size = 0.5) +
  scale_color_manual(
    values = setNames(
      c("red", "blue"),
      c(asclepias_name, danaus_name)
    ),
    labels = setNames(
      c(bquote(italic(.(asclepias_name))), bquote(italic(.(danaus_name)))),
      c(asclepias_name, danaus_name)
    ),
    name = "Espèce"
  ) +
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  scale_x_continuous(labels = french_long_label) +
  scale_y_continuous(labels = french_lat_label) +
  coord_sf(expand = FALSE) +
  theme(legend.position = "bottom")
end()

init("correlation_map")
ggplot(k_df, aes(x = r / 1000)) +
  geom_line(aes(y = theo, color = "Théorique (CSR)"),
    linewidth = 0.8, linetype = "dashed"
  ) +
  geom_line(aes(y = iso, color = "Observée"), linewidth = 1.2) +
  scale_color_manual(
    values = c(
      "Théorique (CSR)" = "#A23B72",
      "Observée" = "#2E86AB"
    ),
    name = ""
  ) +
  labs(
    x = "Distance r (km)",
    y = "K(r) (km^2)"
  ) +
  theme(legend.position = "bottom")
end()

init("correlation_map_close")
ggplot(k_df_close, aes(x = r)) +
  geom_line(aes(y = theo, color = "Théorique (CSR)"),
    linewidth = 0.8, linetype = "dashed"
  ) +
  geom_line(aes(y = iso, color = "Observée"), linewidth = 1.2) +
  scale_color_manual(
    values = c(
      "Théorique (CSR)" = "#A23B72",
      "Observée" = "#2E86AB"
    ),
    name = ""
  ) +
  labs(
    x = "Distance r (m)",
    y = "K(r) (km2)"
  ) +
  theme(legend.position = "bottom")
end()
