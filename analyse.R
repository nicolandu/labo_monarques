#!/usr/bin/env Rscript

options(
  tikzDefaultEngine = "luatex",
  tikzLualatexPackages = c(
    r"[\usepackage{tikz}]",
    r"[\RequirePackage[output-decimal-marker={,}, per-mode=reciprocal, forbid-literal-units]{siunitx}]" # nolint: line_length_linter.
  )
)

library(dplyr)
library(finch)
library(ggplot2)
library(glue)
library(scales)
library(sf)
library(spatstat)
library(tikzDevice)

year_min <- 2020
mtl_name <- "Communauté-Urbaine-de-Montréal"
data_dir <- "./data"
output_dir <- "./figures"

canada_shapefile <- glue("{data_dir}/gadm41_CAN.gpkg")
darwin <- glue("{data_dir}/0008031-251009101135966.zip")

a_species <- c(
  "Asclepias syriaca",
  "Asclepias incarnata",
  "Arctium lappa",
  "Solidago canadensis"
)
b_species <- c(
  "Danaus plexippus",
  "Limenitis archippus"
)

close_distance <- 1000 # m
norm_min_distance <- 50 # m

brewer_palette <- "Dark2"

theo_label <- r"[CSR ($\pi r^2$)]"

# Faster to set to FALSE when testing (output to PDF)
# Set to TRUE for generating the .tex files used in the report
use_tikz <- TRUE

init <- function(name) {
  if (use_tikz) {
    tikz(glue("{output_dir}/{name}.tikz"),
      width = 7, height = 7, standAlone = FALSE,
    )
  }
}

end <- function() {
  if (use_tikz) {
    dev.off()
  }
}

french_lon_label <- function(x) {
  sapply(x, function(lon) {
    lon_str <- gsub("\\.", ",", format(round(abs(lon), 1), nsmall = 1))
    if (lon < 0) {
      paste0(lon_str, r"[\unit{\degree}~O]")
    } else {
      paste0(lon_str, r"[\unit{\degree}~E]")
    }
  })
}

french_lat_label <- function(x) {
  sapply(x, function(lat) {
    lat_str <- gsub(r"[\\.]", ",", format(round(abs(lat), 2), nsmall = 2))
    if (lat < 0) {
      paste0(lat_str, r"[\unit{\degree}~S]")
    } else {
      paste0(lat_str, r"[\unit{\degree}~N]")
    }
  })
}

all_species <- c(a_species, b_species)

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
  occ$verbatimScientificName %in% c(a_species, b_species),
]

occ_proj <- st_transform(occ, 32188) # NAD83

occ_proj <- subset(occ_proj, verbatimScientificName %in% all_species)

tmp_coords <- st_coordinates(occ_proj)
dup_locs <- duplicated(tmp_coords)
occ_proj <- occ_proj[!dup_locs, ]
coords <- st_coordinates(occ_proj)

# Reorder factor
occ_proj$verbatimScientificName <-
  factor(occ_proj$verbatimScientificName, levels = all_species)

# Reorder rows so plotting respects the factor order
occ_proj <- occ_proj |>
  arrange(verbatimScientificName)

canada <- st_read(canada_shapefile, layer = "ADM_ADM_2")
mtl <- canada[canada$NAME_2 == mtl_name, ]

mtl_proj <- st_transform(mtl, 32188) # NAD83
mtl_win <- as.owin(mtl_proj)

combined <- ppp(coords[, "X"], coords[, "Y"],
  window = mtl_win,
  marks = factor(occ_proj$verbatimScientificName)
)

results_list <- list()
results_list_close <- list()

for (a in a_species) {
  for (b in b_species) {
    combined <- ppp(
      coords[, "X"], coords[, "Y"],
      window = mtl_win,
      marks = factor(occ_proj$verbatimScientificName)
    )

    k <- Kcross(combined, i = a, j = b, correction = "iso")
    k_df <- as.data.frame(k)
    # Convert from m^2 to km^2
    k_df$theo <- k_df$theo / 1e6
    k_df$iso <- k_df$iso / 1e6
    k_df$a <- a
    k_df$b <- b


    i <- which(k_df$r >= close_distance)[1]
    # Subset all rows up to that index
    k_df_close <- k_df[1:i, ]

    results_list[[paste(a, b, sep = "_")]] <- k_df
    results_list_close[[paste(a, b, sep = "_")]] <- k_df_close
  }
}

k_df <- bind_rows(results_list)
k_df_close <- bind_rows(results_list_close)


occ_proj$label <- with(occ_proj, paste(
  r"[\textit{]", verbatimScientificName, r"[}]",
  sep = ""
))
k_df$label <- with(k_df, paste(
  r"[\textit{]", a, r"[} $\times$ \textit{]", b, r"[}]",
  sep = ""
))
k_df_close$label <- with(k_df_close, paste(
  r"[\textit{]", a, r"[} $\times$ \textit{]", b, r"[}]",
  sep = ""
))

theo_df <- k_df |>
  select(r, theo) |>
  distinct()
theo_df$label <- theo_label

# Subset all rows up to that index
i <- which(theo_df$r >= close_distance)[1]
theo_df_close <- theo_df[1:i, ]

# Avoid name clashes betweeen k_df and theo_df
# Drop theo_ref at the end
k_df_norm <- k_df |>
  left_join(
    theo_df |>
      select(r, theo) |>
      rename(theo_ref = theo),
    by = "r"
  ) |>
  mutate(norm = iso / theo_ref) |>
  select(-theo_ref)
r_prev <- max(
  k_df_norm$r[k_df_norm$r < norm_min_distance],
  na.rm = TRUE
)
k_df_norm <- k_df_norm |>
  filter(r >= r_prev)

k_df_close_norm <- k_df_close |>
  left_join(
    theo_df_close |>
      select(r, theo) |>
      rename(theo_ref = theo),
    by = "r"
  ) |>
  mutate(norm = iso / theo_ref) |>
  select(-theo_ref)
r_prev <- max(
  k_df_close_norm$r[k_df_close_norm$r < norm_min_distance],
  na.rm = TRUE
)
k_df_close_norm <- k_df_close_norm |>
  filter(r >= r_prev)

r_prev <- max(k_df_norm$r[k_df_norm$r < close_distance], na.rm = TRUE)
k_df_far_norm <- k_df_norm |>
  filter(r >= r_prev)

# Get unique labels from k_df (same labels as in k_df_close)
all_labels <- unique(k_df$label)


colors_norm <- setNames(
  scales::brewer_pal(palette = brewer_palette)(length(all_labels)),
  all_labels
)

colors <- c(
  setNames("black", theo_label),
  colors_norm
)

init("montreal_map")
ggplot() +
  geom_sf(data = mtl, fill = "lightgrey", color = "black", alpha = 0.3) +
  geom_sf(
    data = occ_proj,
    aes(color = label),
    size = 0.5,
    alpha = 0.5
  ) +
  scale_color_brewer(palette = brewer_palette) +
  labs(
    x = "Longitude",
    y = "Latitude",
    color = "Espèce"
  ) +
  scale_x_continuous(labels = french_lon_label) +
  scale_y_continuous(labels = french_lat_label) +
  coord_sf(expand = FALSE) +
  theme(legend.position = "bottom", legend.text = element_text(size = 8)) +
  guides(color = guide_legend(ncol = 3, byrow = TRUE))
end()

init("correlation")
ggplot(k_df, aes(x = r / 1000, y = iso, color = label)) +
  geom_line() +
  geom_line(
    data = theo_df,
    aes(x = r / 1000, y = theo, color = label),
    linetype = "dashed"
  ) +
  scale_color_manual(values = colors) +
  labs(
    x = r"[Distance $r$ (\unit{\km})]", y = r"[$K_{AB}(r)$ (\unit{\km\squared})]",
    color = r"[Espèces ($A \times B$)]"
  ) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 2, byrow = TRUE))
end()

init("correlation_close")
ggplot(k_df_close, aes(x = r, y = iso, color = label)) +
  geom_line() +
  geom_line(
    data = theo_df_close,
    aes(x = r, y = theo, color = label),
    linetype = "dashed"
  ) +
  scale_color_manual(values = colors) +
  labs(
    x = r"[Distance $r$ (\unit{\m})]",
    y = r"[$K_{AB}(r)$ (\unit{\km\squared})]",
    color = r"[Espèces ($A \times B$)]"
  ) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 2, byrow = TRUE))
end()

init("correlation_norm")
ggplot(k_df_norm, aes(x = r / 1000, y = norm, color = label)) +
  geom_line() +
  scale_color_manual(values = colors_norm) +
  labs(
    x = r"[Distance $r$ (\unit{\km})]",
    y = r"[$\frac{K_{AB}(r)}{\pi r^2}$]",
    color = r"[Espèces ($A \times B$)]"
  ) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 2, byrow = TRUE))
end()

init("correlation_close_norm")
ggplot(k_df_close_norm, aes(x = r, y = norm, color = label)) +
  geom_line() +
  scale_color_manual(values = colors_norm) +
  labs(
    x = r"[Distance $r$ (\unit{\m})]",
    y = r"[$\frac{K_{AB}(r)}{\pi r^2}$]",
    color = r"[Espèces ($A \times B$)]"
  ) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 2, byrow = TRUE))
end()

init("correlation_far_norm")
ggplot(k_df_far_norm, aes(x = r / 1000, y = norm, color = label)) +
  geom_line() +
  scale_color_manual(values = colors_norm) +
  labs(
    x = r"[Distance $r$ (\unit{\km})]",
    y = r"[$\frac{K_{AB}(r)}{\pi r^2}$]",
    color = r"[Espèces ($A \times B$)]"
  ) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 2, byrow = TRUE))
end()
