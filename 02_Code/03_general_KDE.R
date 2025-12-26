#Library import

library(sf)
library(tidyverse)
library(SpatialKDE)
library(sfheaders)
library(ggrepel)
library(ggsflabel)
library(ggspatial)
library(netUtils)
library(tidygraph)
library(readxl)
library(arrow)
library(Rcpp)
library(tictoc)
library(igraph)
library(ggraph)
library(paletteer)
library(gt)
library(gtExtras)
library(webshot2)
library(RcppArmadillo)
library(Matrix)
library(patchwork)
library(terra)
library(raster)
library(tidygeocoder)

sourceCpp("02_code/helper_functions/network_core_periphery.cpp")
sourceCpp("02_code/helper_functions/main_path.cpp")
source("02_code/helper_functions/network_core_periphery.r")
source("02_code/helper_functions/05_spnet_corper.r")

#General KDE

KDE_general <- (function(df) {
  inner_ap <- ap_gxti |>
    inner_join(
      df,
      by = "ida"
    ) |>
    dplyr::select(ida, concat_level3, name_level3)

  ap_x_cc <- cc |>
    filter(citing %in% inner_ap$ida | cited %in% inner_ap$ida) |>
    dplyr::select(cited, citing)

  ap_f <- ap_gxti |>
    filter(ida %in% ap_x_cc$citing | ida %in% ap_x_cc$cited) |>
    left_join(inner_ap, by = "ida") |>
    dplyr::select(ida, concat_level3, name_level3, n_inventor) |>
    mutate(green = if_else(is.na(name_level3), "N", "Y"))

  inv_df <- inv_jipo |>
    filter(ida %in% ap_f$ida) |>
    left_join(
      ap_f |>
        distinct(ida, n_inventor) |>
        dplyr::select(ida, n_inventor),
      by = "ida"
    ) |>
    mutate(prop_pat = 1 / n_inventor) |>
    filter(!is.na(x))

  df_sf <- inv_df |>
    group_by(x, y) |>
    summarise(pat = sum(prop_pat), .groups = "drop") |>
    st_as_sf(coords = c("x", "y")) |>
    st_set_crs("WGS84") |>
    st_transform(crs = st_crs(mun_sf)) |>
    st_filter(mun_sf, .predicate = st_intersects) |>
    filter(!is.na(pat))

  #Calculating Network core and periphery (inventors that share a patent)

  edges <- inv_df |>
    group_by(ida) |>
    filter(n() > 1) |>
    do(as.data.frame(t(combn(.$psn_id, 2)))) |>
    rename(from = V1, to = V2) |>
    distinct(from, to)

  g <- as_tbl_graph(edges, directed = FALSE) |>
    netUtils::delete_isolates() |>
    igraph::simplify(remove.multiple = TRUE, remove.loops = TRUE)

  g_f <- core_periphery_parallel(g, n_cores = 12)

  ap_net_corper <- g_f$node_assignments |>
    dplyr::select(node, degree, group) |>
    remove_rownames() |>
    rename(net_corper = group, ida = node)

  g_cor_per <- g |>
    as_tbl_graph() |>
    activate(nodes) |>
    left_join(ap_net_corper, join_by(name == ida))

  #Calculating the KDE
  df_sf_kde <- df_sf |>
    kde(
      band_width = 25000,
      kernel = "quartic", #Quartic is the Gaussian kernel distribution
      grid = grid,
      weights = df_sf$pat,
      quiet = FALSE
    ) |>
    mutate(
      sp_corper = if_else(
        kde_value >= quantile(kde_value[kde_value != 0], 0.975),
        "Core",
        "Periphery"
      )
    )

  bbox <- st_bbox(
    df_sf_kde |>
      filter(sp_corper == "Core")
  )

  # Compute aspect ratio (height / width)
  aspect_ratio <- (bbox$ymax - bbox$ymin) / (bbox$xmax - bbox$xmin)

  jpcities_sf_crop <- jpcities_sf |>
    st_filter(
      df_sf_kde |>
        filter(sp_corper == "Core"),
      .predicate = st_intersects
    )

  kde_map <- ggplot() +
    geom_sf(data = ken_sf, fill = "white", color = "lightgrey", lwd = 0.2) +
    geom_sf(
      data = df_sf_kde |>
        filter(sp_corper == "Core"),
      aes(fill = kde_value),
      color = "transparent",
      alpha = 0.6
    ) +
    coord_sf(
      xlim = c(bbox["xmin"], bbox["xmax"]),
      ylim = c(bbox["ymin"], bbox["ymax"])
    ) +
    geom_sf_text_repel(data = jpcities_sf_crop, aes(label = Name), size = 3) +
    scale_fill_viridis_c(name = "KDE Values") +
    annotation_scale(style = "ticks", location = "br") +
    annotation_north_arrow(
      style = north_arrow_minimal,
      which_north = "true",
      location = "tl"
    ) +
    theme_minimal() +
    theme(
      legend.key.size = unit(8, "pt"),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      title = element_text(size = 10)
    )

  ggsave(
    paste0("03_Graphs/01_KDE_maps/KDE_general.png"),
    kde_map,
    width = 19,
    height = 19 * aspect_ratio,
    bg = "white",
    units = "cm",
    dpi = 900
  )

  list(
    KDE = df_sf_kde,
    g = g_cor_per,
    ap_final = ap_f,
    inventor_final = inv_df
  )
})(gxti_general)


save.image("final_analysis_data.RData")
