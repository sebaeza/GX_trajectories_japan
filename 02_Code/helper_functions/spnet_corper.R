#Final function to pass on map2

corper_function <- function(df, name) {
  inner_ap <- ap_gxti |>
    inner_join(
      df,
      by = "ida"
    ) |>
    dplyr::select(
      ida,
      concat_level3,
      name_level3
    )

  ap_x_cc <- cc |>
    filter(citing %in% inner_ap$ida | cited %in% inner_ap$ida) |>
    dplyr::select(
      cited,
      citing
    )

  ap_f <- ap_gxti |> #Dataframe with unique ap IDs for the analyzed category
    filter(ida %in% ap_x_cc$citing | ida %in% ap_x_cc$cited) |>
    left_join(
      inner_ap,
      by = "ida"
    ) |>
    dplyr::select(
      ida,
      concat_level3,
      name_level3,
      n_inventor
    ) |>
    mutate(
      green = if_else(is.na(name_level3), "N", "Y"),
      GXTI = "gxA01a"
    ) #Here have to replace by the argument "name"

  #Calculating KDE

  inv_df <- inv_jipo |> #Dataframe with unique IDs for inventors in the analyzed category
    filter(ida %in% ap_f$ida) |>
    left_join(
      ap_f |>
        distinct(
          ida,
          n_inventor
        ) |>
        dplyr::select(
          ida,
          n_inventor
        ),
      by = "ida"
    ) |>
    mutate(
      prop_pat = 1 / n_inventor
    )

  df_sf <- inv_df |>
    filter(!is.na(x)) |>
    group_by(x, y) |>
    summarise(pat = sum(prop_pat), .groups = "drop") |>
    st_as_sf(coords = c("x", "y")) |>
    st_set_crs("WGS84") |>
    st_transform(crs = st_crs(mun_sf)) |>
    st_filter(mun_sf, .predicate = st_intersects) |>
    filter(!is.na(pat))

  df_sf_kde <- df_sf |>
    kde(
      band_width = 25000,
      kernel = "quartic", #Quartic is the Gaussian kernel distribution
      grid = grid,
      weights = df_sf$pat,
      quiet = TRUE
    ) |> #'[Important filter: We consider all the cells? or just consider those with a value']
    mutate(
      gxti_sp_corper = if_else(
        kde_value >= quantile(kde_value[kde_value != 0], 0.975),
        "Core",
        "Periphery"
      )
    ) |>
    rename(gxti_kde_value = kde_value)

  #Extracting the KDE value per inventor and patent. Using KDE_General
  ap_kde_corper <- inv_df |>
    filter(!is.na(x)) |>
    st_as_sf(coords = c("x", "y")) |>
    st_set_crs("WGS84") |>
    st_transform(crs = st_crs(KDE_general$KDE)) |>
    st_join(KDE_general$KDE, .predicate = st_intersects) |>
    st_join(df_sf_kde, .predicate = st_intersects) |>
    sf_to_df(fill = TRUE) |>
    dplyr::select(
      ida,
      ida_seq,
      kde_value,
      sp_corper,
      gxti_kde_value,
      gxti_sp_corper
    ) |>
    mutate(
      full_sp_corper = if_else(
        is.na(sp_corper) & is.na(gxti_sp_corper),
        "Periphery/Periphery",
        paste0(sp_corper, "/", gxti_sp_corper)
      )
    )

  bbox <- st_bbox(
    df_sf_kde |>
      filter(gxti_sp_corper == "Core")
  )

  # Compute aspect ratio (height / width)
  aspect_ratio <- (bbox$ymax - bbox$ymin) / (bbox$xmax - bbox$xmin)

  jpcities_sf_crop <- jpcities_sf |>
    st_filter(
      df_sf_kde |>
        filter(gxti_sp_corper == "Core"),
      .predicate = st_intersects
    )

  kde_map <- ggplot() +
    geom_sf(data = ken_sf, fill = "white", color = "lightgrey", lwd = 0.2) +
    geom_sf(
      data = df_sf_kde |>
        filter(gxti_sp_corper == "Core"),
      aes(fill = gxti_kde_value),
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
    paste0("03_Graphs/01_KDE_maps/", name, ".png"), #Change the name in this part too
    kde_map,
    width = 19,
    height = 19 * aspect_ratio,
    bg = "white",
    units = "cm",
    dpi = 300
  )

  #Calculating Network core and periphery

  #Network of patents
  # edges <- inv_df |>
  #   group_by(psn_id) |>
  #   filter(n() > 1) |>
  #   do(as.data.frame(t(combn(.$ida, 2)))) |>
  #   rename(from = V1,
  #          to = V2) |>
  #   distinct(from, to) |>
  #   filter(from != to)

  #Network of inventors that share a patent

  edges <- inv_df |>
    group_by(ida) |>
    filter(n() > 1) |>
    do(as.data.frame(t(combn(.$psn_id, 2)))) |>
    rename(from = V1, to = V2) |>
    distinct(from, to)

  #Rest of the network

  g <- as_tbl_graph(edges, directed = FALSE) |>
    netUtils::delete_isolates() |>
    igraph::simplify(remove.multiple = TRUE, remove.loops = TRUE)

  g_f <- core_periphery_parallel(g, n_cores = 12)

  ap_net_corper <- g_f$node_assignments |>
    dplyr::select(node, degree, group) |>
    remove_rownames() |>
    rename(net_corper = group, psn_id = node)

  g_cor_per <- g |>
    as_tbl_graph() |>
    activate(nodes) |>
    left_join(ap_net_corper, join_by(name == psn_id))

  #Making the final dataframe for the category

  gxti_name <- na.omit(unique(ap_f$name_level3))

  gxt_code <- na.omit(unique(ap_f$concat_level3))

  ap_corper <- inv_df |>
    dplyr::select(
      ida,
      ida_seq,
      psn_id
    ) |>
    left_join(
      ap_kde_corper |>
        dplyr::select(!ida),
      by = "ida_seq"
    ) |>
    left_join(ap_net_corper, by = "psn_id") |>
    mutate(
      full_sp_corper = if_else(
        is.na(full_sp_corper),
        "Periphery/Periphery",
        full_sp_corper
      ),
      net_corper = case_when(
        is.na(net_corper) ~ "Periphery",
        net_corper == "core" ~ "Core",
        net_corper == "periphery" ~ "Periphery"
      ),
      corper = paste0(full_sp_corper, "-", net_corper),
      name_level3 = gxti_name,
      concat_level3 = gxt_code
    ) |>
    left_join(
      ap_f |>
        dplyr::select(ida, green),
      by = "ida"
    )

  #Calculating main paths

  ap_x_cc_dag <- ap_x_cc |>
    dplyr::select(cited, citing) |>
    filter(cited != citing) |>
    rename(from = cited, to = citing) |>
    mutate(
      year_from = as.integer(substr(from, 1, 4)),
      year_to = as.integer(substr(to, 1, 4))
    ) |>
    filter(year_from < year_to)

  # Build the directed graph from the filtered edges:
  g_dag <- as_tbl_graph(ap_x_cc_dag, directed = TRUE) |>
    biggest_component() |>
    igraph::simplify(remove.loops = TRUE)

  # Verify that the graph is a DAG:
  if (!igraph::is_dag(g_dag)) {
    stop("Graph is still not a DAG.")
  }

  # Convert the DAG to a sparse matrix:
  A <- as_adjacency_matrix(g_dag, sparse = TRUE)

  # Compute the global main path:
  main_path <- global_standard_main_path(A)

  vertex_ids <- V(g_dag)$name

  main_path_ids <- vertex_ids[main_path]

  g_MPA <- g_dag |>
    as_tbl_graph() |>
    activate(nodes) |>
    filter(name %in% main_path_ids) |>
    rename(ida = name)

  inv_f <- inv_df |>
    dplyr::select(ida, ida_seq)

  nodes_pat <- g_MPA %>% activate(nodes) %>% as_tibble()

  edges_pat <- g_MPA %>%
    activate(edges) %>%
    as_tibble() %>%
    mutate(from_patent = nodes_pat$ida[from], to_patent = nodes_pat$ida[to]) |>
    dplyr::select(from_patent, to_patent)

  # 2. Join inventor data to get inventor ids for both ends (the joins naturally create a cross-product)
  edges_inv <- edges_pat %>%
    left_join(inv_jipo, by = c("from_patent" = "ida")) %>%
    rename(from_inventor = ida_seq) %>%
    left_join(inv_jipo, by = c("to_patent" = "ida")) %>%
    rename(to_inventor = ida_seq) %>%
    dplyr::select(from_inventor, to_inventor)

  # 4. Create the inventor network graph
  g_inventor <- tbl_graph(
    edges = edges_inv %>% rename(from = from_inventor, to = to_inventor),
    directed = TRUE
  )

  g_MPA_inv <- g_inventor |>
    as_tbl_graph() |>
    activate(nodes) |>
    left_join(ap_corper, join_by(name == ida_seq)) |>
    mutate(
      corper = factor(
        corper,
        levels = c(
          "Core/Core-Core",
          "Core/Core-Periphery",
          "Core/Periphery-Core",
          "Core/Periphery-Periphery",
          "Periphery/Core-Core",
          "Periphery/Core-Periphery",
          "Periphery/Periphery-Core",
          "Periphery/Periphery-Periphery"
        )
      )
    )

  hr_tab <- g_MPA_inv |>
    activate(nodes) |>
    as_tibble() |>
    distinct(ida) |>
    left_join(
      hr,
      by = "ida"
    ) |>
    dplyr::select(
      ida,
      name
    ) |>
    mutate(
      name = if_else(is.na(name), "-", name),
      eng = unlist(polyglotr::google_translate(
        name,
        source_language = "ja",
        target_language = "en"
      ))
    ) |>
    dplyr::select(ida, eng)

  #Making the MPA graph

  g_graph <- ggraph(
    g_MPA_inv,
    layout = "sugiyama"
  ) +
    geom_edge_link(
      arrow = arrow(type = "closed", length = unit(2, "mm")),
      end_cap = circle(3, "mm"),
      edge_alpha = 0.6
    ) +
    geom_node_point(
      aes(
        color = corper
        # shape = green)
      ),
      size = 5
    ) +
    geom_node_text(
      data = function(x) {
        # Convert to tibble first
        nodes <- x %>%
          as_tibble() %>%
          # Join with hr_tab to get patent holder names
          left_join(hr_tab, by = c("ida")) %>%
          # Keep only nodes with patent holder names
          filter(!is.na(eng)) %>%
          # Group by patent holder name and keep only the first instance
          group_by(eng) %>%
          slice_head(n = 1) %>%
          ungroup()
        return(nodes)
      },
      aes(label = eng),
      repel = TRUE,
      nudge_y = 0.2,
      size = 3.5,
      color = "grey30"
    ) +
    # scale_shape_manual(
    #   name = "Green Technology",
    #   values = c(
    #     "Y" = 18,
    #     "N" = 15),
    #   labels = c(
    #     "Y" = "GXTI",
    #     "N" = "Non-GXTI"
    #   )) +
    scale_color_manual(
      values = c(
        "Core/Core-Core" = "#871C0FFF",
        "Core/Core-Periphery" = "#AF2213FF",
        "Core/Periphery-Core" = "#D9792EFF",
        "Core/Periphery-Periphery" = "#F4C659FF",
        "Periphery/Core-Core" = "#22394AFF",
        "Periphery/Core-Periphery" = "#368990FF",
        "Periphery/Periphery-Core" = "#6FC0BAFF",
        "Periphery/Periphery-Periphery" = "#F9F2D8FF"
      ),
      name = "Space(GKDE/GXTI) - Network"
    ) +
    theme_void() +
    theme(
      legend.position = "right",
      plot.margin = margin(8, 8, 8, 8)
    ) +
    guides(
      color = guide_legend(order = 1),
      shape = guide_legend(order = 2)
    )

  g_df <- g_MPA |>
    activate(nodes) |>
    as_tibble() |>
    mutate(year = as.numeric(substr(ida, 1, 4)))

  g_inv_df <- g_MPA_inv |>
    activate(nodes) |>
    as_tibble()

  num_nodes <- g_df |>
    nrow()

  num_nodes_MPA_inv <- g_inv_df |>
    nrow()

  dynamic_height <- 14 + 0.5 * (num_nodes_MPA_inv)

  ggsave(
    paste0("03_Graphs/02_MPA/", name, ".png"), #Change name here too
    plot = g_graph,
    units = "cm",
    height = dynamic_height,
    width = 19,
    bg = "white",
    dpi = 300
  )

  #Summary table
  #Calculating the number of core clusters

  df_sf_kde$core_val <- ifelse(df_sf_kde$gxti_sp_corper == "Core", 1, NA)
  df_vect <- vect(df_sf_kde)
  r <- rast(ext(df_vect), resolution = 5000)
  r_core <- rasterize(df_vect, r, field = "core_val", fun = "sum")
  core_clusters <- patches(r_core, directions = 8)
  n_clusters <- length(unique(core_clusters[!is.na(core_clusters)]))

  #Calculating the number of core cells in non core KDE areas

  df_sf_kde <- df_sf_kde |> mutate(cell_id = row_number())
  KDE_general_cell <- KDE_general$KDE |> mutate(cell_id = row_number())

  # Drop geometries and join by cell_id
  df_kde_df <- st_drop_geometry(df_sf_kde)
  KDE_general_df <- st_drop_geometry(KDE_general_cell)

  joined <- inner_join(df_kde_df, KDE_general_df, by = "cell_id")

  n_non_overlap <- joined |>
    filter(gxti_sp_corper == "Core", sp_corper != "Core") |>
    nrow()

  #Final summary table

  summary_table <- ap_corper |>
    mutate(year = as.numeric(substr(ida, 1, 4))) |>
    group_by(concat_level3, name_level3) |>
    summarise(
      years = paste0(min(year), "-", max(year)),
      time_span = max(year) - min(year),
      inventors = n_distinct(psn_id, na.rm = TRUE),
      patents_ext = n_distinct(ida),
      green_patents = n_distinct(ap_corper$ida[ap_corper$green == "Y"]),
      non_green_patents = n_distinct(ap_corper$ida[ap_corper$green == "N"]),

      gnrl_sp_cor = n_distinct(ap_corper$psn_id[sp_corper == "Core"]),
      gnrl_sp_per = n_distinct(ap_corper$psn_id[sp_corper == "Periphery"]),

      gxti_sp_cor = n_distinct(ap_corper$psn_id[gxti_sp_corper == "Core"]),
      gxti_sp_per = n_distinct(ap_corper$psn_id[gxti_sp_corper == "Periphery"]),

      full_sp_cor_cor = n_distinct(ap_corper$psn_id[
        full_sp_corper == "Core/Core"
      ]),
      full_sp_cor_cor = n_distinct(ap_corper$psn_id[
        full_sp_corper == "Core/Core"
      ]),
      full_sp_cor_per = n_distinct(ap_corper$psn_id[
        full_sp_corper == "Core/Periphery"
      ]),
      full_sp_per_cor = n_distinct(ap_corper$psn_id[
        full_sp_corper == "Periphery/Core"
      ]),
      full_sp_per_per = n_distinct(ap_corper$psn_id[
        full_sp_corper == "Periphery/Periphery"
      ]),

      net_cor = n_distinct(ap_corper$psn_id[net_corper == "Core"]),
      net_per = n_distinct(ap_corper$psn_id[net_corper == "Periphery"]),

      corper_cor_cor_cor = n_distinct(ap_corper$psn_id[
        corper == "Core/Core-Core"
      ]),
      corper_cor_cor_per = n_distinct(ap_corper$psn_id[
        corper == "Core/Core-Periphery"
      ]),
      corper_cor_per_cor = n_distinct(ap_corper$psn_id[
        corper == "Core/Periphery-Core"
      ]),
      corper_cor_per_per = n_distinct(ap_corper$psn_id[
        corper == "Core/Periphery-Periphery"
      ]),
      corper_per_cor_cor = n_distinct(ap_corper$psn_id[
        corper == "Periphery/Core-Core"
      ]),
      corper_per_cor_per = n_distinct(ap_corper$psn_id[
        corper == "Periphery/Core-Periphery"
      ]),
      corper_per_per_cor = n_distinct(ap_corper$psn_id[
        corper == "Periphery/Periphery-Core"
      ]),
      corper_per_per_per = n_distinct(ap_corper$psn_id[
        corper == "Periphery/Periphery-Periphery"
      ]),

      MPA_pat_nodes = num_nodes,
      MPA_inv_nodes = num_nodes_MPA_inv,
      MPA_time = max(g_df$year) - min(g_df$year),

      MPA_gnrl_sp_cor = sum(g_inv_df$sp_corper == "Core", na.rm = TRUE),
      MPA_gnrl_sp_per = sum(g_inv_df$sp_corper == "Periphery", na.rm = TRUE),
      MPA_gxti_sp_cor = sum(g_inv_df$gxti_sp_corper == "Core", na.rm = TRUE),
      MPA_gxti_sp_per = sum(
        g_inv_df$gxti_sp_corper == "Periphery",
        na.rm = TRUE
      ),

      MPA_full_sp_cor_cor = sum(
        g_inv_df$full_sp_corper == "Core/Core",
        na.rm = TRUE
      ),
      MPA_full_sp_cor_per = sum(
        g_inv_df$full_sp_corper == "Core/Periphery",
        na.rm = TRUE
      ),
      MPA_full_sp_per_cor = sum(
        g_inv_df$full_sp_corper == "Periphery/Core",
        na.rm = TRUE
      ),
      MPA_full_sp_per_per = sum(
        g_inv_df$full_sp_corper == "Periphery/Periphery",
        na.rm = TRUE
      ),

      MPA_net_cor = sum(g_inv_df$net_corper == "Core", na.rm = TRUE),
      MPA_net_per = sum(g_inv_df$net_corper == "Periphery", na.rm = TRUE),

      MPA_corper_cor_cor_cor = sum(
        g_inv_df$corper == "Core/Core-Core",
        na.rm = TRUE
      ),
      MPA_corper_cor_cor_per = sum(
        g_inv_df$corper == "Core/Core-Periphery",
        na.rm = TRUE
      ),
      MPA_corper_cor_per_cor = sum(
        g_inv_df$corper == "Core/Periphery-Core",
        na.rm = TRUE
      ),
      MPA_corper_cor_per_per = sum(
        g_inv_df$corper == "Core/Periphery-Periphery",
        na.rm = TRUE
      ),
      MPA_corper_per_cor_cor = sum(
        g_inv_df$corper == "Periphery/Core-Core",
        na.rm = TRUE
      ),
      MPA_corper_per_cor_per = sum(
        g_inv_df$corper == "Periphery/Core-Periphery",
        na.rm = TRUE
      ),
      MPA_corper_per_per_cor = sum(
        g_inv_df$corper == "Periphery/Periphery-Core",
        na.rm = TRUE
      ),
      MPA_corper_per_per_per = sum(
        g_inv_df$corper == "Periphery/Periphery-Periphery",
        na.rm = TRUE
      ),

      MPA_green = sum(g_inv_df$green == "Y", na.rm = TRUE),
      MPA_non_green = sum(g_inv_df$green == "N", na.rm = TRUE),

      KDE_core_clusters = n_clusters,
      KDE_core_noncore = n_non_overlap
    ) |>
    ungroup()

  return(list(
    summary = summary_table,
    kde = df_sf_kde,
    g = g_cor_per,
    mpa = g_MPA_inv
  ))
}
