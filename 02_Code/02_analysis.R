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


#Final function for the calculation

final_res <- map2(
  gxti,
  names(gxti),
  corper_function,
  .progress = TRUE
)


final_df <- map(final_res, ~ if (is.list(.x)) .x[[1]] else .x) |>
  keep(is.data.frame) |>
  bind_rows()

save.image("final_results_data.RData")

write.csv(
  file = "01_Data/05_cleaned/cor_df.csv",
  cor_df,
  row.names = FALSE
)

rm(list = setdiff(ls(), c("final_res", "final_df", "KDE_general")))
gc()
