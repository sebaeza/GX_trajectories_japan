
load("final_results_data.RData")

#Correlation analysis across variables

library(tidyverse)
library(polyglotr)

library(nortest)
library(tseries)

library(ggcorrplot)
#library(ggstatsplot)
library(GGally)
library(corrplot)
library(easystats)

library(gt)
library(gtsummary)
library(modelsummary)

library(igraph)
library(tidygraph)
library(ggraph)
library(netUtils)
library(graphlayouts)

library(sf)
library(ggsflabel)
library(ggspatial)
library(SpatialKDE)
library(sfheaders)

library(patchwork)
library(ragg)


cor_df <- (function(df){
      
cor_df <-  df |> dplyr::select(
      concat_level3, #1
      name_level3, #2
      
      time_span, #3
      patents_ext, #4
      
      green_patents, #5
      non_green_patents, #6
      
      gnrl_sp_cor, #7
      gnrl_sp_per, #8
      gxti_sp_cor, #9
      gxti_sp_per, #10
      
      full_sp_cor_cor, #11
      full_sp_cor_per, #12
      full_sp_per_cor, #13
      full_sp_per_per, #14
      
      net_cor, #15
      net_per, #16
      
      corper_cor_cor_cor, #17
      corper_cor_cor_per, #18
      corper_cor_per_cor, #19
      corper_cor_per_per, #20
      corper_per_cor_cor, #21
      corper_per_cor_per, #22
      corper_per_per_cor, #23
      corper_per_per_per, #24
      
      MPA_inv_nodes, #25
      MPA_time, #26
      
      MPA_gnrl_sp_cor, #27
      MPA_gnrl_sp_per, #28
      MPA_gxti_sp_cor, #29
      MPA_gxti_sp_per, #30
      
      MPA_full_sp_cor_cor, #31
      MPA_full_sp_cor_per, #32
      MPA_full_sp_per_cor, #33
      MPA_full_sp_per_per, #34
      
      MPA_net_cor, #35
      MPA_net_per, #36
      
      MPA_corper_cor_cor_cor, #37
      MPA_corper_cor_cor_per, #38
      MPA_corper_cor_per_cor, #39
      MPA_corper_cor_per_per, #40
      MPA_corper_per_cor_cor, #41
      MPA_corper_per_cor_per, #42
      MPA_corper_per_per_cor, #43
      MPA_corper_per_per_per, #44
      
      MPA_green, #45
      MPA_non_green, #46
      
      KDE_core_clusters, #47
      KDE_core_noncore #48
    ) |>
    mutate(
      across(5:24, ~ as.numeric((.x / sum(.x)) / (patents_ext / sum(patents_ext)), 
                                .names = "{(.col)}"))
    ) |>
    mutate(
      across(27:46, ~ as.numeric((.x / sum(.x)) / (MPA_inv_nodes / sum(MPA_inv_nodes)), 
                                       .names = "{(.col)}"))
    ) |>
    
    mutate(across(3:48, ~ as.numeric(scale(.x), .names = "{(.col)}")))
    #mutate(across(5:22, ~ as.numeric(log(.x + 1)), .names = "{(.col)}"))
  
  # cor_df <- df |>
  #   select(
  #     concat_level3,
  #     name_level3,
  #     
  #     sp_cor,
  #     sp_per,
  #     sp_for_per,
  #     net_cor,
  #     net_per,
  #     net_iso_per,
  #     
  #     MPA_sp_cor,
  #     MPA_sp_per,
  #     MPA_sp_for_per,
  #     MPA_net_cor,
  #     MPA_net_per,
  #     MPA_net_iso_per,
  #     
  #     KDE_core_clusters,
  #     KDE_core_noncore)

  cor_df
  
})(final_df)


#Corrplot using corrplot

M <- cor(
  cor_df |>
    select(where(is.numeric)),
  method = "spearman")

p.mat <- cor.mtest(
  cor_df |>
    select(where(is.numeric)),
  method = "spearman",
  conf.level = 0.95)

corrplot(
  M,
  p.mat = p.mat$p,
  method = "ellipse",
  type = "lower",
  sig.level = 0.05,
  order = "hclust",
  insig = "blank",
  )


#Corrplot using ggcorrplot

corr_plot <- ggcorrplot(
  M,
  method = "square",
  p.mat = pmat,
  show.diag = FALSE,
  hc.order = TRUE,
  hc.method = "average",
  type = "lower",
  insig = "blank",
  tl.cex = 8,
  legend.title = "Spearman correlation",
  sig.level = 0.05,
  ggtheme = theme_minimal) +
  theme(
    text = element_text(size = 8))

ggsave("03_Graphs/05_Correlation/corr_plot.png",
       plot = corr_plot,
       units = "cm",
       dpi = 1000,
       width = 19,
       height = 15,
       bg = "white")


ggpair_plot <- ggpairs(cor_df_selection)

ggsave("03_Graphs/05_Correlation/corr_plot_pairs.png",
       plot = ggpair_plot,
       units = "cm",
       dpi = 1000,
       width = 45,
       height = 45,
       bg = "white")

#Testing for normality----

# Calculate normality tests for each variable

cor_df_num <- cor_df |>
  dplyr::select(where(is.numeric)) |>
  dplyr:: select(
    full_sp_cor_cor, #11
    full_sp_cor_per, #12
    full_sp_per_cor, #13
    full_sp_per_per, #14
    net_cor, #15
    net_per, #16
    MPA_full_sp_cor_cor, #31
    MPA_full_sp_cor_per, #32
    MPA_full_sp_per_cor, #33
    MPA_full_sp_per_per, #34
    MPA_net_cor, #35
    MPA_net_per, #36
    KDE_core_clusters, #47
    KDE_core_noncore #48
  )

summary_df <- cor_df_num |>
  pivot_longer(cols = everything(),
               names_to = "Variable", 
               values_to = "value") |>
  group_by(Variable) |>
  summarise(
    Min = min(value, na.rm = TRUE),
    Max = max(value, na.rm = TRUE),
    Mean = mean(value, na.rm = TRUE),
    N = 62,
    .groups = "drop"
  )

summary_df |>
  gt() |>
  fmt_number(columns = c("Min", "Max"), decimals = 3)

normality_results <- map_dfr(names(cor_df_num), function(var) {
  x <- cor_df[[var]]
  sw <- shapiro.test(x)
  ad <- ad.test(x)
  jb <- jarque.bera.test(x)
  tibble(
    variable  = var,
    test      = c("Shapiro–Wilk", "Anderson–Darling", "Jarque–Bera"),
    statistic = c(sw$statistic, ad$statistic, jb$statistic),
    p_value   = c(sw$p.value, ad$p.value, jb$p.value)
  )
}) %>%
  group_by(variable) %>%
  mutate(is_normal = if_else(all(p_value > 0.05), "Normal", "Not normal")) %>%
  ungroup()


print(normality_results)

# Create combined Q–Q plots for all variables in one plot
cor_df_long <- cor_df_num %>%
  pivot_longer(cols = everything(), 
               names_to = "variable", 
               values_to = "value")

ggplot(cor_df_long, aes(sample = value)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(title = "Q–Q Plots for Each Variable")


#Correlation table----

corr_table <- correlation(
  cor_df_num,
  method = "spearman",
  ci = 0.95)

datasummary_correlation(
  data = corr_table,
  output = "03_Graphs/03_Tables/correlation_table.docx",
  stars = TRUE,
  notes = "+ = .1, * = .05, **= .01, *** = 0.001")


#Scatterstats

ggscatterstats(
  data = cor_df,
  x = KDE_core_noncore,
  y = MPA_sp_per,
  type = "nonparametric",
  digits = "signif"
)
