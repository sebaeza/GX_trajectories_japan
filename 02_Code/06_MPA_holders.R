# Required packages
library(readxl)
library(dplyr)
library(stringr)

# Load Excel file
path_excel <- "hr_mpa_nistep.xlsx"

raw <- readxl::read_excel(path_excel, sheet = "Sheet 1")

# Check column names (just in case)
names(raw)

# Extract only the rows with company names
df <- raw %>%
  filter(!is.na(comp_name))

# Create unique patent × company pairs
df_main <- df %>%
  distinct(ida_seq, concat_level3, comp_name, pref_code, jsic_l, jsic_m)

# Simple checks
nrow(df_main)                      # Number of patent × company pairs
length(unique(df_main$ida_seq))    # Number of main-path patents with identifiable companies
length(unique(df_main$comp_name))  # Number of companies

# Core prefecture codes
core_pref <- c(11, 12, 13, 14, 26, 27, 28, 21, 23, 24)

# Classification of ownership location: core / periphery / unknown
df_main <- df_main %>%
  mutate(pref_code_int = as.integer(pref_code),
         ownership_pos = case_when(
           pref_code_int %in% core_pref ~ "core",
           is.na(pref_code_int)        ~ "unknown",
           TRUE                        ~ "periphery"
         ))

ownership_summary_overall <- df_main %>%
  count(ownership_pos) %>%
  mutate(share = n / sum(n))

ownership_summary_overall

# Ownership classification by GX category
ownership_by_gx <- df_main %>%
  group_by(concat_level3, ownership_pos) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(share = n / sum(n)) %>%
  ungroup()

ownership_by_gx %>%
  arrange(concat_level3, desc(share)) %>%
  head(20)  # Just checking the top rows

# Subset: gxA07a
gxA07a_df <- df_main %>%
  filter(concat_level3 == "gxA07a")

gxA07a_df

gxA07a_summary <- gxA07a_df %>%
  count(comp_name, pref_code_int, ownership_pos, jsic_l, jsic_m)

gxA07a_summary

# Subset: gxC01a
gxC01a_df <- df_main %>%
  filter(concat_level3 == "gxC01a")

gxC01a_df

gxC01a_summary <- gxC01a_df %>%
  count(comp_name, pref_code_int, ownership_pos, jsic_l, jsic_m) %>%
  arrange(comp_name)

gxC01a_summary

# Subset: gxB06d
gxB06d_df <- df_main %>%
  filter(concat_level3 == "gxB06d")

gxB06d_df

gxB06d_summary <- gxB06d_df %>%
  count(comp_name, pref_code_int, ownership_pos, jsic_l, jsic_m)

gxB06d_summary

# Overall share of ownership location categories
ownership_summary_overall <- df_main %>%
  count(ownership_pos) %>%
  mutate(share = n / sum(n))

ownership_summary_overall

# Core ratio by GX category
core_share_by_gx <- df_main %>%
  group_by(concat_level3) %>%
  summarise(
    n_total = n(),
    n_core  = sum(ownership_pos == "core"),
    n_peri  = sum(ownership_pos == "periphery"),
    n_unknown = sum(ownership_pos == "unknown"),
    share_core = n_core / n_total
  ) %>%
  arrange(desc(share_core))

core_share_by_gx

# Overall JSIC distribution
jsic_overall <- df_main %>%
  count(jsic_l, jsic_m) %>%
  arrange(desc(n))

jsic_overall

# Aggregation by JSIC large sector
jsic_large <- df_main %>%
  mutate(jsic_l2 = if_else(is.na(jsic_l), "Unknown", jsic_l)) %>%
  count(jsic_l2, name = "n") %>%
  arrange(desc(n))

jsic_large
