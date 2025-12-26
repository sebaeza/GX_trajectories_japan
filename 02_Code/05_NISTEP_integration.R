library(tidyverse)
library(arrow)
library(igraph)
library(tidygraph)
library(sf)
library(openxlsx)

#Importing NISTEP data
nistep <- read_delim(
  "~/DBASES/Patent Data/JIPO/NISTEP/NISTEP_JIPO.txt"
) %>%
  mutate(comp_id = as.integer(comp_id))

#Importing hr data

hr <- read_delim_arrow(
  "~/DBASES/Patent Data/JIPO/2024/2024a/hr.txt",
  delim = "\t",
  schema = schema(
    ida = string(),
    seq = string(),
    ida_seq = string(),
    name = string(),
    address = string(),
    idname = string()
  ),
  skip = 1L
)

#Importing the analysis results
#load(file = "final_analysis_data.RData")
load(file = "final_results_data.RData")

#Adding all MPA patents into one dataframe
hr_mpa_nistep <- map_dfr(
  final_res,
  \(.x) {
    .x$mpa %>%
      activate(nodes) %>%
      as_tibble()
  },
  .id = "original_level"
) %>%
  select(ida, name_level3, concat_level3, green) %>%
  distinct(ida, .keep_all = TRUE) %>%
  inner_join(
    hr,
    by = "ida"
  ) %>%
  left_join(
    nistep,
    by = "ida_seq"
  )

#Inventors MPA

inv_mpa <- map_dfr(
  final_res,
  \(.x) {
    .x$mpa %>%
      activate(nodes) %>%
      as_tibble()
  },
  .id = "original_level"
) %>%
  select(-c("name_level3", "concat_level3"))

#Writing mpa_inv data

openxlsx::write.xlsx(inv_mpa, "01_Data/08_mpa_results/inv_mpa.xlsx")

#Writing mpa data with the nistep general information

write_csv(hr_mpa_nistep, "01_Data/07_NISTEP/hr_mpa_nistep.csv")

#Reading the mpa_nistep data

hr_mpa_nistep <- read_csv("01_Data/07_NISTEP/hr_mpa_nistep.csv") %>%
  mutate(ida = as.character(ida), comp_id = as.integer(comp_id)) %>%
  rename(name_jipo = name.x, name_nistep = name.y, address_jipo = address) %>%
  mutate(ayear = as.integer(year(adate))) %>%
  select(
    ida,
    name_level3,
    concat_level3,
    green,
    adate,
    ayear,
    ida_seq,
    comp_id,
    comp_name,
    name_jipo,
    name_nistep,
    address_jipo
  )

#Reading NISTEP company tables

nistep_address <- read_delim(
  "~/DBASES/Patent Data/JIPO/NISTEP/NISTEP_Company/3_address_TBL.txt",
  na = c("\\N")
) %>%
  filter(
    comp_id %in% hr_mpa_nistep$comp_id,
    office_code == 1 | office_code == 2
  ) %>%
  mutate(
    across(where(is.double) & !c(latitude, longitude), as.integer)
  ) %>%
  arrange(-desc(comp_id)) %>%
  select(-c("reg_date", "update", "cd_year"))

#Midstep dataframe with different addresses

hr_mpa_nistep_address <- hr_mpa_nistep %>%
  left_join(
    nistep_address,
    by = "comp_id",
    relationship = "many-to-many"
  ) %>%
  rename(address_nistep = address)

#Selecting closest size

nistep_size <- read_delim(
  "~/DBASES/Patent Data/JIPO/NISTEP/NISTEP_Company/4_comp_size_TBL.txt",
  na = c("\\N")
) %>%
  filter(comp_id %in% hr_mpa_nistep$comp_id) %>%
  mutate(
    across(where(is.double), as.integer)
  ) %>%
  select(1:5)

matched_data <- hr_mpa_nistep_address %>%
  mutate(row_id = row_number()) %>% # distinct ID to track original rows
  left_join(nistep_size, by = "comp_id", relationship = "many-to-many") %>%
  mutate(diff = abs(ayear - judg_year)) %>%
  group_by(row_id) %>%
  arrange(diff) %>%
  slice(1) %>%
  ungroup() %>%
  select(-row_id, -diff)

nistep_industry <- read_delim(
  "~/DBASES/Patent Data/JIPO/NISTEP/NISTEP_Company/6_ind_class_jsic_TBL.txt",
  na = c("\\N")
) %>%
  filter(comp_id %in% hr_mpa_nistep$comp_id) %>%
  mutate(
    across(where(is.double), as.integer)
  ) %>%
  select(1:4)


nistep_industry_cat <- read_delim(
  "~/DBASES/Patent Data/JIPO/NISTEP/NISTEP_Company/61_jsic_MTBL.txt",
  na = c("\\N")
) %>%
  mutate(
    across(where(is.double), as.integer)
  ) %>%
  select(1:5)

nistep_listed <- read_delim(
  "~/DBASES/Patent Data/JIPO/NISTEP/NISTEP_Company/8_sec_code_TBL.txt",
  na = c("\\N")
) %>%
  filter(comp_id %in% hr_mpa_nistep$comp_id) %>%
  mutate(
    across(where(is.double), as.integer)
  ) %>%
  select(1:7)

nistep_data <- read_delim(
  "~/DBASES/Patent Data/JIPO/NISTEP/NISTEP_Company/1_comp_name_main_TBL.txt",
  na = c("\\N")
) %>%
  select(comp_id, comp_name, e_name) %>%
  mutate(comp_id = as.integer(comp_id))

nistep_consolidate <- read_delim(
  "~/DBASES/Patent Data/JIPO/NISTEP/NISTEP_Company/9_consolidate_TBL.txt",
  na = c("\\N")
) %>%
  filter(comp_id %in% hr_mpa_nistep$comp_id) %>%
  mutate(
    across(where(is.double), as.integer)
  ) %>%
  left_join(
    nistep_data,
    by = join_by(parent_compid == comp_id)
  ) %>%
  distinct(comp_id, .keep_all = TRUE) %>%
  select(comp_id, parent_compid, comp_name, e_name) %>%
  rename(parent_comp_name = comp_name)

#Public organizations

nistep_public_dic <- nistep_public <- read_delim(
  "~/DBASES/Patent Data/JIPO/NISTEP/Public_institution/NISTEP_public_dictionary.txt"
) %>%
  mutate(low_org_id = as.integer(low_org_id))


nistep_public <- read_delim(
  "~/DBASES/Patent Data/JIPO/NISTEP/Public_institution/NISTEP_public_table.txt"
) %>%
  mutate(comp_id = as.integer(comp_id), low_org = as.integer(low_org)) %>%
  left_join(
    nistep_public_dic,
    by = join_by(low_org == low_org_id)
  )

#Final dataframe

final_data <- matched_data %>%
  mutate(row_id = row_number()) %>% # Track original rows again
  left_join(nistep_industry, by = "comp_id", relationship = "many-to-many") %>%
  mutate(diff_ind = abs(ayear - jsics_year)) %>%
  group_by(row_id) %>%
  arrange(diff_ind) %>%
  slice(1) %>% # Keep only the closest industry year match
  ungroup() %>%
  select(-row_id, -diff_ind) %>%
  left_join(
    nistep_industry_cat,
    by = "jsic_code"
  ) %>%
  left_join(
    nistep_listed,
    by = "comp_id"
  ) %>%
  left_join(
    nistep_consolidate,
    by = "comp_id"
  ) %>%
  left_join(
    nistep_public %>%
      distinct(comp_id, .keep_all = TRUE),
    by = "comp_id"
  )


openxlsx::write.xlsx(final_data, "01_Data/07_NISTEP/hr_mpa_nistep.xlsx")
