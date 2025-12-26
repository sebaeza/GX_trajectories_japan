library(tidyverse)
library(arrow)
library(dplyr)
library(readr)
library(purrr)
library(stringr)

#Importing the big databases using arrow
##Importing the JIPO patent data
ap <- read_delim_arrow(
  "~/Desktop/RESEARCH/JIPO/2024/2024a/ap.txt",
  delim = "\t",
  schema = schema(
    ida = string(),
    adate = string(),
    sdate = string(),
    idr = string(),
    rdate = string(),
    tdate = string(),
    class1 = string(),
    group1 = string(),
    class2 = string(),
    group2 = string(),
    claim1 = int64(),
    claim2 = int64(),
    claim3 = int64()
  ),
  skip = 1L,
  as_data_frame = FALSE
)

##Importing the JIPO inventor data

inventor <- read_delim_arrow(
  "~/Desktop/RESEARCH/JIPO/2024/2024a/inventor.txt",
  delim = "\t",
  schema = schema(
    ida = string(),
    seq = string(),
    ida_seq = string(),
    name = string(),
    address = string()
  ),
  skip = 1L,
  as_data_frame = FALSE
)

##Importing the JIPO patent holder data

hr <- read_delim_arrow(
  "~/Desktop/RESEARCH/JIPO/2024/2024a/hr.txt",
  delim = "\t",
  schema = schema(
    ida = string(),
    seq = string(),
    ida_seq = string(),
    name = string(),
    address = string(),
    idname = string()
  ),
  skip = 1L,
  as_data_frame = FALSE
)

##Importing the JIPO citation data

citation <- read_delim_arrow(
  "~/Desktop/RESEARCH/JIPO/2024/2024a/cc.txt",
  delim = "\t",
  as_data_frame = FALSE
)

##Importing PATSTAT tables
##Importing patent data table

tls_201 <- open_csv_dataset(
  "~/Desktop/RESEARCH/PATSTAT/tables/tls_201",
  schema = schema(
    appln_id = string(),
    appln_auth = string(),
    appln_nr = string(),
    appln_kind = string(),
    appln_filing_date = date32(),
    appln_filing_year = string(),
    appln_nr_epodoc = string(),
    appln_nr_original = string(),
    ipr_type = string(),
    receiving_office = string(),
    internat_appln_id = int64(),
    int_phase = string(),
    reg_phase = string(),
    nat_phase = string(),
    earliest_filing_date = date32(),
    earliest_filing_year = int64(),
    earliest_filing_id = string(),
    earliest_publn_date = date32(),
    earliest_publn_year = int64(),
    earliest_pat_publn_id = string(),
    granted = string(),
    docdb_family_id = int64(),
    inpadoc_family_id = int64(),
    docdb_family_size = int64(),
    nb_citing_docdb_fam = int64(),
    nb_applicants = int64(),
    nb_inventors = int64()
  ),
  skip = 1L
)

##Importing inventor-patent connection for applications

tls_207 <- open_csv_dataset(
  "~/Desktop/RESEARCH/PATSTAT/tables/tls_207",
  schema = schema(
    person_id = string(),
    appln_id = string(),
    applt_seq_nr = int64(),
    invt_seq_nr = int64(),
  ),
  skip = 1L
)

##Importing inventor table

tls_206 <- open_csv_dataset(
  "~/Desktop/RESEARCH/PATSTAT/tables/tls_206",
  schema = schema(
    person_id = string(),
    person_name = string(),
    person_name_orig_lg = string(),
    person_address = string(),
    person_ctry_code = string(),
    nuts = string(),
    nuts_level = int64(),
    doc_std_name_id = string(),
    doc_std_name = string(),
    psn_id = string(),
    psn_name = string(),
    psn_level = string(),
    psn_sector = string(),
    han_id = string(),
    han_name = string(),
    han_harmonized = string()
  ),
  skip = 1L
)

#Importing the rest of the tables that include the sector information for the PATSTAT Database

tls_222 <- open_csv_dataset(
  "~/Desktop/RESEARCH/PATSTAT/tables/tls_222",
  schema = schema(
    appln_id = string(),
    jp_class_scheme = string(),
    jp_class_symbol = string()
  ),
  skip = 1L
)

tls_209 <- open_csv_dataset(
  "~/Desktop/RESEARCH/PATSTAT/tables/tls_209",
  schema = schema(
    appln_id = string(),
    ipc_class_symbol = string(),
    ipc_class_level = string(),
    ipc_version = date32(),
    ipc_value = string(),
    ipc_position = string(),
    ipc_gener_auth = string(),
  ),
  skip = 1L
)

tls_901 <- open_csv_dataset(
  "~/Desktop/RESEARCH/PATSTAT/tables/tls_901",
  schema = schema(
    ipc_maingroup_symbol = string(),
    techn_field_nr = int64(),
    techn_sector = string(),
    techn_field = string()
  ),
  skip = 1L
)

tls_902 <- open_csv_dataset(
  "~/Desktop/RESEARCH/PATSTAT/tables/tls_902",
  schema = schema(
    ipc = string(),
    not_with_ipc = string(),
    unless_with_ipc = string(),
    nace2_code = string(),
    nace2_weight = int64(),
    nace2_descr = string()
  ),
  skip = 1L
)

tls_202 <- open_csv_dataset(
  "~/Desktop/RESEARCH/PATSTAT/tables/tls_202",
  schema = schema(
    appln_id = string(),
    appln_title_lg = string(),
    appln_title = string()
  ),
  skip = 1L
)

tls_203 <- open_csv_dataset(
  "~/Desktop/RESEARCH/PATSTAT/tables/tls_203",
  schema = schema(
    appln_id = string(),
    appln_abstract_lg = string(),
    appln_abstract = string()
  ),
  skip = 1L
)

#Importing GXTI Categories

# 2. Read and combine
gxti <- map_dfr(
  list.files(
    "01_Data/03_GXTI",
    pattern = "\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  ),
  function(path) {
    # Read the CSV
    data <- read_csv(path, show_col_types = FALSE)

    # Remove base folder from path and split
    rel_path <- str_remove(path, "^03_GXTI/")
    path_parts <- str_split(rel_path, "/")[[1]]

    # Last part is the filename; the rest are folder levels
    file_name <- path_parts[length(path_parts)]
    folder_levels <- path_parts[-length(path_parts)]

    # Make a named list of folder level columns
    folder_cols <- set_names(
      as.list(folder_levels),
      paste0("folder_lvl", seq_along(folder_levels))
    )

    # Add folder columns and file name to the data
    data %>% mutate(!!!folder_cols, file_name = file_name)
  }
) |>
  mutate(
    Application_number_clean = str_remove_all(`Application number`, "[,-]"),
    Document_number_clean = str_remove_all(`Document number`, ","),
    Application_number_digits_only = str_remove_all(
      Application_number_clean,
      "[^0-9]"
    ),
    Registration_number_digits_only = str_remove_all(
      `Registration number`,
      "[^0-9]"
    )
  )
