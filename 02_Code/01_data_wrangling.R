#Data wrangling combining data from the GXTI, JIPO and PATSTAT----

#source("02_Code/00_data_import.R")

#First filtering for all data included in the three databases
#JIPO is the base database. We search coincidences in this database

ap_gxti <- (function(df) {
  ap_JP <- ap |>
    filter(
      ida %in% df$ida
    ) |>
    select(
      ida
    ) |>
    collect()

  cc <- citation |>
    filter(
      citing %in% ap_JP$ida | cited %in% ap_JP$ida
    ) |>
    select(
      cited,
      citing
    ) |>
    collect()

  ap_gxti <- ap |>
    filter(
      ida %in% cc$cited | ida %in% cc$citing
    ) |>
    collect() |>
    select(
      ida
    ) |>
    mutate(
      ida_epodoc = str_replace(paste0("JP", ida), "(?<=^JP\\d{4})", "0")
    )

  inv_gxti <- inventor |>
    filter(
      ida %in% ap_gxti$ida
    ) |>
    collect() |>
    group_by(
      ida
    ) |>
    summarise(
      n_inventor = n_distinct(ida_seq)
    ) |>
    ungroup()

  ap_gxti <- ap_gxti |>
    left_join(
      inv_gxti,
      by = "ida"
    ) |>
    mutate(
      n_inventor = if_else(is.na(n_inventor), 0, n_inventor)
    )

  tls_201_jp <- tls_201 |>
    filter(
      str_starts(appln_nr_epodoc, "JP")
    ) |>
    select(
      appln_id,
      appln_nr_epodoc,
      nb_inventors
    ) |>
    collect() |>
    filter(
      appln_nr_epodoc %in% ap_gxti$ida_epodoc
    )

  ap_gxti <- ap_gxti |>
    inner_join(
      tls_201_jp,
      join_by(
        ida_epodoc == appln_nr_epodoc
      ),
      keep = TRUE
    ) |>
    filter(
      n_inventor != 0 & nb_inventors != 0
    )

  return(ap_gxti)
})(gxti)


inv_jipo <- inventor |>
  filter(
    ida %in% ap_gxti$ida
  ) |>
  collect() |>
  mutate(
    address = if_else(address == "（省略）", NA_character_, address)
  ) |>
  left_join(
    addresses_jp_2024,
    by = "address"
  )

inv_tls_207 <- tls_207 |>
  filter(
    appln_id %in% ap_gxti$appln_id
  ) |>
  filter(invt_seq_nr != 0) |>
  collect() |>
  left_join(
    ap_gxti |>
      select(
        ida,
        appln_id
      ),
    by = "appln_id"
  ) |>
  mutate(
    ida_seq = paste0(ida, "_", sprintf("%03d", invt_seq_nr))
  )

inv_tls_206 <- tls_206 |>
  filter(
    person_id %in% inv_tls_207$person_id
  ) |>
  collect()

psn_tls_206 <- tls_206 |>
  filter(
    psn_id %in% inv_tls_206$psn_id
  ) |>
  collect() |>
  filter(
    !is.na(person_address)
  ) |>
  select(
    psn_id,
    psn_name,
    person_address
  ) |>
  group_by(
    psn_id
  ) |>
  mutate(
    person_address = person_address
  ) |>
  distinct(
    psn_id,
    .keep_all = TRUE
  )

inv_tls_206 <- inv_tls_206 |>
  select(
    person_id,
    psn_id
  ) |>
  left_join(
    psn_tls_206,
    by = "psn_id"
  )

inv_tls_207 <- inv_tls_207 |>
  left_join(
    inv_tls_206,
    by = "person_id"
  )

inv_jipo_f <- inv_jipo |>
  left_join(
    inv_tls_207 |>
      select(
        ida_seq,
        appln_id,
        psn_id,
        psn_name,
        person_address
      ),
    by = "ida_seq"
  )

#Geocode addresses

inv_geo <- inv_jipo_f |>
  filter(
    is.na(address) & !is.na(person_address)
  ) |>
  tidygeocoder::geocode(
    person_address,
    lat = "lat",
    long = "long",
    method = "arcgis"
  )

inv_jipo_f <- inv_jipo_f |>
  left_join(
    inv_geo |>
      select(
        person_address,
        lat,
        long
      ) |>
      distinct(
        person_address,
        .keep_all = TRUE
      ),
    by = "person_address"
  ) |>
  mutate(
    x = if_else(!is.na(x), x, long),
    y = if_else(!is.na(y), y, lat)
  ) |>
  select(
    ida,
    appln_id,
    ida_seq,
    psn_id,
    name,
    psn_name,
    CITY_ENG,
    person_address,
    x,
    y
  ) |>
  group_by(
    psn_id
  ) |>
  fill(
    x,
    y,
    .direction = "downup"
  ) |>
  ungroup() |>
  group_by(
    ida
  ) |>
  fill(
    x,
    y,
    .direction = "downup"
  ) |>
  ungroup()


n_distinct(inv_jipo_f$ida)
#Filtering patents where is no address information and also in patents

inv_jipo_f <- inv_jipo_f |>
  filter(!is.na(x))

rm(inv_jipo_f)

ap_gxti_f <- ap_gxti |>
  filter(ida %in% inv_jipo_f$ida)

#Citation network filtered

cc <- citation |>
  filter(
    citing %in% ap_gxti_f$ida & cited %in% ap_gxti_f$ida
  ) |>
  collect()

#hr Filtered

hr_f <- hr |>
  filter(ida %in% ap_gxti_f$ida) |>
  filter(seq == "1") |>
  collect()

ap_gxti <- ap_gxti_f

inv_jipo <- inv_jipo_f

hr <- hr_f

gxti_general <- gxti

gxti <- split(gxti_general, gxti_general$concat_level3)


rm(
  list = setdiff(
    ls(),
    c("ap_gxti", "gxti", "cc", "inv_jipo", "hr", "gxti_general")
  )
)


save.image("data_wrangling.RData")

load("data_wrangling.RData")

#Last filter of patents with inventors without psn_id

inv_jipo <- inv_jipo |>
  filter(!is.na(psn_id))

ap_gxti <- ap_gxti |>
  filter(ida %in% inv_jipo$ida)

cc <- cc |>
  filter(citing %in% ap_gxti$ida & cited %in% ap_gxti$ida)

hr <- hr |>
  filter(ida %in% ap_gxti$ida)


save.image("data_wrangling.RData")

#Data wrangling for all data in the JIPO database combined with patstat----

source("02_Code/00_data_import_windows.R")

#Testing transforming all data available in both databases

ap_JP_f <- ap |>
  collect() |>
  mutate(
    jp_ida = paste0("JP", ida),
    ida_epodoc = str_replace(jp_ida, "(?<=^JP\\d{4})", "0"),
    .after = idr
  ) |>
  left_join(
    inventor |>
      group_by(ida) |>
      summarise(n_inventor = n_distinct(ida_seq)) |>
      ungroup() |>
      collect(),
    by = "ida"
  ) |>
  inner_join(
    tls_201 |>
      filter(str_starts(appln_nr_epodoc, "JP")) |>
      select(appln_id, appln_nr_epodoc, nb_inventors) |>
      collect() |>
      mutate(appln_nr_epodoc = str_extract(appln_nr_epodoc, "^JP\\d+")),
    join_by(ida_epodoc == appln_nr_epodoc, n_inventor == nb_inventors),
    keep = TRUE
  ) |>
  distinct(ida, .keep_all = TRUE)

gc()

#Getting inventor data

inventor_JP_f <- inventor |>
  filter(
    ida %in% ap_JP_f$ida
  ) |>
  collect() |>
  left_join(
    addresses_jp_2024 |>
      select(address, LocName, CITY_ENG, x, y),
    by = "address"
  ) |>
  left_join(
    ap_JP_f |>
      select(ida, appln_id),
    by = "ida"
  )

inventor_207_jp_f <- tls_207 |>
  filter(appln_id %in% ap_JP_f$appln_id) |>
  left_join(
    ap_JP_f |>
      select(ida, appln_id),
    by = "appln_id"
  ) |>
  filter(invt_seq_nr != 0) |>
  collect() |>
  mutate(ida_seq = paste0(ida, "_", sprintf("%03d", invt_seq_nr))) |>
  left_join(
    tls_206 |>
      select(person_id, psn_id, psn_name, han_id, han_name) |>
      collect(),
    by = "person_id"
  )

tls_206_JP <- tls_206 |>
  filter(psn_id %in% inventor_207_jp_f$psn_id) |>
  filter(!is.na(person_address)) |>
  select(psn_id, person_address) |>
  collect() |>
  distinct(psn_id, .keep_all = TRUE)

inventor_207_jp_final <- inventor_207_jp_f |>
  left_join(tls_206_JP, by = "psn_id")

inventor_JP_final <- inventor_JP_f |>
  left_join(
    inventor_207_jp_final |>
      select(ida_seq, psn_id, psn_name, person_address),
    by = "ida_seq"
  )

citation_patstat <- citation |>
  filter(cited %in% ap_JP_f$ida & citing %in% ap_JP_f$ida) |>
  select(cited, citing) |>
  filter(cited != citing) |>
  collect()

write.csv(
  ap_JP_f,
  "01_Data/05_cleaned/ap_patstat.csv",
  row.names = FALSE,
  fileEncoding = "utf-8"
)

write.csv(
  inventor_JP_final,
  "01_Data/05_cleaned/inventor_patstat.csv",
  row.names = FALSE,
  fileEncoding = "utf-8"
)

write.csv(
  citation_patstat,
  "01_Data/05_cleaned/citation_patstat.csv",
  row.names = FALSE,
  fileEncoding = "utf-8"
)
