library(tidyverse)
library(sf)
library(lubridate)

load("./data/derived/tract/seattle_tract_boundaries.RData")
load("./data/derived/bg/seattle_bg_boundaries.RData")
load("./data/derived/block/seattle_block_boundaries.RData")

read_sheets  <- function(sheet_type){
  lookup     <- c("graffiti"=1, "dumping"=2, "general"=3)
  file_dir   <- "./data/raw/spu_reports/"
  file_names <- stringr::str_subset(list.files(file_dir), "xlsx")
  file_paths <- stringr::str_c(file_dir, file_names)
  col_types  <- list("graffiti" = "guess",
                     "dumping" = c("text", "text", "date", "text","text","text","numeric","numeric", "text","text","text","text","text","text"),
                     "general" = c("text", "text", "date", "text","text","text","numeric","numeric", "text","text","text","text","text"))
  if(sheet_type == "dumping"){
    file_paths <- file_paths[-stringr::str_detect(file_paths, "2012")]
  }
  sheet_out <- lapply(file_paths, function(x){ 
    readxl::read_excel(x, sheet = lookup[sheet_type],
                       col_types = col_types[[sheet_type]]) }) %>%
    dplyr::bind_rows() %>% 
    rename_all(~tolower(str_replace_all(str_remove_all(., "[\\?',]")," ", "_")))
  return(sheet_out)
}

graffiti <- read_sheets("graffiti")
dumping  <- read_sheets("dumping")
general  <- read_sheets("general")

save(graffiti, file="./data/derived/disaggregated/graffiti.RData")
save(dumping, file="./data/derived/disaggregated/dumping.RData")
save(general, file="./data/derived/disaggregated/general.RData")

unauthorized_camping_complaints <- general %>%
  filter(issue_category == "Unauthorized Camping")
save(unauthorized_camping_complaints, file = "./data/derived/disaggregated/unauthorized_camping_complaints.RData")

unauthorized_camping_complaints_monthly_tract <- unauthorized_camping_complaints %>%
  filter(!is.na(x_value) & !is.na(y_value) & x_value > 0 & y_value > 0 & !is.na(created_date)) %>%
  st_as_sf(coords = c("x_value", "y_value"), 
           crs = 2285,
           agr = "identity") %>%
  st_transform(crs = 3689) %>%
  mutate(year = year(created_date), month = month(created_date)) %>%
  st_join(seattle_tract_boundaries, join = st_nearest_feature) %>%
  st_drop_geometry() %>%
  mutate(complaints = 1) %>%
  select(tract, year, month, complaints) %>%
  group_by(tract, year, month) %>%
  summarize_all(~sum(.)) %>%
  ungroup() %>%
  complete(tract = seattle_tract_boundaries$tract, nesting(year, month), fill = list(complaints = 0)) %>%
  mutate(date = ym(paste(year, month, sep = "-"))) %>%
  select(-year, -month)

save(unauthorized_camping_complaints_monthly_tract, file = "./data/derived/tract/unauthorized_camping_complaints_monthly_tract.RData")


unauthorized_camping_complaints_monthly_bg <- unauthorized_camping_complaints %>%
  filter(!is.na(x_value) & !is.na(y_value) & x_value > 0 & y_value > 0 & !is.na(created_date)) %>%
  st_as_sf(coords = c("x_value", "y_value"), 
           crs = 2285,
           agr = "identity") %>%
  st_transform(crs = 3689) %>%
  mutate(year = year(created_date), month = month(created_date)) %>%
  st_join(seattle_bg_boundaries, join = st_nearest_feature) %>%
  st_drop_geometry() %>%
  mutate(complaints = 1) %>%
  select(blockgroup, year, month, complaints) %>%
  group_by(blockgroup, year, month) %>%
  summarize_all(~sum(.)) %>%
  ungroup() %>%
  complete(blockgroup = seattle_bg_boundaries$blockgroup, nesting(year, month), fill = list(complaints = 0)) %>%
  mutate(date = ym(paste(year, month, sep = "-")))%>%
  select(-year, -month)

save(unauthorized_camping_complaints_monthly_bg, file = "./data/derived/bg/unauthorized_camping_complaints_monthly_bg.RData")


unauthorized_camping_complaints_monthly_block <- unauthorized_camping_complaints %>%
  filter(!is.na(x_value) & !is.na(y_value) & x_value > 0 & y_value > 0 & !is.na(created_date)) %>%
  st_as_sf(coords = c("x_value", "y_value"), 
           crs = 2285,
           agr = "identity") %>%
  st_transform(crs = 3689) %>%
  mutate(year = year(created_date), month = month(created_date)) %>%
  st_join(seattle_block_boundaries, join = st_nearest_feature) %>%
  st_drop_geometry() %>%
  mutate(complaints = 1) %>%
  select(block, year, month, complaints) %>%
  group_by(block, year, month) %>%
  summarize_all(~sum(.)) %>%
  ungroup() %>%
  complete(block = seattle_block_boundaries$block, nesting(year, month), fill = list(complaints = 0)) %>%
  mutate(date = ym(paste(year, month, sep = "-")))%>%
  select(-year, -month)

save(unauthorized_camping_complaints_monthly_block, file = "./data/derived/block/unauthorized_camping_complaints_monthly_block.RData")