library(dplyr)
library(stringr)

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

save(graffiti, file="./data/derived/graffiti.RData")
save(dumping, file="./data/derived/dumping.RData")
save(general, file="./data/derived/general.RData")

readr::write_csv(graffiti, path = "./data/derived/graffiti.csv")
readr::write_csv(dumping, path = "./data/derived/dumping.csv")
readr::write_csv(general, path = "./data/derived/general.csv")
