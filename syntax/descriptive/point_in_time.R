library(tidyverse)

pit_counts <- map_dfr(1:14, ~ readxl::read_excel("./data/raw/2007-2020-PIT-Estimates-by-CoC.xlsx", sheet = .x) %>%
                        select(`CoC Name`, matches("Overall Homeless,")) %>%
                        janitor::clean_names(parsing_option = 0) %>%
                        janitor::clean_names() %>%
                        mutate(., year = as.numeric(str_extract(names(.)[2], "[\\d]+"))) %>%
                        rename_with(~str_remove_all(str_remove_all(., "\\d"), "_$")) %>%
                        filter(str_detect(coc_name, "Seattle")) %>%
                        select(-coc_name, year = year, pit_count = overall_homeless))

save(pit_counts, file = "./data/derived/other/pit_counts.RData")
