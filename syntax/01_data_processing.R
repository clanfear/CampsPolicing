library(tidyverse)

data_processing_scripts <- list.files("./syntax/data_processing/", full.names = TRUE)
for(script in data_processing_scripts){
  message(paste0("\n(",format(Sys.time(), format = "%H:%S"),") Running ", script, "\n"))
  source(script)
}
