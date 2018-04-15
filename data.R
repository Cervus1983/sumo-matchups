# data.world (you'll need your own account)
library(data.world)
set_config(cfg_env(auth_token_var = "DW_API_TOKEN"))

library(tidyverse)

# wrapper for data frame concatenation
dfapply <- function(...) do.call(rbind, lapply(...))

# downloads entire data set into single data frame
dataset_as_data_frame <- function(dataset_name) {
	dataset <- paste(Sys.getenv("DW_USER"), dataset_name, sep = "/")
	dfapply(
		map_chr(get_dataset(dataset)$files, "name"),
		function(file_name) download_file_as_data_frame(dataset, file_name)
	)
}

"sumo-results" %>% 
	dataset_as_data_frame() %>% 
	write_csv("data.csv")
