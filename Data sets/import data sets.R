# load packages  ----
library(stringr)
library(purrr)

# list all files ----
all_files <- list.files(paste0(getwd(), "/Data sets/"), recursive = T)

# only include files that end in .txt ----
all_data_files <- all_files[str_detect(all_files, ".txt")]

# create data set names in lower case ----
ds_names <- str_to_lower(str_split(all_data_files, pattern = ".txt", simplify = TRUE)[,1])

# create read_txt function ----
read_txt <- function(file_name) {
  read.table(file = paste0(getwd(), "/Data sets/", file_name),
             header = T)
}

# import all data sets into a list ----
survival_data <- purrr::map(all_data_files, read_txt)

# give survival data names ----
names(survival_data) <- ds_names

# unpack data sets ----
list2env(survival_data, .GlobalEnv)

# remove unncessary objects from workspace ---
remove(survival_data, all_data_files, all_files, ds_names)
