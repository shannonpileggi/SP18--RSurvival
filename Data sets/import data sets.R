# load stringr ----
library(stringr)
library(purrr)

# list all files ----
all_files <- list.files(paste0(getwd(), "/Data sets/"), recursive = T)

# only include files that end in .txt ----
all_data_files <- all_files[str_detect(all_files, ".txt")]

# create data set names ----
ds_names <- str_to_lower(str_split(all_data_files, pattern = ".txt", simplify = TRUE)[[1]])

# create read_txt function ----
read_txt <- function(file_name) {
  read.table(file = paste0(getwd(), "/Data sets/", file_name),
             header = T)
}

# works for first data set ----
aggressive <- read_txt(all_data_files[1])

# create null list ----
survival_data <- list()

# import all data sets into a list ----
for (i in 1:length(all_data_files)){
  survival_data[[i]] <- read_txt(all_data_files[i])
}



purrr::map(survival_data, read_txt)

