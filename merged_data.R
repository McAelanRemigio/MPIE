setwd("directory")

# for efficient data handling 
library(dplyr)
library(readr)

# reading and merging CSV files automatically
file_list = list.files(pattern = "*.csv")
print(file_list)

merged_data = file_list %>% 
  lapply(function(file) {
    data <- read_csv(file)
    return(data)
  }) %>% 
  bind_rows()
# check merged data
View(merged_data)

# save merged data file
write_csv(merged_data, "merged_data.csv")
