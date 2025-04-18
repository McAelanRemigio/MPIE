setwd("directory")
 
library(dplyr)
library(tidyr)
library(stringr)
library(scales)

tidy_data = merged_data %>%
  pivot_longer(cols = starts_with("EnrolledCourseSection"),  # Identify course columns
               names_to = "course_section",                  # Column name for the course sections
               values_to = "course_name") %>%                # Column name for the actual course names
  drop_na(course_name)  # Remove rows where there is no course (optional)

# Check the tidy data
View(tidy_data)
sort(unique(tidy_data$course_name)) # used to find out every unique MATH class
# MATH 101, MATH 104, MATH 244, MATH 250, MATH 251

# Save the tidy data if necessary
write_csv(tidy_data, "tidy_data.csv")
