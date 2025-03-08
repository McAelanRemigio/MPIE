setwd("directory")

library(dplyr)
library(tidyr)
library(stringr)
library(scales)

View(tidy_data)

# 1) What are the pass rates by instructor

passrate = select(tidy_data, InstructorName_A, InstructorName_B, EnrolledCourseGrade_A, EnrolledCourseGrade_B)
View(passrate)

instructor = passrate %>%
  pivot_longer(cols = c(EnrolledCourseGrade_A, EnrolledCourseGrade_B),
               names_to = "course_grade_column",
               values_to = "course_grade") %>%
  pivot_longer(cols = c(InstructorName_A, InstructorName_B),
               names_to = "instructorname_A_or_B",
               values_to = "instructor_name") %>%
  filter(!is.na(course_grade)) %>%  # Remove missing grades
  mutate(pass = ifelse(course_grade %in% c("A", "B", "C"), 1, 0))

View(instructor)
summary(instructor)

instructor_passrate = instructor %>%
  group_by(instructor_name) %>%
  summarise(
    total_students = n(),
    total_passed = sum(pass),
    pass_rate = total_passed / total_students
  )
# ALL pass rates per instructor
View(instructor_passrate)
