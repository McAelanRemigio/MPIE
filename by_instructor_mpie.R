setwd("directory")

library(dplyr)
library(tidyr)
library(stringr)
library(scales)

View(tidy_data)
 
# 1) What are the pass rates by instructor by MPIE

passrate = select(tidy_data, InstructorName_A, InstructorName_B, EnrolledCourseGrade_A, EnrolledCourseGrade_B, MPIE_YN_A, MPIE_YN_B)
View(passrate)

instructor = passrate %>%
  pivot_longer(cols = c(EnrolledCourseGrade_A, EnrolledCourseGrade_B),
               names_to = "course_grade_column",
               values_to = "course_grade") %>%
  pivot_longer(cols = c(InstructorName_A, InstructorName_B),
               names_to = "instructorname_type",
               values_to = "instructor_name") %>%
  pivot_longer(cols = c(MPIE_YN_A, MPIE_YN_B),
               names_to = "MPIE_A_type",
               values_to = "MPIE") %>%
  filter(!is.na(course_grade)) %>%  # Remove missing grades
  mutate(pass = ifelse(course_grade %in% c("A", "B", "C"), 1, 0))

  instructor = instructor %>%
  mutate(MPIE = ifelse(MPIE %in% c("YN", "Y"), 1, 0)) 

MPIE_instructor_passrate = instructor %>%
  group_by(instructor_name, MPIE) %>%
  summarise(
    total_students = n(),
    total_passed = sum(pass),
    pass_rate = total_passed / total_students
  )
# ALL pass rates per instructor by MPIE
View(MPIE_instructor_passrate)
write_csv(MPIE_instructor_passrate, "MPIE_instructor_passrate.csv")
