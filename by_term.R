setwd("directory")

library(dplyr)
library(tidyr)
library(stringr)
library(scales)

View(tidy_data)

# 2) What are the pass rates for each term?

passrate = select(tidy_data, EnrollmentTerm, EnrolledCourseGrade_A, EnrolledCourseGrade_B, EnrolledCourseGrade_C)
View(passrate)

terms = passrate %>%
  pivot_longer(cols = starts_with("EnrolledCourseGrade"),
               names_to = "course_grade_column",
               values_to = "course_grade") %>%
  filter(!is.na(course_grade)) %>%  # Remove missing grades
  mutate(pass = ifelse(course_grade %in% c("A", "B", "C"), 1, 0)) %>%
  group_by(EnrollmentTerm)

View(terms)
summary(terms)

terms_passrate = terms %>%
  group_by(EnrollmentTerm) %>%
  summarise(
    total_students = n(),
    total_passed = sum(pass),
    pass_rate = total_passed / total_students
  )
# ALL pass rates per term
View(terms_passrate)
