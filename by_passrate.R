setwd("directory")

# 1) What are the pass rates for each course? 

# MATH 101 and MATH 101 Supplementary
passrate = select(tidy_data, course_section, course_name, EnrolledCourseGrade_A, EnrolledCourseGrade_B, EnrolledCourseGrade_C)
View(passrate)

math101 = passrate %>%
  pivot_longer(cols = starts_with("EnrolledCourseGrade"),
               names_to = "course_grade_column",
               values_to = "course_grade") %>%
  filter(str_starts(course_name, "MATH-101"),
         str_starts(course_section, "EnrolledCourseSection_"),
         !is.na(course_grade)) %>%
  mutate(pass = ifelse(course_grade %in% c("A", "B", "C"), 1, 0))
View(math101)
summary(math101)

math101_passrate = math101 %>%
  group_by(course_name) %>%
  summarise(
    total_students = n(),
    total_passed = sum(pass),
    pass_rate = total_passed / total_students
  )
# ALL pass rates for ALL MATH 101 classes individually
View(math101_passrate)
overallmath101_passrate = percent(sum(math101_passrate$total_passed) / sum(math101_passrate$total_students))
print(overallmath101_passrate) 

# MATH 104 and MATH 104 Supplementary
passrate = select(tidy_data, course_section, course_name, EnrolledCourseGrade_A, EnrolledCourseGrade_B, EnrolledCourseGrade_C)
View(passrate)

math104 = passrate %>%
  pivot_longer(cols = starts_with("EnrolledCourseGrade"),
               names_to = "course_grade_column",
               values_to = "course_grade") %>%
  filter(str_starts(course_name, "MATH-104"),
         str_starts(course_section, "EnrolledCourseSection_"),
         !is.na(course_grade)) %>%
  mutate(pass = ifelse(course_grade %in% c("A", "B", "C"), 1, 0))
View(math104)
summary(math104)

math104_passrate = math104 %>%
  group_by(course_name) %>%
  summarise(
    total_students = n(),
    total_passed = sum(pass),
    pass_rate = total_passed / total_students
  )
# ALL pass rates for ALL MATH 104 classes individually
View(math104_passrate)
overallmath104_passrate = percent(sum(math104_passrate$total_passed) / sum(math104_passrate$total_students))
print(overallmath104_passrate) 

# MATH 244
passrate = select(tidy_data, course_section, course_name, EnrolledCourseGrade_A, EnrolledCourseGrade_B, EnrolledCourseGrade_C)
View(passrate)

math244 = passrate %>%
  pivot_longer(cols = starts_with("EnrolledCourseGrade"),
               names_to = "course_grade_column",
               values_to = "course_grade") %>%
  filter(str_starts(course_name, "MATH-244"),
         str_starts(course_section, "EnrolledCourseSection_"),
         !is.na(course_grade)) %>%
  mutate(pass = ifelse(course_grade %in% c("A", "B", "C"), 1, 0))
View(math244)
summary(math244)

math244_passrate = math244 %>%
  group_by(course_name) %>%
  summarise(
    total_students = n(),
    total_passed = sum(pass),
    pass_rate = total_passed / total_students
  )
# ALL pass rates for ALL MATH 244 classes individually
View(math244_passrate)
overallmath244_passrate = percent(sum(math244_passrate$total_passed) / sum(math244_passrate$total_students))
print(overallmath244_passrate) 

# MATH 250 and MATH 250 Supplementary
passrate = select(tidy_data, course_section, course_name, EnrolledCourseGrade_A, EnrolledCourseGrade_B, EnrolledCourseGrade_C)
View(passrate)

math250 = passrate %>%
  pivot_longer(cols = starts_with("EnrolledCourseGrade"),
               names_to = "course_grade_column",
               values_to = "course_grade") %>%
  filter(str_starts(course_name, "MATH-250"),
         str_starts(course_section, "EnrolledCourseSection_"),
         !is.na(course_grade)) %>%
  mutate(pass = ifelse(course_grade %in% c("A", "B", "C"), 1, 0))
View(math250)
summary(math250)

math250_passrate = math250 %>%
  group_by(course_name) %>%
  summarise(
    total_students = n(),
    total_passed = sum(pass),
    pass_rate = total_passed / total_students
  )
# ALL pass rates for ALL MATH 250 classes individually
View(math250_passrate)
overallmath250_passrate = percent(sum(math250_passrate$total_passed) / sum(math250_passrate$total_students))
print(overallmath250_passrate) 

# MATH 251
passrate = select(tidy_data, course_section, course_name, EnrolledCourseGrade_A, EnrolledCourseGrade_B, EnrolledCourseGrade_C)
View(passrate)

math251 = passrate %>%
  pivot_longer(cols = starts_with("EnrolledCourseGrade"),
               names_to = "course_grade_column",
               values_to = "course_grade") %>%
  filter(str_starts(course_name, "MATH-251"),
         str_starts(course_section, "EnrolledCourseSection_"),
         !is.na(course_grade)) %>%
  mutate(pass = ifelse(course_grade %in% c("A", "B", "C"), 1, 0))
View(math251)
summary(math251)

math251_passrate = math251 %>%
  group_by(course_name) %>%
  summarise(
    total_students = n(),
    total_passed = sum(pass),
    pass_rate = total_passed / total_students
  )
# ALL pass rates for ALL MATH 251 classes individually
View(math251_passrate)
overallmath251_passrate = percent(sum(math251_passrate$total_passed) / sum(math251_passrate$total_students))
print(overallmath251_passrate) 
