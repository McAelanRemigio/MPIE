# What are the pass rates for each course? 

data = data %>%
  mutate(pass = ifelse(EnrolledCourseGrade_A %in% c("A", "B", "C", "P") |
                       EnrolledCourseGrade_B %in% c("A", "B", "C", "P") |
                       EnrolledCourseGrade_C %in% c("A", "B", "C", "P"), 1, 0))

calculate_pass_rate = function(course_code) # course_code is a prameter that represents course identifier
{
  data %>%
    filter(str_starts(course_name, course_code),
           str_starts(course_section, "EnrolledCourseSection_")) %>%
    filter(!is.na(EnrolledCourseGrade_A) |
           !is.na(EnrolledCourseGrade_B) |
           !is.na(EnrolledCourseGrade_C)) %>% 
          group_by(course_name) %>%
          summarise(
          total_students = n(),
          total_passed = sum(pass),
          pass_rate = total_passed / total_students
          )
}

# Compute pass rates for each course
avgmath101 = mean(math101$pass_rate, na.rm = TRUE)
avgmath104 = mean(math104$pass_rate, na.rm = TRUE)
avgmath244 = mean(math244$pass_rate, na.rm = TRUE)
avgmath250 = mean(math250$pass_rate, na.rm = TRUE)
avgmath251 = mean(math251$pass_rate, na.rm = TRUE)

cat("Average pass rate for MATH 101: ", scales::percent(avgmath101), "\n")
cat("Average pass rate for MATH 104: ", scales::percent(avgmath104), "\n")
cat("Average pass rate for MATH 244: ", scales::percent(avgmath244), "\n")
cat("Average pass rate for MATH 250: ", scales::percent(avgmath250), "\n")
cat("Average pass rate for MATH 251: ", scales::percent(avgmath251), "\n")
