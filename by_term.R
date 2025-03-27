setwd("directory")

library(dplyr)
library(tidyr)
library(stringr)
library(scales)

View(tidy_data)

# What are the pass rates for each term?
terms_passrate = data %>% 
  mutate(pass = ifelse(Grade %in% c ("A", "B", "C", "P"), 1, 0)) %>% 
  group_by(EnrollmentTerm) %>%
  summarise(
    total_students = n(),
    total_passed = sum(pass),
    pass_rate = total_passed / total_students
  )

terms_passrate
# View(terms_passrate)

ggplot(terms_passrate, aes(x = EnrollmentTerm, y = pass_rate * 100, group = 1)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Pass Rate Trends Over Time", x = "Enrollment Term", y = "Pass Rate (%)") +
  ylim(0, 100) +
  theme_minimal()
