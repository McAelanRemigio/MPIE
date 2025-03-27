setwd("directory")
 
library(dplyr)
library(tidyr)
library(stringr)
library(scales)

View(tidy_data)

# What are the pass rates by instructor
instructor_passrate = data %>%
  mutate(pass = ifelse(Grade %in% c("A", "B", "C", "P"), 1, 0)) %>%
  gather(key = "Instructor_Type", value = "InstructorName", InstructorName) %>%
  group_by(InstructorName) %>%
  summarise(
    total_students = n(),
    total_passed = sum(pass),
    pass_rate = total_passed / total_students
  )

# View the pass rate per instructor
instructor_passrate
# View(instructor_passrate)

pic = ggplot(instructor_passrate, aes(x = reorder(InstructorName, pass_rate), y = pass_rate * 100)) +
  geom_col(fill = "steelblue") +
  coord_flip() +  
  labs(title = "Pass Rate by Instructor", x = "Instructor Name", y = "Pass Rate (%)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8, color = "white"))

ggsave("instructor_passrate.png", plot = pic, width = 14, height = 8, dpi = 300)  
