# What are the pass rates for each course? 

data = data %>%
  mutate(pass = ifelse(Grade %in% c("A", "B", "C", "P"), 1, 0))

data = data %>% mutate(course_name = sub("-[A-Za-z0-9]+$", "", data$course_name))

passrate = data %>% mutate(pass = ifelse(Grade %in% c ("A", "B", "C", "P"), 1, 0)) %>%
  group_by(course_name) %>%
  summarise(
    total = n(),
    pass= sum(pass),
    passrate = (pass / total) * 100
  )
passrate
# View(passrate)

ggplot(passrate, aes(x = reorder(course_name, passrate), y = passrate)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Pass Rates by Course", x = "Course Name", y = "Pass Rate (%)") +
  ylim(0, 100) +
  theme_minimal()
