tidy_mpie_data_with_instructor <- tidy_mpie_data_with_instructor %>%
  mutate(Instructor_Status = case_when(
    Post_MPIE ~ "Post_MPIE",
    MPIE_AllTerm == "N" ~ "Never_MPIE",
    TRUE ~ "Other"
  ))

# Filter to just Post_MPIE and Never_MPIE
filtered_data <- tidy_mpie_data_with_instructor %>%
  filter(Instructor_Status %in% c("Post_MPIE", "Never_MPIE"))

# Count students per term and status
student_counts <- filtered_data %>%
  group_by(EnrollmentTerm, Instructor_Status) %>%
  summarise(StudentCount = n(), .groups = "drop")

# Plot
ggplot(student_counts, aes(x = EnrollmentTerm, y = StudentCount, fill = Instructor_Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(
    title = "Students Taught by Post-MPIE vs Never-MPIE Instructors",
    x = "Enrollment Term",
    y = "Number of Students",
    fill = "Instructor Status"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
