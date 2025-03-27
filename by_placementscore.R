# Normalize Placement Scores to a scale of 0-1 
data = data %>%
  mutate(
    PlacementScore_BSTEM_norm = PlacementScore_BSTEM / 9,   
    PlacementScore_SLAM_norm = PlacementScore_SLAM / 3,   
    PlacementScore_MDTP_norm = PlacementScore_MDTP / 27,
    PlacementScore_ESL_norm = PlacementScore_ESL / 6        
  )

data = data %>%
  mutate(pass = ifelse(Grade %in% c("A", "B", "C", "P"), 1, 0))

# Correlation between normalized placement scores and pass/fail outcome
correlation_bstem = cor(data$PlacementScore_BSTEM_norm, data$pass, use = "complete.obs")
correlation_slam = cor(data$PlacementScore_SLAM_norm, data$pass, use = "complete.obs")
correlation_mdtp = cor(data$PlacementScore_MDTP_norm, data$pass, use = "complete.obs")
correlation_esl = cor(data$PlacementScore_ESL_norm, data$pass, use = "complete.obs")

print(correlation_bstem)
print(correlation_slam)
print(correlation_mdtp)
print(correlation_esl)


ggplot(data) +
  geom_histogram(aes(x = PlacementScore_BSTEM_norm), bins = 30, fill = "steelblue", alpha = 0.6) +
  labs(title = "Distribution of BSTEM Placement Scores (Normalized)", x = "Normalized Score (0-1)", y = "Frequency") +
  theme_minimal()

ggplot(data) +
  geom_histogram(aes(x = PlacementScore_SLAM_norm), bins = 30, fill = "steelblue", alpha = 0.6) +
  labs(title = "Distribution of SLAM Placement Scores (Normalized)", x = "Normalized Score (0-1)", y = "Frequency") +
  theme_minimal()

ggplot(data) +
  geom_histogram(aes(x = PlacementScore_MDTP_norm), bins = 30, fill = "steelblue", alpha = 0.6) +
  labs(title = "Distribution of MDTP Placement Scores (Normalized)", x = "Normalized Score (0-1)", y = "Frequency") +
  theme_minimal()

ggplot(data) +
  geom_histogram(aes(x = PlacementScore_ESL_norm), bins = 30, fill = "steelblue", alpha = 0.6) +
  labs(title = "Distribution of ESL Placement Scores (Normalized)", x = "Normalized Score (0-1)", y = "Frequency") +
  theme_minimal()
