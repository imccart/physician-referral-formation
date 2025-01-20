# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)

# Set the working directory (update path as necessary)
setwd("/path/to/your/directory")

# Load the dataset
df_ref_initial <- read_csv("temp_data/movers_referrals.csv")

# Group by 'year' and 'hrr_doc' and count unique physicians
hrr_physicians <- df_ref_initial %>%
  group_by(year, hrr_doc) %>%
  summarise(unique_doctors = n_distinct(doctor), .groups = "drop")

# Filter and visualize the number of HRRs per physician count
hrr_distribution <- hrr_physicians %>%
  filter(unique_doctors > 1) %>%
  count(unique_doctors)

# Bar plot for the distribution
ggplot(hrr_distribution, aes(x = as.factor(unique_doctors), y = n)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Number of HRRs per number of physicians",
    x = "Number of Physicians",
    y = "Number of HRRs"
  ) +
  theme_minimal()

# Add visualization
ggsave("hrr_distribution_plot.png")

# Creating additional features
df_ref_initial <- df_ref_initial %>%
  mutate(
    male_spec = ifelse(sex_spec == "M", 1, 0),
    male_doc = ifelse(sex_doc == "M", 1, 0),
    exp_spec = year - grad_year_spec
  )

# Example for grouping and aggregation (replace with actual logic as needed)
agg_data <- df_ref_initial %>%
  group_by(year) %>%
  summarise(
    total_referrals = sum(referral, na.rm = TRUE),
    avg_diff_gradyear = mean(diff_gradyear, na.rm = TRUE)
  )

# Export processed data
write_csv(df_ref_initial, "processed_data.csv")
write_csv(agg_data, "aggregated_data.csv")

# Placeholder for MD-PPAS data processing (if applicable)
# mdppas <- read_csv("path/to/PhysicianData_2012.csv")
# mdppas <- mdppas %>%
#   select(npi, sex, birth_dt) %>%
#   mutate(phy_zip1 = str_pad(phy_zip1, 5, pad = "0")) %>%
#   drop_na()

# Uncomment to show top 20 specialties (if the data includes 'spec')
# top_specialties <- mdppas %>%
#   count(spec, sort = TRUE) %>%
#   mutate(percentage = n / sum(n) * 100) %>%
#   slice_max(n = 20, order_by = n)
# print(top_specialties)
