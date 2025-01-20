# Load required libraries
library(dplyr)
library(tidyr)
library(readr)

# Load data
df_physicians <- read_csv("path/to/physicians.csv") # Update with correct path
df_ref_all <- read_csv("path/to/movers_referrals.csv") # Update with correct path

# Focus on the following variables
df_physicians <- df_physicians %>%
  select(npi, grad_year, med_school) %>%
  distinct(npi, .keep_all = TRUE) %>%
  mutate(med_school = ifelse(med_school == "OTHER", NA, med_school),
         npi = as.integer(npi))

# Merge with Referrals
df_ref_all <- df_ref_all %>%
  left_join(df_physicians, by = c("doctor" = "npi")) %>%
  rename_with(~paste0(.x, "_doc"), ends_with("_x")) %>%
  left_join(df_physicians, by = c("specialist" = "npi")) %>%
  rename_with(~paste0(.x, "_spec"), ends_with("_y"))

# Create additional variables
df_ref_all <- df_ref_all %>%
  mutate(
    same_sex = ifelse(sex_doc == sex_spec, 1, 0),
    diff_age = abs(as.numeric(birth_doc) - as.numeric(birth_spec)),
    diff_gradyear = abs(as.numeric(grad_year_doc) - as.numeric(grad_year_spec)),
    same_school = ifelse(med_school_doc == med_school_spec, 1, 0),
    same_prac = ifelse(group_doc == group_spec, 1, 0),
    same_zip = ifelse(zip_doc == zip_spec, 1, 0)
  )

# Add same sex details
df_ref_all <- df_ref_all %>%
  mutate(
    same_male = ifelse(sex_doc == "M" & same_sex == 1, 1, 0),
    same_female = ifelse(sex_doc == "F" & same_sex == 1, 1, 0)
  )

# Prepare data for logistic regression
zmat <- list()
rmat <- list()

unique_years <- unique(df_ref_all$year)
unique_hrrs <- unique(df_ref_all$hrr_doc)

for (year in unique_years) {
  for (hrr in unique_hrrs) {
    df <- df_ref_all %>%
      filter(year == year & hrr_doc == hrr)

    n <- n_distinct(df$doctor)
    m <- n_distinct(df$specialist)

    rho <- as.integer(n * (n - 1) * m * (m - 1) / 2)

    docs <- unique(df$doctor)
    specs <- unique(df$specialist)

    # Logistic regression data frame preparation
    zz <- numeric(rho)
    rr <- matrix(0, nrow = rho, ncol = 12) # Adjust dimensions

    # Populate zz and rr as per Python logic
    # Logic for filling zz and rr will depend on further specifications
  }
}

# Save final output for logistic regression
write_csv(df_ref_all, "path/to/df_logit.csv") # Update path as needed
