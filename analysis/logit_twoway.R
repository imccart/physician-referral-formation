# Load required libraries
library(dplyr)
library(readr)

# Set working directory (update to your directory)
setwd("/path/to/your/directory")

# Load the dataset
df_ref_all <- read_csv("temp_data/movers_referrals.csv")

# Filter data for a specific HRR and year
df <- df_ref_all %>%
  filter(hrr_doc == 421, year == 2018)

# Summary statistics
n <- n_distinct(df$doctor)
m <- n_distinct(df$specialist)
docs <- unique(df$doctor)
specs <- unique(df$specialist)

# Prepare data for logistic regression
y <- df %>%
  select(doctor, specialist, referral)

x <- df %>%
  select(doctor, specialist, same_sex, diff_age, diff_gradyear, same_school, same_prac)

k <- ncol(x) - 2
rho <- as.integer(n * (n - 1) * m * (m - 1) / 2)
cat("Value of rho:", rho, "\n")

# Initialize arrays for computation
zz <- matrix(0, nrow = rho, ncol = 1)
rr <- matrix(0, nrow = rho, ncol = k)
ss <- numeric(rho)

# Perform the two-way logit transformation
c <- 1
for (i1 in docs) {
  docs_remaining <- setdiff(docs, i1)
  
  for (j1 in specs) {
    specs_remaining <- setdiff(specs, j1)
    
    y11 <- y %>% filter(doctor == i1 & specialist == j1) %>% pull(referral)
    x11 <- x %>% filter(doctor == i1 & specialist == j1) %>% select(-doctor, -specialist)
    
    for (i2 in docs_remaining) {
      y21 <- y %>% filter(doctor == i2 & specialist == j1) %>% pull(referral)
      x21 <- x %>% filter(doctor == i2 & specialist == j1) %>% select(-doctor, -specialist)
      
      for (j2 in specs_remaining) {
        y12 <- y %>% filter(doctor == i1 & specialist == j2) %>% pull(referral)
        x12 <- x %>% filter(doctor == i1 & specialist == j2) %>% select(-doctor, -specialist)
        y22 <- y %>% filter(doctor == i2 & specialist == j2) %>% pull(referral)
        x22 <- x %>% filter(doctor == i2 & specialist == j2) %>% select(-doctor, -specialist)
        
        ss[c] <- as.integer(((y11 > y12) & (y21 < y22)) | ((y11 < y12) & (y21 > y22)))
        zz[c] <- (y11 - y12) - (y21 - y22)
        rr[c, ] <- as.numeric((x11 - x12) - (x21 - x22))
        
        c <- c + 1
      }
    }
  }
}

# Normalize zz and filter based on ss
zz <- zz / 2
zzz <- zz[ss == 1]
zzz <- (zzz + 1) / 2
rrr <- rr[ss == 1, ]

# Save outputs
write_csv(data.frame(zzz, rrr), "twoway_logit_data.csv")
