# loading required package
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

# loading dataset
billionaires <- read.csv("Billionaires Statistics Dataset.csv")

# displaying Columns names
colnames(billionaires)

# inspecting structure
str(billionaires)

# missing values summary
colSums(is.na(billionaires))

# keeping only required columns and removing missing values
billionaires_clean <- billionaires %>% select(gender, finalWorth) %>%  
  filter(!is.na(gender), !is.na(finalWorth))

# converting gender to factor
billionaires_clean$gender <- factor(billionaires_clean$gender)

# checking sample sizes
table(billionaires_clean$gender)

# verifying clean data
summary(is.na(billionaires_clean))

# Statistical summary by gender
billionaires_clean %>% group_by(gender) %>% summarise(n = n(), mean_worth = mean(finalWorth), 
                      sd_worth = sd(finalWorth), median_worth = median(finalWorth))

# Plotting Histogram of Billionaire net worth
ggplot(billionaires_clean, aes(x = finalWorth)) +
  geom_histogram(binwidth = 5000, fill = "grey80", colour = "black") +
  labs(title = "Histogram of Billionaire Net Worth",
    x = "Net Worth (USD Millions)",
    y = "Frequency") + theme_minimal()

# Plotting Boxplot of Networth by Gender
ggplot(billionaires_clean, aes(x = gender, y = finalWorth)) +
  geom_boxplot() +
  labs(title = "Net Worth of Billionaires by Gender",
    x = "Gender",
    y = "Net Worth (USD Millions)") + theme_minimal()

# Inferential Statistics using t-test
t.test(finalWorth ~ gender, data = billionaires_clean, alternative = "two.sided")

# computing group statistics
billionaires_clean %>% group_by(gender) %>% summarise(
    n = n(), mean = mean(finalWorth), sd = sd(finalWorth))

# storing t-test result
t_test_result <- t.test(finalWorth ~ gender, data = billionaires_clean, alternative = "two.sided")

# extracting values
M1 <- stats$mean[1]
M2 <- stats$mean[2]
SD1 <- stats$sd[1]
SD2 <- stats$sd[2]
n1 <- stats$n[1]
n2 <- stats$n[2]

# extracting pooled SD
sd_pooled <- sqrt(((n1 - 1)*SD1^2 + (n2 - 1)*SD2^2) / (n1 + n2 - 2))

# Checking Cohen's d
print(cohen_d <- (M1 - M2) / sd_pooled)

# Checking t-test results for Hypothesis testing
if (t_test_result$p.value < 0.05) {
  cat("Null hypothesis rejected: there is a significant difference in mean net worth by gender.\n")
} else {
  cat("Null hypothesis not rejected: no significant difference in mean net worth by gender.\n")
}