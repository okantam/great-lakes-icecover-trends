library(zoo)
library(imputeTS)
library(dplyr)
library(pracma)
library(tidyverse)
library(lmtest)
library(kableExtra)
library(MESS)
library(Kendall)
library(trend)

# Load the dataset
data <- read.csv("/Users/presidentoliver/Desktop/Practicum in Data Analysis/Project 5 - Great Lakes Ice Cover /greatlakesicedata.csv", row.names = 1, check.names = FALSE)

# Define a function to handle filling and interpolation within each year
fill_na_ice_cover <- function(year_data) {
  # Identify indices of first and last non-NA values
  first_non_na <- which(!is.na(year_data))[1]
  last_non_na <- tail(which(!is.na(year_data)), 1)
  
  # Fill leading NAs: Start with 10% of the first observed value, then interpolate up
  if (!is.na(first_non_na) && first_non_na > 1) {
    # Create a starting value at 10% of the first observed value
    start_value <- year_data[first_non_na] * 0.05
    # Create a vector with this initial value and then use linear interpolation up to the first observed value
    leading_values <- c(start_value, rep(NA, first_non_na - 2))
    leading_values <- na.approx(c(leading_values, year_data[first_non_na]), na.rm = FALSE)
    year_data[1:first_non_na] <- leading_values
  }
  
  # Fill trailing NAs: Start with the last observed value, then interpolate down to 10% of it
  if (!is.na(last_non_na) && last_non_na < length(year_data)) {
    # Create an ending value at 10% of the last observed value
    end_value <- year_data[last_non_na] * 0.05
    # Create a vector with this last observed value and then use linear interpolation down to the end value
    trailing_values <- c(year_data[last_non_na], rep(NA, length(year_data) - last_non_na - 1))
    trailing_values <- na.approx(c(trailing_values, end_value), na.rm = FALSE)
    year_data[(last_non_na):length(year_data)] <- trailing_values
  }
  
  # Perform linear interpolation on remaining NA values (between November and early June)
  year_data <- na.approx(year_data, na.rm = FALSE)
  
  return(year_data)
}

# Apply the function across each column (each year) except for the summer months
ice_data_filled <- data %>%
  mutate(across(starts_with("19") | starts_with("20"), ~ fill_na_ice_cover(.)))


view(ice_data_filled)
# Step 3: Reshape to long format for easier manipulation and regression-based imputation
ice_data_long <- ice_data_filled %>%
  mutate(day_month = rownames(ice_data_filled)) %>%
  pivot_longer(cols = -day_month, names_to = "year", values_to = "ice_cover") %>%
  mutate(year = as.numeric(sub("X", "", year)))  # Remove "X" prefix and convert year to numeric


# Remove rows where day_month is "29-Feb"
#data_long <- data_long %>% filter(day_month != "29-Feb")

# Now you can create the date variable without issues
ice_data_long <- ice_data_long %>%
  mutate(date = dmy(paste(day_month, year, sep = "-")))

ice_summary_stats <- ice_data_long %>%
  # Convert 'day_month' to a Date and extract the month
  mutate(month = month(date)) %>%
  # Remove rows with NA ice_cover values
  filter(!is.na(ice_cover)) %>%
  # Group by year and month for monthly statistics
  group_by(year, month) %>%
  # Calculate summary statistics, removing remaining NAs within groups
  summarize(
    mean_ice_cover = mean(ice_cover, na.rm = TRUE),
    median_ice_cover = median(ice_cover, na.rm = TRUE),
    max_ice_cover = max(ice_cover, na.rm = TRUE),
    min_ice_cover = min(ice_cover, na.rm = TRUE),
    sd_ice_cover = sd(ice_cover, na.rm = TRUE)
  ) %>%
  # Drop any groups with all NA summary stats, if any
  filter(!is.na(mean_ice_cover) & !is.na(median_ice_cover) & 
           !is.na(max_ice_cover) & !is.na(min_ice_cover) & !is.na(sd_ice_cover))

## display summary statistics and make the table scrollable 

kable(ice_summary_stats, 
      caption = "Table 1: Summary statistics of percentage ice cover") %>%
  kable_styling(full_width = FALSE) %>%
  scroll_box(width = "100%", height = "400px")

# Step 2: Create a custom day_of_year sequence
# Define the sequence from Nov 9 to June 5, excluding the unrecorded days
recorded_days <- unique(ice_data_long$day_month)
day_of_year_mapping <- data.frame(
  day_month = recorded_days,
  day_of_year = seq_along(recorded_days)  # Assigns 1, 2, 3... sequentially starting from Nov 9
)

# Step 3: Merge the custom day_of_year into data_long based on day_month
ice_data_long <- ice_data_long %>%
  left_join(day_of_year_mapping, by = "day_month")

# Step 4: Calculate AUC for each year using the custom day_of_year
auc_by_year <- ice_data_long %>%
  filter(!is.na(ice_cover)) %>%
  arrange(year, day_of_year) %>%
  group_by(year) %>%
  summarize(AUC = trapz(x = day_of_year, y = ice_cover))


#  Plot year against AUC using ggplot2
AUC_time_series <- ggplot(auc_by_year, aes(x = year, y = AUC)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(
    title = "Time Series of Annual AUC for Ice Cover",
    x = "Year",
    y = "Area Under Curve (AUC)",
    caption = "Figure 1"
  ) +
  theme_classic() +
  theme(
    plot.caption = element_text(hjust = 0.5)  # Centers the caption
  )

AUC_time_series

# Step 6: Create the heatmap using ggplot2
new_heatmap <- ggplot(ice_data_long, aes(x = day_of_year, y = year, fill = ice_cover)) +
  geom_tile() +
  scale_fill_viridis_c(option = "rocket", na.value = "white", direction = -1) +
  labs(
    title = "Heatmap of Ice Cover Across Days and Years",
    x = "Custom Day of Year (Starting Nov 9)",
    y = "Year",
    fill = "Ice Cover (%)",
    caption = "Figure 2"
  ) +
  theme_classic()+
  theme(
    plot.caption = element_text(hjust = 0.5)
  )

new_heatmap

auc_by_year <- ice_data_long %>%
  filter(!is.na(ice_cover)) %>%
  arrange(year, day_of_year) %>%
  group_by(year) %>%
  summarize(AUC = auc(x = day_of_year, y = ice_cover))


## Visualize the trend

ggplot(auc_by_year, aes(x = year, y = AUC)) +
  geom_line() +
  geom_smooth(method = "loess") +
  labs(title = "Trend in AUC over the Years",
       y = "Year",
       caption = "Figure 3") +
  theme_classic() +
  theme_classic()+
  theme(
    plot.caption = element_text(hjust = 0.5)
  )

# Fitting a polynomial regression model
poly_model <- lm(AUC ~ poly(year, 2), data = auc_by_year)

# Summary of the model
#summary(poly_model)

# Plot the fitted curve
plot_of_fitted <- ggplot(auc_by_year, aes(x = year, y = AUC)) +
  geom_point(color = "blue") +  # Original data points
  geom_line(aes(y = predict(poly_model)), color = "red") +  # Fitted polynomial line
  labs(
    title = "Fitted Polynomial Model for AUC",
    x = "Year",
    y = "Area Under Curve (AUC)"
  ) +
  theme_minimal()

# Extract coefficients and summary statistics
model_summary <- summary(poly_model)
coef_table <- as.data.frame(model_summary$coefficients)
colnames(coef_table) <- c("Estimate", "Std. Error", "t value", "P-value")

# Display the table with kable
summary_table <- kable(coef_table, caption = "Table 2: Summary of Polynomial Regression Model for AUC") %>%
  kable_styling(full_width = FALSE, position = "center")
summary_table

# Perform Mann-Kendall Trend Test
mk_test <- mk.test(auc_by_year$AUC)

# Create a data frame for Mann-Kendall test results
mk_results <- data.frame(
  Statistic = c("S", "Var(S)", "Tau", "Z", "P-value"),
  Value = c(-402, 16059.33, -0.3031674, -3.1643, 0.001554)
)

# Display the table with kable
kable(mk_results, caption = "Table 3: Results of the Mann-Kendall Trend Test") %>%
  kable_styling(full_width = FALSE, position = "center")


## check assumptions for the polynomial model


# Residual vs Fitted plot for linearity 

plot(poly_model$fitted.values, resid(poly_model), 
     main = "Residuals vs Fitted Values", 
     xlab = "Fitted Values", 
     ylab = "Residuals", 
     pch = 20, col = "blue")
abline(h = 0, col = "red", lty = 2)

# constant variance 

# Perform the Breusch-Pagan Test
bptest(poly_model)

# normality

# Q-Q plot
qqnorm(resid(poly_model), main = "Q-Q Plot of Residuals")
qqline(resid(poly_model), col = "red", lty = 2)

# Shapiro-Wilk test
shapiro.test(resid(poly_model))

# independence 

# Perform Durbin-Watson Test
dwtest(poly_model)

