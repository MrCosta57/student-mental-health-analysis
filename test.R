# Load necessary library
library(ggplot2)
library(ggmosaic) #ggmosaic is a ggplot2 extension for mosaic plots, and it may be necessary to install the package via install.packages("ggmosaic")

# Sample data (replace with your own)
data <- data.frame(
  x = sample(c("Category A", "Category B"), 100, replace = TRUE),
  y = sample(c("Group 1", "Group 2", "Group 3"), 100, replace = TRUE)
)

# Create the mosaic plot
ggplot(data) +
  geom_mosaic(aes(x = product(x), fill = y)) +
  labs(title = "Mosaic Plot of X and Y", x = "X Variable", fill = "Y Variable") 



library(RColorBrewer)

# Create mosaic plot with colorblind-friendly brewer palette
ggplot(data) +
  geom_mosaic(aes(x = product(x), fill = y)) +
  scale_fill_brewer(palette = "Dark2") + # Choose from available palettes
  labs(title = "Mosaic Plot with ColorBrewer Palette", x = "X Variable", fill = "Y Variable")


library(ggplot2)
library(dplyr)

# Sample data (replace with your actual data)
df <- data.frame(
  predictor = sample(c("Group A", "Group B", "Group C"), 100, replace = TRUE),
  response = sample(c("Yes", "No"), 100, replace = TRUE)
)

# Calculate response totals
response_totals <- df %>%
  count(response) %>%
  rename(response_total = n)

# Aggregate data
df_agg <- df %>%
  count(predictor, response) %>%
  left_join(response_totals, by = "response") %>%  # Join response totals
  mutate(proportion = n / response_total)

# Create the plot
ggplot(df_agg, aes(x = predictor, y = proportion, fill = response)) +
  geom_col(position = "dodge") +  # Dodged bars for comparison between responses
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Predictor Variable", y = "Proportion within Response", fill = "Response") +
  theme_minimal() 



# Function to extract predictors from a formula that are in a given vector
extract_predictors <- function(formula, input_vector) {
  # Extract the terms object from the formula
  terms_obj <- terms(formula)
  
  # Extract the variable names from the terms object
  vars <- attr(terms_obj, "variables")
  
  # Convert to character and exclude the response variable
  predictors <- as.character(vars[-1])
  
  # Return the intersection of predictors and input_vector
  common_predictors <- intersect(predictors, input_vector)
  
  return(common_predictors)
}

# Example usage
example_formula <- y ~ x1 + x2 + x3 + x4
input_vector <- c("x1", "x3", "x5")
extract_predictors(example_formula, input_vector)

