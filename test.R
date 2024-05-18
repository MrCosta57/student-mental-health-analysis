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
