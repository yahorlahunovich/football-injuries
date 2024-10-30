# Load necessary libraries
library(ggplot2)
library(grid)
library(png)

# Read in the PNG file as a raster (replace with your own path)
img <- png::readPNG("resources/body_cr.png")
img_grob <- rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc"))

# Function to generate points around each circle
create_circle <- function(center = c(0, 0), radius = 1, n_points = 100, id, linewidth) {
  theta <- seq(0, 2 * pi, length.out = n_points)
  x <- center[1] + radius * cos(theta)
  y <- center[2] + radius * sin(theta)
  data.frame(x = x, y = y, id = id, linewidth = linewidth)  # Add id and linewidth columns
}

# Generate data for three circles with different linewidths
circle1 <- create_circle(center = c(1, 1), radius = 0.5, id = "Circle 1", linewidth = 5)
circle2 <- create_circle(center = c(3, 3), radius = 1, id = "Circle 2", linewidth = 0.1)
circle3 <- create_circle(center = c(5, 1), radius = 1.5, id = "Circle 3", linewidth = 0.15)

# Combine all circles into one data frame
circles_data <- rbind(circle1, circle2, circle3)
View(circles_data)

# Create a data frame for circle labels (centers of each circle)
labels_data <- data.frame(
  x = c(1, 3, 5),   # x-coordinates for circle centers
  y = c(1, 3, 1),   # y-coordinates for circle centers
  label = c("Circle 1", "Circle 2", "Circle 3")  # Labels for each circle
)

# Plot circles with labels and background image
ggplot(circles_data, aes(x = x, y = y, group = id, fill = id)) +
  # Add background image
  annotation_custom(img_grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  # Add circles with varying line widths
  geom_polygon(alpha = 0.4, color = "black", aes(linewidth = linewidth)) +
  theme_minimal() +
  coord_equal() +
  labs(title = "Circles with Varying Linewidths and Background Image")
