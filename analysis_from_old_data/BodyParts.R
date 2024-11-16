library(dplyr)
library(tidyr)
library(ggplot2)
library(png)
library(showtext)
library(grid)

showtext_auto()
font_add_google(name = "Roboto", family = "Roboto")
font_add_google(name = "Lobster", family = "Lobster")
font_add_google(name = "Roboto Serif", family = "Roboto Serif")

premier_player_injuries <- read.csv("data/premier_player_injuries.csv")

t <- premier_player_injuries %>% 
  mutate(body.part = case_when(
    grepl("knee|meniscus|ligament|patelar", injury, ignore.case = TRUE) ~ "Knee",
    grepl("facial|head|eye", injury, ignore.case = TRUE) ~ "Head",
    grepl("hip", injury, ignore.case = TRUE) ~ "Hip",
    grepl("rib", injury, ignore.case = TRUE) ~ "Rib",
    grepl("foot|toe", injury, ignore.case = TRUE) ~ "Foot",
    grepl("hamstring|thighs", injury, ignore.case = TRUE) ~ "Thigh",
    grepl("ankle", injury, ignore.case = TRUE) ~ "Ankle",
    grepl("calf|achilles|shin", injury, ignore.case = TRUE) ~ "Calf",
    grepl("groin", injury, ignore.case = TRUE) ~ "Groin",
    grepl("back|lumbago", injury, ignore.case = TRUE) ~ "Back",
    grepl("hand", injury, ignore.case = TRUE) ~ "Hand",
    grepl("elbow", injury, ignore.case = TRUE) ~ "Elbow",
    grepl("arm", injury, ignore.case = TRUE) ~ "Arm",
    grepl("finger", injury, ignore.case = TRUE) ~ "Finger"
  )) %>% filter(!(is.na(body.part))) %>% 
  group_by(body.part) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

  body_image = readPNG("resources/body_cr.png")
  body_image <- rasterGrob(body_image, width = unit(1, "npc"), height = unit(1, "npc"))

t <- t %>% 
  mutate(x = c(0.55, 0.35, 0.1, 0.2, 0.49, 0.29, 0.4, 0.4, 0.61, 0.13, 0.46, 0.37, 0.7, 0.3),  
         y = c(0.34, 0.33, 0.1, 0.2, 0.47, 0.14, 0.65, 0.51, 0.94, 0.73, 0.7, 0.85, 0.7, 0.84),  
  ) %>% 
  mutate(label = paste(body.part, as.character(count), sep = "\n")) %>% 
  mutate(radius = if_else(count > 100, 
                          sqrt(count)*0.0025, 
                          0.02 + count/10000)) %>% 
  mutate(label_y = if_else(radius > 0.04, 
                           y + radius - 0.036, 
                           y + radius + 0.02)) %>% 
  mutate(num_y = if_else(radius > 0.04, y - 0.02, y)) %>% 
  mutate(num_size = if_else(radius >  0.04, 4.5, 3)) %>% 
  mutate(fraction = count/sum(count), fraction = round(fraction*100, 1)) %>% 
  mutate(fraction = paste(fraction, "%", sep = ""))

create_circle <- function(x_center, y_center, radius, linewidth, n_points = 1000) {
  theta <- seq(0, 2 * pi, length.out = n_points)
  data.frame(
    x_rim = x_center + radius * cos(theta),
    y_rim = y_center + radius * sin(theta),
    linewidth = linewidth
  )
}

circle_data <- t %>%
  rowwise() %>%
  do(cbind(., create_circle(.$x, .$y, .$radius, sqrt(.$radius*50))))

t$num_size[c(4, 5)] <- c(4, 4)
t$num_size[10:14] <- rep(2.5, 5)

p <- ggplot(circle_data, aes(x = x_rim, y = y_rim, 
                        group = body.part, 
                        fill = "#D32F2F")) +
  annotation_custom(body_image, xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
  geom_polygon(alpha = 0.6, color = "white",
               aes(linewidth = linewidth)) +
  scale_linewidth_identity() +
  coord_equal() + xlim(0, 1) +
  scale_size_identity() +
  geom_text(data = t, aes(x = x, y = label_y, label = body.part),
            color = "lightblue", size = 40, family = "Roboto") +
  geom_text(data = t, aes(x = x, y = num_y, label = fraction, 
                          size = num_size * 10),
            color = "#890764", fontface = "bold") +
  theme_void() +
  theme(legend.position = "none") + 
  labs(title = "Percentage of injuries by bodypart") + 
  theme(
    plot.title = element_text(
      size = 200,              
      face = "bold",        
      color = "#121420",
      vjust = -1.5,
      hjust = 0.4,
      family = "Roboto Serif"
    )
  )
ggsave("body_plot.png", plot = p, width = 18, height = 18, units = "in", dpi = 300)
