library(dplyr)
library(tidyr)
library(ggplot2)
library(png)
library(grid)

premier_player_injuries <- read.csv("data/premier_player_injuries.csv")
france_player_injuries <- read.csv("data/france_player_injuries")
spain_player_injuries <- read.csv("data/spain_player_injuries")
italy_player_injuries <- read.csv("data/italy_player_injuries")
germany_player_injuries <- read.csv("data/germany_player_injuries")
View(premier_player_injuries)

. <- premier_player_injuries %>% 
  select(injury) %>% distinct() %>% fliter()
View(.)

t <- premier_player_injuries %>% 
  mutate(body.part = case_when(
    grepl("knee|meniscus|ligament|patelar", injury, ignore.case = TRUE) ~ "knee",
    grepl("facial|head|eye", injury, ignore.case = TRUE) ~ "face",
    grepl("hip", injury, ignore.case = TRUE) ~ "hip",
    grepl("rib", injury, ignore.case = TRUE) ~ "rib",
    grepl("foot|toe", injury, ignore.case = TRUE) ~ "foot",
    grepl("hamstring|thighs", injury, ignore.case = TRUE) ~ "thigh",
    grepl("ankle", injury, ignore.case = TRUE) ~ "ankle",
    grepl("calf|achilles|shin", injury, ignore.case = TRUE) ~ "calf",
    grepl("groin", injury, ignore.case = TRUE) ~ "groin",
    grepl("back", injury, ignore.case = TRUE) ~ "back",
    grepl("hand", injury, ignore.case = TRUE) ~ "hand",
    grepl("elbow", injury, ignore.case = TRUE) ~ "elbow",
    grepl("arm", injury, ignore.case = TRUE) ~ "arm",
    grepl("finger", injury, ignore.case = TRUE) ~ "finger",
    grepl("lumbago", injury, ignore.case = TRUE) ~ "low back"
  )) %>% filter(!(is.na(body.part))) %>% 
  group_by(body.part) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

View(t)
body_image = readPNG("resources/body.png")

injury_locations <- data.frame(
  x = c(0.46, 0.45, 0.47, 0.47, 0.5, 0.45, 0.5, 0.45, 0.5, 0.34, 0.45, 0.355, 0.33, 0.37, 0.5, 0, 1),  
  y = c(0.3, 0.4, 0.06, 0.2, 0.5, 0.01, 0.65, 0.48, 0.95, 0.51, 0.68, 0.58, 0.45, 0.63, 0.58, 0, 1),  
  injury_count = c(t$count, 0, 0)
)


ggplot() +
  annotation_custom(rasterGrob(body_image, 
                               width = unit(1, "npc"), 
                               height = unit(1, "npc")),
                    -Inf, Inf, -Inf, Inf) +
  theme_void() + geom_point(data = injury_locations, 
                            aes(x = x, y = y, size = injury_count,
                                color = injury_count, alpha = 0.9)) +
  scale_color_gradient(low = "white", high = "red") + 
  scale_size(range = c(5, 20)) + 
  labs(title = "Injury Heatmap on Human Body")










