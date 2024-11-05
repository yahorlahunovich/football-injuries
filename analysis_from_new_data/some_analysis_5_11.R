library(dplyr)
library(ggplot2)
library(ggtext)
library(lubridate)

df <- read.csv("~/Documents/uni/sem_3/twd/football-injuries/analysis_from_new_data/Final-player.csv")
injuries <- read.csv("~/Documents/uni/sem_3/twd/football-injuries/analysis_from_new_data/Final-player-injuies.csv")

# let's see which injuries are the worst(based on recovery days)

inj_rec <- injuries %>% 
  filter(!is.na(days)) %>%
  mutate(days = as.numeric(days)) %>% 
  group_by(type) %>% 
  summarise(
    count = n(),
    mean_day = mean(days, na.rm = TRUE)
  ) %>% 
  arrange(desc(count)) %>% 
  head(25)

ggplot(inj_rec, aes(x = reorder(type, mean_day), y = mean_day, fill = count)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_gradient(low = "#c2f0c2", high = "#006400") + # Light green to dark green
  geom_text(aes(label = paste0(round(mean_day, 1))), 
            hjust = -0.2, color = "black", size = 4, fontface = "bold") + 
  labs(
    title = "Average Recovery Time by Injury Type",
    subtitle = "Mean days required for players to recover from top 25 types of injury",
    x = NULL,
    y = "Average Days to Recover",
    fill = "Count"
  ) +
  coord_flip() + 
  expand_limits(y = max(inj_rec$mean_day) * 1.1) +
  theme(
    plot.title = element_text(size = 24, face = "bold", color = "#004d00"),
    plot.subtitle = element_text(size = 16, color = "#005700"),
    axis.text.x = element_text(size = 12, color = "#004d00"), 
    axis.text.y = element_text(size = 12, face = "bold", color = "#004d00"), 
    legend.position = "none"
  ) +
  theme_minimal()

injuries <- injuries %>%
  mutate(
    height = as.numeric(paste0(substr(height, 1, 1), "", substr(height, 3, 4)))/100, 
    weight = as.numeric(weight),                    
    bmi = weight / (height^2)                                      
  ) %>% 
  mutate(
    bmi = round(bmi)
  )

grouped_injuries <- injuries %>% 
  group_by(bmi) %>% 
  summarise(
    count = n()
  )



injuries_summary <- injuries %>%
  mutate(
    birth_date = ymd(birth),
    season_end_year = paste0("20", substr(season, 4, 5)),
    injury_date = ymd(paste0(season_end_year, "-06-30")),
    age = as.integer(interval(birth_date, injury_date) / years(1))
  ) %>%
  group_by(id, name, age) %>%
  summarise(total_injuries = n()) %>%
  ungroup()

ggplot(injuries_summary, aes(x = age, y = total_injuries)) +
  geom_point(alpha = 0.4, color = "#1f78b4") +
  geom_smooth(method = "loess", color = "#e31a1c", size = 1) +
  labs(
    title = "Total Injuries per Player by Age",
    subtitle = "Relationship Between Player Age and Number of Injuries",
    x = "Age (Years)",
    y = "Number of Injuries"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(size = 20, face = "bold", color = "#2c3e50"),
    plot.subtitle = element_text(size = 14, color = "#34495e"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "#ecf0f1"),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(breaks = seq(18, 40, by = 2)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  coord_cartesian(clip = "off")




injuries_summary <- injuries %>%
  mutate(
    birth_date = ymd(birth),
    season_end_year = ifelse(as.numeric(substr(season, 4, 5)) > 50,
                             paste0("19", substr(season, 4, 5)),
                             paste0("20", substr(season, 4, 5))),
    injury_date = ymd(paste0(season_end_year, "-06-30")),
    age = as.integer(interval(birth_date, injury_date) / years(1))
  ) %>%
  group_by(age) %>%
  summarise(total_injuries = n(), .groups = 'drop')

ggplot(injuries_summary, aes(x = age, y = total_injuries)) +
  geom_bar(stat = "identity", fill = "#006400") +
  geom_text(aes(label = total_injuries), vjust = -0.5, size = 3) +
  labs(
    title = "Total Injuries by Player Age",
    subtitle = "Aggregate Number of Injuries Across All Players",
    x = "Age (Years)",
    y = "Total Number of Injuries"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(size = 20, face = "bold", color = "#2c3e50"),
    plot.subtitle = element_text(size = 14, color = "#34495e"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "#ecf0f1"),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(breaks = seq(18, 40, by = 2)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  coord_cartesian(clip = "off")
