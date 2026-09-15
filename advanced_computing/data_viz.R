library(tidyverse)
library(ggplot2)

# load the data
setwd('/Users/srgib/GitHub/qBio')
world_game <- read.csv("world_game_v2.txt",stringsAsFactors = FALSE,header = TRUE)

country_codes <- read.csv('country_codes.csv',stringsAsFactors = FALSE, header=FALSE)
colnames(country_codes) <- c('code','country')

# do some pre-processing and cleaning
world_game$continent <- tolower(world_game$continent)
world_game$continent <- trimws(world_game$continent)
# and conver to a factor
world_game$continent <- factor(world_game$continent)



# ggplot approach to plotting
# 1. a data frame (df)
# 2. aesthetic with a mapping between plot elements (axes, color, size, shape, etc.)
#     to values in the df
# 3. geom which adds an element to the plot
p <- ggplot(world_game, aes(x=area, y = population)) +
  geom_point() + 
  theme_minimal() + 
  scale_x_log10() + 
  scale_y_log10()

ggsave('world_game.png', p, height=4, width=6)
ggsave('world_game.pdf', p, height=4, width=6)
ggsave('world_game.svg', p, height=4, width=6)


### challenges

# one idea, plot the data on top of the world map
library(maps)
library(countries)

world_map <- map_data('world')

ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightgray", color = "white") +
  coord_quickmap()


land_area_plot <- ggplot(world_game, aes(x=area, y=highest.point, size=neighboring.countries, color=continent)) +
  geom_point() + 
  theme_minimal() + 
  scale_x_log10() + 
  facet_wrap(~continent) + 
  scale_color_brewer(palette = "Set2") + 
  labs(
    title = 'Country Land Area by Height, Continent, and Highest Point',
    x = 'Land Area (square miles)',
    y = 'Highest Point (ft)'
  )

ggsave('land_area_by_height_continent_highest_point.pdf', land_area_plot, height=7, width=12)

# group the data by continent and then compute correlation coefficients
# between population, highest.point, neighboring.countries with area
world_df <- world_game
world_df <- world_df %>%
  group_by(continent) %>%
    mutate(
      pop_cor = cor(area, population), 
      highest_cor = cor(area, highest.point),
      neighboring_cor = cor(area, neighboring.countries)
    )

continent_corr <- world_df %>%
  select(c('continent', 'pop_cor', 'highest_cor', 'neighboring_cor')) %>%
  distinct(continent, .keep_all = TRUE)

colnames(continent_corr) <- c('Continent',
                              'Population',
                              'Highest Point',
                              'Neighboring Countries')
continent_corr <- continent_corr %>%
  column_to_rownames(var = "Continent") %>% 
  as.data.frame()
  

heatmap(as.matrix(continent_corr))

pivot_wider(names_from = Metric, values_from = Value)
  
install.packages("devtools")
library(devtools)
install_github('jimjam-slam/ggflags')
library(ggflags)


library(countries)
test_df <- auto_merge(world_game, country_codes)
test_df <- left_join(test_df, country_codes)



### Part 2
world_game$rank_by_area <- rank(world_game$area)
library(ggplot2) 
library(ggrepel)
world_plot <- ggplot(data = world_game, mapping = aes(x = rank_by_area, y = log10(population), label = country)) +
  geom_point(aes(color = factor(continent), size = area), show.legend = FALSE) +
  geom_text_repel(size = 2, segment.color = NA) +
  scale_color_manual(values = c(
    "africa" = "seagreen",
    "asia" = "orange",
    "europe" = "royalblue4",
    "north america" = "red",
    "oceania" = "lightblue",
    "south america" = "deeppink4"
  )) +
  facet_wrap(~ continent) +
  labs(
    title = "populations vs. land sizes",
    y = "log10 population size",
    x = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_line(),
    panel.grid = element_blank(),
    panel.background = element_blank())

world_plot
