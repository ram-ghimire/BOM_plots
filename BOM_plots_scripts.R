install.packages("cowplot")
library(tidyverse)
library(cowplot)

## Code carried over from previous challenge -----

BOM_data <- read_csv("data/BOM_data.csv")
BOM_stations <- read_csv("data/BOM_stations.csv")

# Tidy the BOM_data file and convert measurements into numeric values
BOM_with_temps <- BOM_data %>% 
  separate(Temp_min_max, into = c("t_min", "t_max"), sep = "/") %>% 
  mutate(
    t_min          = as.numeric(t_min),
    t_max          = as.numeric(t_max),
    Rainfall       = as.numeric(Rainfall),
    Solar_exposure = as.numeric(Solar_exposure)
  )

# Tidy the BOM_stations metadata by reshaping the dataframe 
stations_tidy <- BOM_stations %>% 
  gather(key = "Station_number", value = "values", -info) %>% 
  spread(key = info, value = values) %>% 
  mutate(
    Station_number = as.numeric(Station_number),
    lat = as.numeric(lat),
    lon = as.numeric(lon)
  )

# Plotting challenge code 
#
# For this challenge I will be using the default scales and themes to make the plot construction
# code clearer.
#
# I encourage you to modify your own code in order to make your plots more presentable.

## Question 1: -----
# For the Perth station (ID 9225), produce three scatter plots showing the relationship between the 
# maximum temperature and each other measurement recorded (minimum temperature, rainfall and 
# solar exposure). 

perth_data <- BOM_with_temps %>% 
  filter(Station_number == 9225)

q1_a <- perth_data %>% 
  ggplot(mapping = aes(x = t_max, y = t_min)) +
  geom_point(alpha = 0.2)

q1_b <- perth_data %>% 
  ggplot(mapping = aes(x = t_max, y = Rainfall)) +
  geom_point(alpha = 0.2)

q1_c <- perth_data %>% 
  ggplot(mapping = aes(x = t_max, y = Solar_exposure)) +
  geom_point(alpha = 0.2)

ggsave("figures/q1_tmax_v_tmin.png", q1_a, width = 5, height = 5)
ggsave("figures/q1_tmax_v_rainfall.png", q1_b, width = 5, height = 5)
ggsave("figures/q1_tmax_v_solar.png", q1_c, width = 5, height = 5)

## Question 2: -----
# Display these four measurements for the Perth station in a single scatter plot by using 
# additional aesthetic mappings.

q2 <- perth_data %>% 
  ggplot(mapping = aes(x = t_max, y = t_min, size = Rainfall, colour = Solar_exposure)) +
  geom_point(alpha = 0.2)

ggsave("figures/q2_all_measurements.png", q2, width = 5, height = 5)

## Question 3: -----
# Take the four plots you have produced in Q1 and Q2 and save them as a multi-panel figure.

# put the legend on the bottom for the Q2 plot
q2_on_bottom <- q2 + theme(legend.position = "bottom")

four_plots <- plot_grid(q1_a, q1_b, q1_c, q2_on_bottom, labels = "AUTO")

ggsave("figures/q3_four_plots_in_one.png", width = 12, height = 12)

## Question 4: -----
# Using the entire BOM dataset, calculate the average monthly rainfall for each station. 
# Produce a lineplot to visualise this data and the state each station is in.

q4 <- BOM_with_temps %>% 
  group_by(Station_number, Month) %>%
  filter(!is.na(Rainfall)) %>% 
  summarise(avg_rainfall = mean(Rainfall)) %>% 
  left_join(stations_tidy) %>% 
  ggplot(mapping = aes(x = Month, y = avg_rainfall, group = Station_number)) +
  geom_line() +
  facet_wrap(~state)

ggsave("figures/q4_avg_rainfall_by_state.png", q4, width = 8, height = 8)

