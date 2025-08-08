# Have to use json reader cause of the file type
install.packages(c("jsonlite", "dplyr", "lubridate", "rgeolocate", "tidyverse"))

# Had to install devtools to override the system since 
# rgeolocate was removed from CRAN 
install.packages("devtools")
devtools::install_github("ironholds/rgeolocate")

# Loaded the packages into the system
library(jsonlite)
library(dplyr)
library(lubridate)
library(rgeolocate)
library(tidyverse)

# Created a variable to store the json data & 
# checked to make sure it worked
ssh_logs <- fromJSON("brute_force_data.json")
print(ssh_logs)

# Reformed the data to create dates/times from strings & 
# checked to make sure it worked
ssh_logs <- ssh_logs |>
  mutate(
    timestamp = parse_date_time(timestamp, orders = "%a %b %d %T %Y"),
    
    hour_of_day = hour(timestamp),
    day_of_week = wday(timestamp, label = TRUE, abbr = FALSE)
  )
print(ssh_logs)

# I created a maxmind account to be able to get GeoLite2-Country file 
# to tranlate IP addresses to actual countries & stored it into a variable
results <- maxmind(ssh_logs$foreign_ip, "GeoLite2-Country.mmdb")
print(results) # Didn't know it created columns for you

# Now send the new data to the ssh_logs
ssh_logs$country <- results$country_name
print(ssh_logs)

# Creating a summary of attack by day and hour 
time_summary <- ssh_logs |> 
  group_by(day_of_week, hour_of_day) |>
  summarise(attack_count = n(), .groups = 'drop')

# 1 - When are attacks the most freq?
# For the fun part, creating the damn heatmap...
# ssh_freq_day_hour <-
ggplot(time_summary, aes(x = hour_of_day, y = day_of_week, fill = attack_count)) + 
  geom_tile(color = "white") + 
  scale_fill_gradient(name = "Attack Count") + 
  labs(
    title = "SSH Attack Frequency by Day and Hour",
    x = "Hour of the Day (24-Hour Format)",
    y = "Day of the Week"
  ) + 
  theme_minimal()

# 2 - Where do the attacks come from and are the patterns differ?
# Making a bar chart to see the amount of attacks coming from each country 
top_countries <- ssh_logs |>
  group_by(country) |>
  summarise(total_attacks = n()) |>
  slice_max(order_by = total_attacks, n = 10)
# Always double check your work
print(top_countries)
# Made a bar graph to show the differences in top 10 countries
ggplot(
  top_countries, 
  aes(x = country, y = total_attacks)) +
  geom_col(fill = "navy") +
  labs(
    title = "Top 10 Source Countries for SSH Attacks",
    x = "Country",
    y = "Total Number of Attacks"
  ) +
  theme_bw()

# Filtered out countries to count the amount of attack happening at each hour of 
# the day in those countries then represented them in a bar graph
filtered_countries <- 
  ssh_logs |>
  filter(country %in% top_countries$country) |>
  group_by(country, hour_of_day) |>
  summarise(attack_count = n(), .groups = 'drop') 
ggplot(
  filtered_countries,
  aes(x = hour_of_day, y = attack_count, fill = country)) + 
  geom_line() + # Changed it to a line graph since the bar graphs made it hard  
  # to understand 
  facet_wrap(~ country, scales = "free") + 
  labs( title = "Hourly Attack Patterns by Country", 
        x = "Hour of the Day", y = "Number of Attacks" ) + 
  theme_light() + 
  theme(legend.position = "none") 

nrow(ssh_logs)

