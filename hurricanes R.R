install.packages('ggthemes')

library(tidyverse)
library(lubridate)
library(readr)
library(janitor)
library(stringr)
library(ggthemes)
library(plotly)

#the following data was scraped from Wikipedia using Google Sheets and a little bit of code
hurricanes <- read.csv("Worst Hurricanes.csv")

#Here I'm cleaning the data a bit
hurricanes.messy <- hurricanes%>%
  clean_namgites() %>%
  rename(peak_wind_speed = peak_1_min_sustained_winds) %>%
  select(cyclone, season, peak_wind_speed) %>%
  mutate(peak_wind_mph = ifelse(str_detect(peak_wind_speed, 'km/h'),
                          str_match(peak_wind_speed, '.*\\skm/h\\s(.*)')[,2],
                          peak_wind_speed)) 

hurricanes.messy$peak_wind_mph<-gsub("(","",as.character(hurricanes.messy$peak_wind_mph))

hurricanes.messy$peak_wind_mph = gsub("[(]", "", hurricanes.messy$peak_wind_mph)

#only want these columns
hurricanes.clean <- hurricanes.messy %>%
  select(cyclone, season, peak_wind_mph)
str(hurricanes.clean)

#now let's categorize them by decade because we want to ultimately see which decade had the
#greatest number of hurricanes that we are considering to be the worst or most intense
#based on peak wind speed
hurricanes.clean <- hurricanes.clean %>%
  mutate(decade = ifelse(season >= 2020, "2020+",
                  ifelse(season >= 2010, "2010-2019",
                  ifelse(season >= 2000, "2000-2009",
                  ifelse(season >= 1990, "1990-1999",
                  ifelse(season >= 1980, "1980-1989",
                  ifelse(season >= 1970, "1970-1979",
                  ifelse(season >= 1960, "1960-1969",
                  ifelse(season >= 1950, "1950-1959",
                  ifelse(season >= 1940, "1940-1949",
                  ifelse(season >= 1930, "1930-1939",
                  ifelse(season >= 1920, "1920-1929"))))))))))))

#now let's visualize that
viz <- hurricanes.clean %>%
  group_by(decade) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = decade, y = count)) +
  geom_bar(stat = "identity", fill = "#FF5733") +
  labs(title = "When did the top 24 most intense hurricanes happen?",
       subtitle = "Intensity measured by peak wind speed",
       caption = "Source: National Hurricane Center",
       x = "Decade",
       y = "Number of Hurricanes") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text())

viz

  
