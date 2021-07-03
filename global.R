library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(maps)
library(mapproj)

# Load data
EV <- read.csv("EV.csv",header = T,na.strings=c("","NA"))
EV$Submitted.Date<- substring(EV$Submitted.Date, 7)
EV <- EV[,-1]
colnames(EV)[1] <- "Year"
colnames(EV)[colSums(is.na(EV)) > 0]
EV<-EV %>%
  group_by(ZIP) %>% 
  fill(County, .direction = "downup")

EV<- EV %>% drop_na()

colnames(EV)[colSums(is.na(EV)) > 0]

EV$County <- tolower(EV$County)

colnames(EV)[8] <- "CO2"
colnames(EV)[9] <- "Petrol"


states <- map_data("state")
ny_df <- subset(states, region == "new york")
counties <- map_data("county")
ny_county <- subset(counties, region == "new york")
colnames(ny_county)[6]<-"County"
NY_County_map <- ggplot(data = ny_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")+
  geom_polygon(data = ny_county, fill = NA, color = "black") +
  geom_polygon(color = "black", fill = NA) +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.title = element_blank()
  )