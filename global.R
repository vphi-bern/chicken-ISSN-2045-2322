library(shiny)
library(shinydashboard)
library(dygraphs)
library(shinyWidgets)
library(highcharter)
library(dplyr)
library(xts)
# library(ggplot2)
library(purrr)
library(htmltools)
library(stringr)
library(shinyjs)
library(formattable)
library(sparkline)
library(lubridate)
library(visNetwork)
# library(rChartsCalmap)
library(viridis)

# library(readr)

movementsData <- read.csv2("www/data/movementsData.csv")

timeSeriesData <- read.csv2("www/data/timeSeriesData.csv")
timeSeriesData <- timeSeriesData %>%
  mutate(day = format(as.Date(time), "%Y-%m-%d")) 
  # %>% mutate(hour = format(as.POSIXct(time), "%H:%M:%S"))

chicken_number <- c("1", "2", "3", "7", "9", "17", "18", "35", "36", "37", "38", "39", "58")

chicken_names <- c(
  "Chicken 1" = "chicken_1",
  "Chicken 2" = "chicken_2",
  "Chicken 3" = "chicken_3",
  "Chicken 7" = "chicken_7",
  "Chicken 9" = "chicken_9",
  "Chicken 17" = "chicken_17",
  "Chicken 18" = "chicken_18",
  "Chicken 35" = "chicken_35",
  "Chicken 36" = "chicken_36",
  "Chicken 37" = "chicken_37",
  "Chicken 38" = "chicken_38",
  "Chicken 39" = "chicken_39",
  "Chicken 58" = "chicken_58"
)

chicken_names_c <- c(
  chicken_1 = "Chicken 1",
  chicken_2 = "Chicken 2",
  chicken_3 = "Chicken 3",
  chicken_7 = "Chicken 7",
  chicken_9 = "Chicken 9",
  chicken_17 = "Chicken 17",
  chicken_18 = "Chicken 18",
  chicken_35 = "Chicken 35",
  chicken_36 = "Chicken 36",
  chicken_37 = "Chicken 37",
  chicken_38 = "Chicken 38",
  chicken_39 = "Chicken 39",
  chicken_58 = "Chicken 58"
)

# uniqueDays <- timeSeriesData %>% distinct(format(as.Date(time), "%d-%m-%Y"))
uniqueDays <- timeSeriesData %>% distinct(day)
colnames(uniqueDays) <- c("time")

# day_hour <- seq(ISOdate(2016, 8, 20, 0, 0, 0), ISOdate(2016, 8, 20, 23, 59, 59), "sec")
# day_hour <- as.data.frame(day_hour)
# day_hour$hour <- format(as.POSIXct(day_hour$day_hour), "%H:%M:%S")

levelColorPalette <- c("1" = "#95CEFF", "2" = "#5C5C61", "3" = "#FFBC75", "4" = "#999EFF", "5" = "#44A9A8")
levelColorPaletteSparkline <- c("1" = "#A4BAE8", "2" = "#ED9C88", "3" = "#FFCC7F", "4" = "#87CA8B", "5" = "#B2D47F")

##########################################
# Import functions
##########################################
source("functions.R")
source("www/modules/timeSeriesModule.R")
source("www/modules/totalTimeModule.R")
source("www/modules/movementsModule.R")
source("www/modules/comparisonModule.R")
# source("www/modules/statisticsModule.R")