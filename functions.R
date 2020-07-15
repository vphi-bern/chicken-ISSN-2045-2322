##########################################
# Unique days per chicken
##########################################
unique_days_per_chicken <- function(chicken_name = "chicken_1") {
  chicken_data <- timeSeriesData[timeSeriesData[chicken_name] != 0, c(chicken_name, "day")] %>%
    group_by(day)
  
  colnames(chicken_data) <- c("chicken", "time")
  
  chicken_data <- aggregate(chicken ~ time, chicken_data, sum)
  chicken_data <- chicken_data %>%
    filter(chicken > 20)
  
  chicken_data$time
}

##########################################
# All chicken per day
##########################################
unique_chicken_per_day <- function(day_string = "2016-08-23") {
  names(which(colSums(timeSeriesData[timeSeriesData$day == day_string, chicken_names] != 0) > 20))
}

time_expend_chicken_in_level_per_day<- function(chicken_name, level, day) {
  chicken_data <- timeSeriesData[timeSeriesData[chicken_name] != 0 & timeSeriesData[chicken_name] == level & timeSeriesData$day == day, c(chicken_name, "time", "day")]
  chicken_data
}

##########################################
# Chicken number by name
##########################################
chicken_number_by_name <- function(chicken_name = "chicken_1") {
  str_replace(chicken_name, "chicken_", "")
}

##########################################
# Chicken number by name
##########################################
unique_levels_chicken_date <- function(chicken_name = "chicken_1", date_string = "2016-08-23") {
  chicken_data <- timeSeriesData[timeSeriesData[chicken_name] != 0 & timeSeriesData$day == date_string, c(chicken_name, "day")]
  colnames(chicken_data) <- c("level", "day")
  sort(unique(chicken_data$level)) 
}

##########################################
# Chicken coincidences levels
##########################################
coincidences_levels_chicken <- function(chickens_name = c("chicken1_1", "chicken2_2"), date_range = c(), hour_range_include = seq(0, 23, 1)) {
  date_range <<- date_range
  chickens_name <- c(c("time", "hour_time"), chickens_name)
  chicken_data <- timeSeriesData[timeSeriesData$day %in% date_range, chickens_name]
  result <- data.frame(from = as.Date(character()), to = as.Date(character()), level = integer(), stringsAsFactors = FALSE)
  
  if(nrow(chicken_data) > 0) {
    chicken_data$min <- do.call(pmin, chicken_data[3:length(chickens_name)])
    chicken_data$max <- do.call(pmax, chicken_data[3:length(chickens_name)])
    
    chicken_data <- chicken_data %>%
      mutate(diff = max - min) %>%
      mutate(coincidence = 0)
    
    diffVector <- as.vector(chicken_data$diff)
    coincidenceVector <- as.vector(chicken_data$coincidence)
    
    periodFlag <- FALSE
    vectorLeng <- length(diffVector)
    
    coincidenceNumber <- 1
    
    for(i in 1:vectorLeng) {
      if(diffVector[i] == 0) {
        periodFlag <- TRUE
        coincidenceVector[i] <- coincidenceNumber
      } else {
        if(periodFlag) {
          periodFlag <- FALSE
          coincidenceNumber <- coincidenceNumber + 1
        }
      }
    }
    
    chicken_data$coincidence <- coincidenceVector
    
    chicken_data <- chicken_data %>%
      filter(coincidence != 0)  %>%
      mutate(coincidence = if_else(hour_time %in% hour_range_include, coincidence, 0))
    
    if(nrow(chicken_data) > 1) {
      coincidenceVector <- as.vector(chicken_data$coincidence)
      coincidenceSecondVector <- coincidenceVector
      vectorLeng <- length(coincidenceVector)
      coincidenceNumber <- 0
      periodFlag <- FALSE
      
      if(coincidenceVector[1] != 0) {
        coincidenceSecondVector[1] <- 1
        periodFlag <- TRUE
        coincidenceNumber <- 1
      }
      
      for(i in 2:vectorLeng) {
        if(coincidenceVector[i] != 0) {
          if(coincidenceVector[i] != coincidenceVector[i-1]) {
            coincidenceNumber <- coincidenceNumber + 1
          }
          
          coincidenceSecondVector[i] <- coincidenceNumber
        }
      }
    }
    
    chicken_data$coincidence <- coincidenceSecondVector
    chicken_data <- chicken_data %>%
      filter(coincidence != 0)
    
    from <- chicken_data %>%
      group_by(coincidence) %>%
      slice(1)
    
    to <- chicken_data %>%
      group_by(coincidence) %>%
      slice(n())
    
    result <- from[,c("time", "min")]
    colnames(result) <- c("from", "level")
    result$to <- to$time
    
    result <- result[, c(1, 3, 2)]
  }
  
  result
}

##########################################
# Change vector chicken id format
##########################################
change_chickens_name_format <- function(chicken_names = c("chicken_1")) {
  sapply(1:length(chicken_names), function(i) {
    chicken_names[i] <- sprintf("chicken%s_%s", chicken_number_by_name(chicken_names[i]), chicken_number_by_name(chicken_names[i]))
  })
}

##########################################
# Separate date range with more than one day
##########################################
separate_range_coincidences <- function(coincidences) {
  coincidencesOutput <- data.frame(from = as.Date(character()), to = as.Date(character()), level = integer(), stringsAsFactors = FALSE)
  
  coincidences <- coincidences %>%
    mutate(day_from = format(as.Date(from), "%Y-%m-%d")) %>%
    mutate(day_to = format(as.Date(to), "%Y-%m-%d")) %>%
    mutate(from = format(as.POSIXct(from), "%Y-%m-%d %H:%M:%S")) %>%
    mutate(to = format(as.POSIXct(to), "%Y-%m-%d %H:%M:%S"))
  
  for(i in 1: nrow(coincidences)) {
    coincidence <- coincidences[i,]
    
    if(coincidence$day_from == coincidence$day_to) {
      coincidencesOutput <- rbind(coincidencesOutput, data.frame(from = coincidence$from,  to = coincidence$to, level = coincidence$level))
    } else {
      
      daysRangeBetween <- seq(as.Date(coincidence$day_from, "%Y-%m-%d"), as.Date(coincidence$day_to, "%Y-%m-%d"), "day")
      countDays <- 1
      for(x in as.list(daysRangeBetween)) {
        if(countDays == 1) {
          dayTo <- as.Date(x, format='%Y-%m-%d') + hours(23) + minutes(59) + seconds(59)
          coincidencesOutput <- rbind(coincidencesOutput, data.frame(from = coincidence$from,  to = format(as.POSIXct(dayTo, "GMT"), "%Y-%m-%d %H:%M:%S"), level = coincidence$level))
        } else if(countDays == length(daysRangeBetween)) {
          coincidencesOutput <- rbind(coincidencesOutput, data.frame(from = format(as.Date(x), "%Y-%m-%d  %H:%M:%S"),  to = coincidence$to, level = coincidence$level))
        } else {
          dayTo <- as.Date(x, format='%Y-%m-%d') + hours(23) + minutes(59) + seconds(59)
          coincidencesOutput <- rbind(coincidencesOutput, data.frame(from = format(as.Date(x), "%Y-%m-%d  %H:%M:%S"),  to = format(as.POSIXct(dayTo, "GMT"), "%Y-%m-%d %H:%M:%S"), level = coincidence$level))
        }
        countDays <- countDays + 1
      }
    }
  }
  
  coincidencesOutput
}

##########################################
# Total movements levels
##########################################
total_changes_level_time_chickens <- function(chickens = c("chicken_1"), day_range = c(), hour_range = seq(0, 23, 1)) {
  movements <- movementsData[movementsData$chicken %in% chickens & movementsData$day %in% day_range & movementsData$hour %in% hour_range, c("time", "chicken", "previous_L", "current_L")]
  
  movements <- movements %>%
    group_by(previous_L, current_L) %>%
    summarise(value = n())
  
  colnames(movements) <- c("from", "to", "value")
  
  as.data.frame(movements)
}

##########################################
# Total movements level from - to
##########################################
total_movements_level_time_chickens_from_to <- function(chickens = c("chicken_1"), day_range = c(), hour_range = seq(0, 23, 1), level_from = 1, level_to = 2) {
  
  movements <- movementsData %>%
    filter(chicken %in% chickens & day %in% day_range & hour %in% hour_range & previous_L == level_from & current_L == level_to) %>%
    nrow()
  
  movements
}

##########################################
# Total movements level from
##########################################
total_movements_level_time_chickens_from <- function(chickens = c("chicken_1"), day_range = c(), hour_range = seq(0, 23, 1), level_from = 1) {
  
  movements <- movementsData %>%
    filter(chicken %in% chickens & day %in% day_range & hour %in% hour_range & previous_L == level_from) %>%
    nrow()
  
  movements
}

##########################################
# Total movements level to
##########################################
total_movements_level_time_chickens_to <- function(chickens = c("chicken_1"), day_range = c(), hour_range = seq(0, 23, 1), level_to = 2) {
  
  movements <- movementsData %>%
    filter(chicken %in% chickens & day %in% day_range & hour %in% hour_range & current_L == level_to) %>%
    nrow()
  
  movements
}

##########################################
# Probability movements levels from X to Y
##########################################
probability_changes_level_chickens_from_to <- function(chickens = c("chicken_1"), day_range = c(), hour_range = seq(0, 23, 1), level_from = 1, level_to = 2) {
  movements <- movementsData[movementsData$chicken %in% chickens & movementsData$day %in% day_range & movementsData$hour %in% hour_range,c("previous_M", "previous_L")]
  result <- 0

  if(nrow(movements) > 0) {
    from_to <- movements %>%
      filter(previous_M == sprintf("%s-%s", level_from, level_to)) %>%
      nrow()
    
    all_from <- movements %>%
      filter(previous_L == level_from) %>%
      nrow()

    if(all_from > 0) {
      result <- from_to/all_from
    }
  }

  round(result, 3)
}

##########################################
# Probability movements levels from X to Y by hour
##########################################
probability_changes_level_chickens_from_to_hour <- function(chickens = c("chicken_1"), day_range = c()) {
  result <- data.frame(matrix(nrow = 20, ncol = 25))
  colnames(result) <- c("Mov", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23")

  value <- 1

  movements <- movementsData[movementsData$chicken %in% chickens & movementsData$day %in% day_range, c("previous_L", "current_L", "hour")]
  
  for(i in 1:5) {
    for(y in 1:5) {
      if(i != y) {
        result[value, "Mov"] <- sprintf("%s-%s", i, y)

        for(t in 0:23) {
          hourMovementsFT <- movements %>%
            filter(previous_L == i & current_L == y & hour == t)

          if(nrow(hourMovementsFT) > 0) {
            hourMovementsF <- movements %>%
              filter(previous_L == i & hour == t)

            if(nrow(hourMovementsF) > 0) {
              result[value, sprintf("%s", t)] <- round(nrow(hourMovementsFT) / nrow(hourMovementsF), 3)
            } else {
              result[value, sprintf("%s", t)] <- 0.0
            }
            
          } else {
            result[value, sprintf("%s", t)] <- 0.0
          }
        }

        value <- value + 1
      }
    }
  }

  result
}

##########################################
# Probability movements levels from X to Y by periods
##########################################
probability_changes_level_chickens_from_to_periods <- function(chickens = c("chicken_1"), day_range = c()) {
  result <- data.frame(matrix(nrow = 20, ncol = 4))
  colnames(result) <- c("Mov", "1", "2", "3")

  value <- 1

  movements <- movementsData[movementsData$chicken %in% chickens & movementsData$day %in% day_range, c("previous_L", "current_L", "hour")]

  for(i in 1:5) {
    for(y in 1:5) {
      if(i != y) {
        result[value, "Mov"] <- sprintf("%s-%s", i, y)

        for(p in 1:3) {
          periodMovementsFT <- movements %>%
            filter(previous_L == i & current_L == y & hour %in% get_hours_per_period(p))

          if(nrow(periodMovementsFT) > 0) {
            periodMovementsF <- movements %>%
              filter(previous_L == i & hour %in% get_hours_per_period(p))

            result[value, sprintf("%s", p)] <- round(nrow(periodMovementsFT) / nrow(periodMovementsF), 3)
          } else {
            result[value, sprintf("%s", p)] <- 0.0
          }
        }

        value <- value + 1
      }
    }
  }

  colnames(result) <- c("Mov", "02-10", "10-16", "16-02")
  result
}

get_hours_per_period <- function(period_number = 1) {
  periodHours <- c(2:9)
  
  if(period_number == 2) {
    periodHours <- c(10:15)
  } else if(period_number == 3) {
    periodHours <- c(c(0:1), c(16:23))
  }
  
  periodHours
}


# dat <- read.csv('http://t.co/mN2RgcyQFc')[,c('date', 'pts')]
# library(rChartsCalmap)
# r1 <- calheatmap(x = 'date', y = 'pts',
#                  data = dat, 
#                  domain = 'month',
#                  start = "2012-10-27",
#                  width = "100%",
#                  legend = seq(10, 50, 10),
#                  itemName = 'point',
#                  range = 7
# )
# r1
# http://cal-heatmap.com/

# data("vaccines")
# library("viridis")
# 
# fntltp <- JS("function(){
#              return this.point.x + ' ' +  this.series.yAxis.categories[this.point.y] + ':<br>' +
#              Highcharts.numberFormat(this.point.value, 2);
#              }")
# 
# plotline <- list(
#   color = "#fde725", value = 1963, width = 2, zIndex = 5,
#   label = list(
#     text = "Vaccine Intoduced", verticalAlign = "top",
#     style = list(color = "#606060"), textAlign = "left",
#     rotation = 0, y = -5)
# )
# 
# hchart(vaccines, "heatmap", hcaes(x = year, y = state, value = count)) %>%
#   hc_colorAxis(stops = color_stops(10, rev(inferno(10))),
#                type = "logarithmic") %>%
#   hc_yAxis(reversed = TRUE, offset = -20, tickLength = 0,
#            gridLineWidth = 0, minorGridLineWidth = 0,
#            labels = list(style = list(fontSize = "8px"))) %>%
#   hc_tooltip(formatter = fntltp) %>%
#   hc_xAxis(plotLines = list(plotline)) %>%
#   hc_title(text = "Infectious Diseases and Vaccines") %>%
#   hc_legend(layout = "vertical", verticalAlign = "top",
#             align = "right", valueDecimals = 0) %>%
#   hc_size(height = 800)