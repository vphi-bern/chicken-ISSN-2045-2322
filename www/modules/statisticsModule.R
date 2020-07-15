statisticsTabItemUI <- function(id, title = "Statistics") {
  ns <- NS(id)
  
  tabItem(id,
    fluidRow(
      tabBox(id = ns("tabBoxStatistics"), width = 9, title = textOutput(ns("statisticsTabBoxTitle")),
        tabPanel("Hourly", value = "hourly",
          uiOutput(ns("hourlyStatisticsUI"))
        ),
        tabPanel("Periods", value = "periods",
          uiOutput(ns("periodsStatisticsUI"))
        )
      ),
      box(width = 3, title = "Controls",
        selectizeInput(ns("statisticsChickens"), "Chickens", choices = chicken_names, selected = c("chicken_1"), multiple = TRUE, options = list(plugins = list('remove_button'))),
        materialSwitch(inputId = ns("statisticsChickensSelectAll"), label = "Select all chickens", value = FALSE, status = "primary", right = TRUE),
        sliderInput(inputId = ns("statisticsDayRange"), "Choose Date Range:", min = as.Date(uniqueDays$time[1]), max = as.Date(uniqueDays$time[nrow(uniqueDays)]), value = c(as.Date(uniqueDays$time[1]), as.Date(uniqueDays$time[nrow(uniqueDays)]))),
        conditionalPanel(condition = sprintf("input['%s'] == 'hourly'", ns("tabBoxStatistics")),
          sliderInput(inputId = ns("statisticsHourRange"), "Choose Hour Range:", min = 0, max = 24, value = c(0, 24))
        )
      ),
      conditionalPanel(condition = sprintf("input['%s'] == 'hourly'", ns("tabBoxStatistics")),
        uiOutput(ns("hourlyStatisticsHUI"))
      ),
      conditionalPanel(condition = sprintf("input['%s'] == 'periods'", ns("tabBoxStatistics")),
        uiOutput(ns("periodsStatisticsHUI"))
      )
    )
  )
}

statisticsTabItem <- function(input, output, session) {
  ns <- session$ns
  
  ########################################################
  # Render Main Box Title
  ########################################################
  output$statisticsTabBoxTitle <- renderText({
    "Name"
    # chicken_names_c[[input$statisticsChickens]]
  })
  
  ##########################################
  ##########################################
  ##
  ## Probabilities table - Levels hourly
  ##
  ##########################################
  ##########################################
  output$hourlyStatisticsUI <- renderUI({
    req(input$statisticsChickens)
    req(input$statisticsDayRange)
    
    validate(
      need(length(seq(input$statisticsHourRange[1], input$statisticsHourRange[2], 1)) > 1, 'Choose at least one hour range!'),
      need(length(input$statisticsChickens) > 0, 'Choose at least one chicken!')
    )
    
    shinyjs::html("hourlyStatisticsUI", "Loading...")
    
    daysSeqString <- as.character(seq(as.Date(input$statisticsDayRange[1], "%Y-%m-%d"), as.Date(input$statisticsDayRange[2], "%Y-%m-%d"), "day"))
    hoursSeqString <- as.character(seq(input$statisticsHourRange[1], input$statisticsHourRange[2] - 1, 1))
    
    statistics <- statisticsHourlyByChicken(input$statisticsChickens, daysSeqString, hoursSeqString)
    statistics$level <- 1:5
    
    statistics <- statistics[,c("level", "1", "2", "3", "4", "5")]
    
    # statistics$level <- 1:5
    toVector <- c("to")
    for(i in 1:5) {
      toVector <- c(toVector, total_movements_level_time_chickens_to(c("chicken_1"), daysSeqString, hoursSeqString, i))
    }
    
    statistics <- rbind(statistics, toVector)
    
    statistics$from <- "-"
    
    for(i in 1:5) {
      statistics$from[i] <- total_movements_level_time_chickens_from(c("chicken_1"), daysSeqString, hoursSeqString, i)
    }
    
    format_table(statistics, list(), align = c('c', 'c', 'c', 'c', 'c', 'c')) %>%
      htmltools::HTML() %>%
      div() %>%
      spk_add_deps()
  })
  
  ##########################################
  ##########################################
  ##
  ## Select all chickens
  ##
  ##########################################
  ##########################################
  observeEvent(input$statisticsChickensSelectAll, {
    if(input$statisticsChickensSelectAll) {
      updateSelectInput(session = session, inputId = "statisticsChickens", selected = chicken_names)
    }
  })
  
  ##########################################
  ##########################################
  ##
  ## Probabilities table - Levels hourly
  ##
  ##########################################
  ##########################################
  output$hourlyStatisticsHUI <- renderUI({
    req(input$statisticsChickens)
    req(input$statisticsDayRange)
    
    # validate(
    #   need(length(seq(input$statisticsHourRange[1], input$statisticsHourRange[2], 1)) > 1, 'Choose at least one hour range!')
    # )
    
    # shinyjs::html("hourlyStatisticsHUI", "Loading...")
    
    daysSeqString <- as.character(seq(as.Date(input$statisticsDayRange[1], "%Y-%m-%d"), as.Date(input$statisticsDayRange[2], "%Y-%m-%d"), "day"))
    
    statisticsHour <- probability_changes_level_chickens_from_to_hour(input$statisticsChickens, daysSeqString)
    levelsMov <- statisticsHour$Mov
    graphstatisticsHour <- data.frame(t(subset(statisticsHour, select = -c(Mov))))
    colnames(graphstatisticsHour) <- levelsMov
    
    levelsNumber <- 1:5
    fluidRow(
      column(12,
        box(width = 12, 
          format_table(statisticsHour, list(), align = rep('c', 25)) %>%
            htmltools::HTML() %>%
            div() %>%
            spk_add_deps()
        ),
        lapply(1:5, function(i) {
         box(width = 12,
           map(levelsNumber[!levelsNumber %in% i], function(x) {
             highchart() %>%
               hc_title(text = sprintf("%s-%s", i, x), style = list(fontSize = "15px")) %>%
               hc_add_series(as.vector(graphstatisticsHour[,c(sprintf("%s-%s", i, x))]), name = "Probability", showInLegend = FALSE, step = "left") %>%
               hc_xAxis(
                 tickInterval = 1,
                 max = 23
               ) %>%
               hc_yAxis(
                 tickInterval = 0.2,
                 max = 1,
                 endOnTick = TRUE
               ) %>%
               hc_tooltip(
                 crosshairs = TRUE
               ) %>%
               hc_plotOptions(
                 line = list(
                   marker = list(
                     enabled = FALSE
                   )
                 )
               )
           }) %>% hw_grid(rowheight = 200, ncol = 4) %>% browsable()
         )
        })
      )
    )
  })
  
  ##########################################
  ##########################################
  ##
  ## Probabilities table - Levels periods
  ##
  ##########################################
  ##########################################
  output$periodsStatisticsUI <- renderUI({
    req(input$statisticsChickens)
    req(input$statisticsDayRange)
    
    validate(
      need(length(seq(input$statisticsHourRange[1], input$statisticsHourRange[2], 1)) > 1, 'Choose at least one hour range!'),
      need(length(input$statisticsChickens) > 0, 'Choose at least one chicken!')
    )
    
    shinyjs::html("periodsStatisticsUI", "Loading...")
    
    daysSeqString <- as.character(seq(as.Date(input$statisticsDayRange[1], "%Y-%m-%d"), as.Date(input$statisticsDayRange[2], "%Y-%m-%d"), "day"))
    
    statisticsPeriod <- probability_changes_level_chickens_from_to_periods(input$statisticsChickens, daysSeqString)
    
    format_table(statisticsPeriod, list(
        '02-10' = color_bar("#A4BAE8"),
        '10-16' = color_bar("#A4BAE8"),
        '16-02' = color_bar("#A4BAE8")
      ), align = c('c', 'r', 'r', 'r')) %>%
      htmltools::HTML() %>%
      div() %>%
      spk_add_deps()
  })
  
  output$periodsStatisticsHUI <- renderUI({
    req(input$statisticsChickens)
    
    daysSeqString <- as.character(seq(as.Date(input$statisticsDayRange[1], "%Y-%m-%d"), as.Date(input$statisticsDayRange[2], "%Y-%m-%d"), "day"))
    
    statisticsPeriod <- probability_changes_level_chickens_from_to_periods(input$statisticsChickens, daysSeqString)
    
    levelsMov <- statisticsPeriod$Mov
    graphStatisticsPeriod <- data.frame(t(subset(statisticsPeriod, select = -c(Mov))))
    colnames(graphStatisticsPeriod) <- levelsMov
    
    levelsNumber <- 1:5
    lapply(1:5, function(i) {
      box(width = 12,
        map(levelsNumber[!levelsNumber %in% i], function(x) {
          highchart() %>%
            hc_title(text = sprintf("%s-%s", i, x), style = list(fontSize = "15px")) %>%
            hc_add_series(as.vector(graphStatisticsPeriod[,c(sprintf("%s-%s", i, x))]), name = "Probability", showInLegend = FALSE, step = "left") %>%
            hc_xAxis(
              categories = rownames(graphStatisticsPeriod)
            ) %>%
            hc_yAxis(
              tickInterval = 0.2,
              max = 1,
              endOnTick = TRUE
            ) %>%
            hc_tooltip(
              crosshairs = TRUE
            ) %>%
            hc_plotOptions(
              line = list(
                marker = list(
                  enabled = FALSE
                )
              )
            )
        }) %>% hw_grid(rowheight = 200, ncol = 4) %>% browsable()
      )
    })
  })
  
  ## Functions
  ##########################################
  statisticsHourlyByChicken <- function(chickens_name = c("chicken_1"), day_range = c(), hour_range = seq(0, 23, 1)) {
    result <- data.frame(matrix(nrow = 5, ncol = 5))
    colnames(result) <- c("1", "2", "3", "4", "5")
    for(i in 1:5) {
      for(y in 1:5) {
        if(i != y) {
          result[i, y] <- probability_changes_level_chickens_from_to(chickens_name, day_range, hour_range, i, y)
        }
      }
    }
    
    result
  }
}