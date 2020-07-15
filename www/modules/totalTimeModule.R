totalTimeTabItemUI <- function(id, title = "Total Time") {
  ns <- NS(id)
  
  tabItem(id,
    fluidRow(
      tabBox(id = ns("tabBoxTotalTime"), width = 9, title = textOutput(ns("totalTimeTabBoxTitle")),
        tabPanel("By chicken", value = "byChicken",
          uiOutput(ns("chickenByChickenTotalTimeUI"))
        ),
        tabPanel("By day", value = "byDay",
          uiOutput(ns("chickenByDayTotalTimeUI"))
        )
      ),
      box(width = 3, title = "Controls",
        conditionalPanel(condition = sprintf("input['%s'] == 'byChicken'", ns("tabBoxTotalTime")),
          selectInput(ns("totalTimeChicken"), "Chicken", choices = chicken_names, selected = "chicken_1")
        ),
        conditionalPanel(condition = sprintf("input['%s'] == 'byDay'", ns("tabBoxTotalTime")),
          selectInput(ns("totalTimeDay"), "Day", choices = uniqueDays$time, selected = uniqueDays$time[1])
        ),
        radioGroupButtons(inputId = ns("totalTimeTypeGraph"), label = "Graph type", choices = c(`<i class='fa fa-bar-chart'></i>` = "column", `<i class='fa fa-dot-circle-o'></i>` = "bubble"), selected = "column", checkIcon = list(yes = tags$i(class = "fa fa-circle", style = "color: #286090"), no = tags$i(class = "fa fa-circle-o", style = "color: #286090")), justified = TRUE),
        conditionalPanel(condition = sprintf("input['%s'] == 'column'", ns("totalTimeTypeGraph")),
          switchInput(inputId = ns("graphTypeColumnPolar"), label = "Polar", offLabel = "No", onLabel = "Yes", value = TRUE)
        )
      )
    ) 
  )
}

totalTimeTabItem <- function(input, output, session) {
  
  ns <- session$ns
  
  ########################################################
  # Render Main Box Title
  ########################################################
  output$totalTimeTabBoxTitle <- renderText({
    if(input$tabBoxTotalTime == "byChicken") {
      chicken_names_c[[input$totalTimeChicken]]
    } else {
      input$totalTimeDay
    }
  })
  
  ##########################################
  ##########################################
  ##
  ## Polar Graph - Graphs Per chicken
  ##
  ##########################################
  ##########################################
  output$chickenByChickenTotalTimeUI <- renderUI({
    req(input$totalTimeChicken)
    
    shinyjs::html("chickenByChickenTotalTimeUI", "Loading...")
    
    chicken_data <- timeSeriesData[, c(sprintf("chicken%s_%s", chicken_number_by_name(input$totalTimeChicken), chicken_number_by_name(input$totalTimeChicken)), "time", "day", "hour_time")]
    
    colnames(chicken_data) <- c("level", "time", "day", "hour")
    chicken_data$hour <- as.numeric(chicken_data$hour)
    
    daysPerChicken <- unique_days_per_chicken(input$totalTimeChicken)
    
    isPolar = input$totalTimeTypeGraph == 'column' && input$graphTypeColumnPolar
    
    map(daysPerChicken, function(x) {
      
      totalTimeData <- chicken_data %>% 
        filter(day == x) %>%
        group_by(hour, level) %>%
        summarize(count = n())
      
      if(input$totalTimeTypeGraph == 'column') {
        hchart(totalTimeData, "column", hcaes(x = "hour", y = "count", group = "level"), color = levelColorPalette[unique_levels_chicken_date(input$totalTimeChicken, x)]) %>%
          hc_chart(polar = isPolar) %>%
          hc_title(
            text = x,
            style = list(
              fontSize = "12px"
            )
          ) %>%
          hc_plotOptions(
            series = list(stacking = "normal", pointPlacement = "between"),
            column = list(pointPadding = 0, groupPadding = 0)
          ) %>%
          hc_tooltip(
            # crosshairs = TRUE,
            headerFormat = '<span style="font-size: 10px"><b>Hour</b>: {point.key}</span><br/>',
            valueSuffix = ' sec',
            shared = TRUE
          ) %>%
          hc_xAxis(
            tickInterval = 1,
            min = 0,
            max = 24, 
            labels = list(formatter = if(isPolar) {JS("function(){if(this.value == 0) return 24; else return this.value;}")} else {""}),
            title = list(text = "")
          ) %>%
          hc_yAxis(
            endOnTick = FALSE,
            maxPadding = 0,
            labels = list(enabled = FALSE),
            title = list(text = "")
          )
      } else {
        hchart(totalTimeData, "point", hcaes(x = "hour", y = "level", size = count), maxSize = "5%") %>%
          hc_title(
            text = x,
            style = list(
              fontSize = "12px"
            )
          )%>%
          hc_xAxis(
            tickInterval = 1,
            max = 24,
            title = list(text = "Hour")
          ) %>%
          hc_yAxis(
            tickInterval = 1,
            title = list(text = "Level"),
            endOnTick = FALSE
          ) %>%
          hc_tooltip(
            headerFormat = sprintf("<b>%s</b><br>", chicken_names_c[[input$totalTimeChicken]]),
            pointFormat = "<b>Level</b>: {point.y}<br><b>Hour</b>: {point.x}<br><b>Total</b>: {point.size} sec"
          )
      }
    }) %>% hw_grid(rowheight = 300, ncol = 3) %>% browsable()
  })
  
  ##########################################
  ##########################################
  ##
  ## Polar Graph - Graphs Per Day
  ##
  ##########################################
  ##########################################
  output$chickenByDayTotalTimeUI <- renderUI({
    req(input$totalTimeDay)
    
    shinyjs::html("chickenByDayTotalTimeUI", "Loading...")
    
    chickensPerDay <- unique_chicken_per_day(input$totalTimeDay)
    
    all_chicken_data <- timeSeriesData[timeSeriesData$day == input$totalTimeDay,]
    
    isPolar = input$totalTimeTypeGraph == 'column' && input$graphTypeColumnPolar
    
    map(chickensPerDay, function(x) {
      
      columns <- c(sprintf("chicken%s_%s", chicken_number_by_name(x), chicken_number_by_name(x)), "hour_time")
      
      totalTimeData <- all_chicken_data[, columns]
      colnames(totalTimeData) <- c("level", "hour")
      totalTimeData$hour <- as.numeric(totalTimeData$hour)
      
      totalTimeData <- totalTimeData %>%
        group_by(hour, level) %>%
        summarize(count = n())
      
      # View(totalTimeData)
      
      if(input$totalTimeTypeGraph == 'column') {
        hchart(totalTimeData, "column", hcaes(x = "hour", y = "count", group = "level"), color = levelColorPalette[unique_levels_chicken_date(x, input$totalTimeDay)]) %>%
          hc_chart(polar = isPolar) %>%
          hc_title(
            text = chicken_names_c[[x]],
            style = list(
              fontSize = "12px"
            )
          ) %>%
          hc_plotOptions(
            series = list(stacking = "normal", pointPlacement = "between"),
            column = list(pointPadding = 0, groupPadding = 0)
          ) %>%
          hc_tooltip(
            # crosshairs = TRUE,
            headerFormat = '<span style="font-size: 10px"><b>Hour</b>: {point.key}</span><br/>',
            valueSuffix = ' sec',
            shared = TRUE
          ) %>%
          hc_xAxis(
            tickInterval = 1,
            min = 0,
            max = 24,
            labels = list(formatter = if(isPolar) {JS("function(){if(this.value == 0) return 24; else return this.value;}")} else {""}),
            title = list(text = "")
          ) %>%
          hc_yAxis(
            endOnTick = FALSE,
            maxPadding = 0,
            labels = list(enabled = FALSE),
            title = list(text = "")
          )
      } else {
        hchart(totalTimeData, "point", hcaes(x = "hour", y = "level", size = "count"), maxSize = "5%") %>%
          hc_title(
            text = chicken_names_c[[x]],
            style = list(
              fontSize = "12px"
            )
          ) %>%
          hc_xAxis(
            tickInterval = 1,
            max = 24,
            title = list(text = "Hour")
          ) %>%
          hc_yAxis(
            tickInterval = 1,
            title = list(text = "Level"),
            endOnTick = FALSE
          ) %>%
          hc_tooltip(
            headerFormat = sprintf("<b>%s</b><br>", input$totalTimeDay),
            pointFormat = "<b>Level</b>: {point.y}<br><b>Hour</b>: {point.x}<br><b>Total</b>: {point.size} sec"
          )
      }
    }) %>% hw_grid(rowheight = 300, ncol = 3) %>% browsable()
  })
}