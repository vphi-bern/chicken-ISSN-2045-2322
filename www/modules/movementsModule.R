movementsTabItemUI <- function(id, title = "Total Time") {
  ns <- NS(id)
  
  tabItem(id,
    fluidRow(
      tabBox(id = ns("tabBoxMovements"), width = 9, title = textOutput(ns("movementsTabBoxTitle")),
        tabPanel("By chicken", value = "byChicken",
          uiOutput(ns("chickenByChickenMovementsUI"))
        ),
        tabPanel("By day", value = "byDay",
          uiOutput(ns("chickenByDayMovementsUI"))
        )
      ),
      box(width = 3, title = "Controls",
        conditionalPanel(condition = sprintf("input['%s'] == 'byChicken'", ns("tabBoxMovements")),
          selectizeInput(ns("movementsChicken"), "Chicken", choices = chicken_names, selected = c("chicken_1"), multiple = FALSE, options = list(plugins = list('remove_button')))
        ),
        conditionalPanel(condition = sprintf("input['%s'] == 'byDay'", ns("tabBoxMovements")),
          selectInput(ns("movementsDay"), "Day", choices = uniqueDays$time, selected = uniqueDays$time[1])
        ),
        radioGroupButtons(inputId = ns("movementsTypeGraph"), label = "Graph type", choices = c(
          `<i class='fa fa-bar-chart'></i>` = "column",
          `<i class='fa fa-dot-circle-o'></i>` = "bubble",
          `<i class='fa fa-sitemap'></i>` = "network",
          `<i class='fa fa-pie-chart'></i>` = "pie",
          `<i class='fa fa-th'></i>` = "heatmap"
        ), selected = "column", justified = TRUE),
        conditionalPanel(condition = sprintf("input['%s'] == 'column'", ns("movementsTypeGraph")),
          switchInput(inputId = ns("graphTypeMovementsColumnPolar"), label = "Polar", offLabel = "No", onLabel = "Yes", value = TRUE)
        ),
        conditionalPanel(condition = sprintf("input['%s'] == 'pie' || input['%s'] == 'network'", ns("movementsTypeGraph"), ns("movementsTypeGraph")),
          sliderInput(inputId = ns("movementsHourRange"), "Choose Hour Range:", min = 0, max = 24, value = c(0, 24))
        ),
        conditionalPanel(condition = sprintf("input['%s'] == 'heatmap'", ns("movementsTypeGraph")),
          sliderInput(inputId = ns("movementsRangeSelector"), NULL, min = 1, max = 100, step = 1, value = c(1, 100)),
          switchInput(inputId = ns("movementsShowLabels"), label = "Labels", value = TRUE, onLabel = "ON", offLabel = "OFF", onStatus = NULL, offStatus = NULL, size = "default", disabled = FALSE)
        )
      ),
      conditionalPanel(condition = sprintf("input['%s'] == 'byChicken' && input['%s'] == 'network'", ns("tabBoxMovements"), ns("movementsTypeGraph")),
        uiOutput(ns("chickenByDayMovementsNetworkUI"))
      ),
      conditionalPanel(condition = sprintf("input['%s'] == 'heatmap'", ns("movementsTypeGraph")),
        uiOutput(ns("chickenHeatMapMovementsUI"))
      )
    ) 
  )
}

movementsTabItem <- function(input, output, session) {
  
  ns <- session$ns
  
  ########################################################
  # Render Main Box Title
  ########################################################
  output$movementsTabBoxTitle <- renderText({
    if(input$tabBoxMovements == "byChicken") {
      chicken_names_c[[input$movementsChicken]]
    } else {
      input$movementsDay
    }
  })
  
  ##########################################
  ##########################################
  ##
  ## Movements Network - Graphs Per chicken
  ##
  ##########################################
  ##########################################
  output$chickenByDayMovementsNetworkUI <- renderUI({
    req(length(seq(input$movementsHourRange[1], input$movementsHourRange[2], 1)) > 1)
    
    div(
      box(width = 4,
        formattableOutput(ns("movementsByChickenTable"))
      ),
      box(width = 5,
        visNetworkOutput(ns("movementsByChickenNetwork"), height = "300px")
      )
    )
  })
  
  output$movementsByChickenTable <- renderFormattable({
    req(input$movementsChicken)
    shinyjs::html("movementsByChickenTable", "Loading...")
    
    daysSeqString <- as.character(seq(as.Date(uniqueDays$time[1], "%Y-%m-%d"), as.Date(uniqueDays$time[nrow(uniqueDays)], "%Y-%m-%d"), "day"))
    hoursSeqString <- as.character(seq(input$movementsHourRange[1], input$movementsHourRange[2] - 1, 1))
    
    movements <- total_changes_level_time_chickens(c(input$movementsChicken), daysSeqString, hoursSeqString)
    
    formattable(movements, list('value' = color_bar("#E0E0E0"), title = FALSE), align = c('l', 'l', 'r'), col.names = c("From", "To", "Movements"))
  })
  
  
  output$movementsByChickenNetwork <- renderVisNetwork({
    req(input$movementsChicken)
    req(length(seq(input$movementsHourRange[1], input$movementsHourRange[2], 1)) > 1)
    
    shinyjs::html("movementsByChickenNetwork", "Loading...")
    
    daysSeqString <- as.character(seq(as.Date(uniqueDays$time[1], "%Y-%m-%d"), as.Date(uniqueDays$time[nrow(uniqueDays)], "%Y-%m-%d"), "day"))
    hoursSeqString <- as.character(seq(input$movementsHourRange[1], input$movementsHourRange[2] - 1, 1))
    
    nodes <- data.frame(id = 1:5, label = 1:5, color = unname(levelColorPaletteSparkline), shape = "ellipse")
    edges <- total_changes_level_time_chickens(c(input$movementsChicken), daysSeqString, hoursSeqString)
    edges <- edges %>%
      mutate(title = sprintf("From: %s, to: %s<br />%s mov", from, to, value))
    
    visNetwork(nodes, edges, main = "<span class='chickens-network-title'>All days</span>", height = "100%", width = "100%") %>%
      visEdges(
        arrows =list(to = list(enabled = T, scaleFactor = .01),
        scaling = list(min = 1, max = 3)), arrowStrikethrough = F,
        smooth = list(enabled = T)
      ) %>%
      visInteraction(dragNodes = TRUE, dragView = T, zoomView = T, tooltipDelay = 0) %>%  
      visLayout(randomSeed = 25)
  })
  
  ##########################################
  ##########################################
  ##
  ## Polar Graph - Graphs Per chicken
  ##
  ##########################################
  ##########################################
  output$chickenByChickenMovementsUI <- renderUI({
    req(input$movementsChicken)
    
    shinyjs::html("chickenByChickenMovementsUI", "Loading...")

    chicken_data <- timeSeriesData[timeSeriesData[sprintf("chicken_%s", chicken_number_by_name(input$movementsChicken))] != 0, c(sprintf("chicken_%s", chicken_number_by_name(input$movementsChicken)), "time", "day", "hour_time")]

    colnames(chicken_data) <- c("level", "time", "day", "hour")
    chicken_data$hour <- as.numeric(chicken_data$hour)

    daysPerChicken <- unique_days_per_chicken(input$movementsChicken)

    isPolar <- input$movementsTypeGraph == 'column' && input$graphTypeMovementsColumnPolar
      
    map(daysPerChicken, function(x) {
      
      totalMovementsData <- chicken_data %>%
        filter(day == x) %>%
        slice(2:n()) %>%
        group_by(hour, level) %>%
        summarize(count = n())
      
      if(input$movementsTypeGraph == 'column') {
        # hchart(totalMovementsData, "column", hcaes(x = hour, y = count, group = level)) %>%
        hchart(totalMovementsData, "column", hcaes(x = hour, y = count, group = level), color = levelColorPaletteSparkline[unique_levels_chicken_date(input$movementsChicken, x)]) %>%
          # hc_colors(unname(levelColorPaletteSparkline)) %>%
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
            valueSuffix = ' mov',
            shared = TRUE
          ) %>%
          hc_xAxis(
            tickInterval = 1,
            min = 0,
            max = if(isPolar) 24 else 23,
            labels = list(formatter = if(isPolar) {JS("function(){if(this.value == 0) return 24; else return this.value;}")} else {""}),
            title = list(text = "")
          ) %>%
          hc_yAxis(
            endOnTick = FALSE,
            maxPadding = 0,
            labels = list(enabled = FALSE),
            title = list(text = "")
          )
      } else if(input$movementsTypeGraph == 'bubble') {
        hchart(totalMovementsData, "point", hcaes(hour, level, size = count), maxSize = "5%") %>%
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
            headerFormat = sprintf("<b>%s</b><br>", chicken_names_c[[input$movementsChicken]]),
            pointFormat = "<b>Level</b>: {point.y}<br><b>Hour</b>: {point.x}<br><b>Total</b>: {point.size} mov"
          )
      } else if(input$movementsTypeGraph == 'network') {
        
        validate(
          need(length(seq(input$movementsHourRange[1], input$movementsHourRange[2], 1)) > 1, 'Choose at least one hour range!')
        )
        
        hoursSeqString <- as.character(seq(input$movementsHourRange[1], input$movementsHourRange[2] - 1, 1))
        
        nodes <- data.frame(id = 1:5, label = 1:5, color = unname(levelColorPaletteSparkline), shape = "ellipse")
        daysSeqString <- as.character(seq(as.Date(x, "%Y-%m-%d"), as.Date(x, "%Y-%m-%d"), "day"))
        edges <- total_changes_level_time_chickens(c(input$movementsChicken), daysSeqString, hoursSeqString)
        edges <- edges %>%
          mutate(title = sprintf("From: %s, to: %s<br />%s mov", from, to, value))
        
        visNetwork(nodes, edges, main = sprintf("<span class='chickens-network-title'>%s</span>", x), height = "100%", width = "100%") %>%
          visEdges(
            arrows =list(
              to = list(enabled = T, scaleFactor = .01), 
              scaling = list(min = 1, max = 3)
            ), 
            arrowStrikethrough = F,
            smooth = list(enabled = T)
          ) %>%
          visInteraction(dragNodes = TRUE, dragView = T, zoomView = T, tooltipDelay = 0) %>%  
          visLayout(randomSeed = 25)
      } else if(input$movementsTypeGraph == 'pie') {
        validate(
          need(length(seq(input$movementsHourRange[1], input$movementsHourRange[2], 1)) > 1, 'Choose at least one hour range!')
        )
        
        hoursSeqString <- as.character(seq(input$movementsHourRange[1], input$movementsHourRange[2] - 1, 1))
        
        daysSeqString <- as.character(seq(as.Date(x, "%Y-%m-%d"), as.Date(x, "%Y-%m-%d"), "day"))
        movements_pie <- total_changes_level_time_chickens(c(input$movementsChicken), daysSeqString, hoursSeqString)
        
        movements_pie <- movements_pie[order(movements_pie$to),]
        movements_internalGraph <- movements_pie[,c("to", "value")] %>%
          group_by(to) %>%
          summarize(total = sum(value))
        
        highchart(height = 300) %>%
          hc_title(
            text = x,
            style = list(
              fontSize = "12px"
            )
          )%>%
          hc_add_series_labels_values(
            movements_internalGraph$to,
            movements_internalGraph$total,
            type = "pie", size = '60%',
            allowPointSelect = FALSE,
            cursor = 'pointer',
            showInLegend = T,
            dataLabels = list(enabled = FALSE),
            colors = unname(levelColorPaletteSparkline[unique(movements_internalGraph$to)]),
            tooltip = list(
              headerFormat = '<span style="font-size: 10px">To level: {point.key}</span><br/>'
            )
          ) %>%
          hc_add_series_labels_values(
            movements_pie$from,
            movements_pie$value,
            type = "pie",
            size = '100%',
            innerSize = "60%",
            allowPointSelect = FALSE,
            cursor = 'pointer',
            showInLegend = F,
            dataLabels = list(enabled = FALSE),
            colors = unname(levelColorPaletteSparkline[movements_pie$from]),
            tooltip = list(
              headerFormat = '<span style="font-size: 10px">From level: {point.key}</span><br/>'
            )
          ) %>%
          hc_tooltip(
            pointFormat = '<span style="color:{point.color}">\u25CF</span><b>{point.y} mov</b><br/>'
          ) %>%
          hc_plotOptions(
            pie = list(
              point = list(
                events = list(legendItemClick = JS('function(){return false;}'))
              )
            )
          )
      } else if(input$movementsTypeGraph == 'heatmap') {
        heatMapTotalMovementsData <- movementsData %>%
          mutate(day = as.character(day)) %>%
          filter(chicken == input$movementsChicken & day == x) %>%
          group_by(hour, current_L) %>%
          summarize(count = n())
        
        hchart(heatMapTotalMovementsData, "heatmap", hcaes(x = hour, y = current_L, value = count)) %>%
          hc_colorAxis(stops = color_stops(10, rev(inferno(10))), type = "logarithmic") %>%
          hc_title(
            text = x,
            style = list(
              fontSize = "12px"
            )
          ) %>%
          hc_xAxis(
            tickInterval = 1,
            min = 0,
            max = 23,
            title = list(text = "Hour")
          ) %>%
          hc_yAxis(
            tickInterval = 1,
            min = 1,
            max = 5,
            endOnTick = FALSE,
            title = list(text = "Level")
          ) %>%
          hc_tooltip(
            headerFormat = sprintf("<b>%s</b><br>", chicken_names_c[[input$movementsChicken]]),
            pointFormat = "<b>Level</b>: {point.y}<br><b>Hour</b>: {point.x}<br><b>Total</b>: {point.value} mov"
          ) %>%
          hc_legend(layout = "horizontal", align = "right", margin = 5, padding = 3)
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
  output$chickenByDayMovementsUI <- renderUI({
    req(input$movementsDay)

    shinyjs::html("chickenByDayMovementsUI", "Loading...")

    chickensPerDay <- unique_chicken_per_day(input$movementsDay)

    all_chicken_data <- timeSeriesData[timeSeriesData$day == input$movementsDay,]

    isPolar = input$movementsTypeGraph == 'column' && input$graphTypeMovementsColumnPolar
    
    map(chickensPerDay, function(x) {
      columns <- c(sprintf("chicken_%s", chicken_number_by_name(x)), "hour_time")
      
      totalMovementsData <- all_chicken_data[all_chicken_data[sprintf("chicken_%s", chicken_number_by_name(x))] != 0, columns]
      colnames(totalMovementsData) <- c("level", "hour")
      totalMovementsData$hour <- as.numeric(totalMovementsData$hour)
      totalMovementsData <- totalMovementsData[2:nrow(totalMovementsData),]
      
      totalMovementsData <- totalMovementsData %>%
        # filter(number %% 2 == 0) %>%
        group_by(hour, level) %>%
        summarize(count = n())
      
      if(input$movementsTypeGraph == 'column') {
        hchart(totalMovementsData, "column", hcaes(x = hour, y = count, group = level), color = levelColorPaletteSparkline[unique_levels_chicken_date(x, input$movementsDay)]) %>%
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
            valueSuffix = ' mov',
            shared = TRUE
          ) %>%
          hc_xAxis(
            tickInterval = 1,
            min = 0,
            max = if(isPolar) 24 else 23,
            labels = list(formatter = if(isPolar) {JS("function(){if(this.value == 0) return 24; else return this.value;}")} else {""}),
            title = list(text = "")
          ) %>%
          hc_yAxis(
            endOnTick = FALSE,
            maxPadding = 0,
            labels = list(enabled = FALSE),
            title = list(text = "")
          )
      } else if(input$movementsTypeGraph == 'bubble') {
        hchart(totalMovementsData, "point", hcaes(hour, level, size = count), maxSize = "5%") %>%
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
            headerFormat = sprintf("<b>%s</b><br>", input$movementsDay),
            pointFormat = "<b>Level</b>: {point.y}<br><b>Hour</b>: {point.x}<br><b>Total</b>: {point.size} mov"
          )
      } else if(input$movementsTypeGraph == 'network') { 
        
        validate(
          need(length(seq(input$movementsHourRange[1], input$movementsHourRange[2], 1)) > 1, 'Choose at least one hour range!')
        )
        
        hoursSeqString <- as.character(seq(input$movementsHourRange[1], input$movementsHourRange[2] - 1, 1))
        
        nodes <- data.frame(id = 1:5, label = 1:5, color = unname(levelColorPaletteSparkline), shape = "ellipse")
        daysSeqString <- as.character(seq(as.Date(input$movementsDay, "%Y-%m-%d"), as.Date(input$movementsDay, "%Y-%m-%d"), "day"))
        edges <- total_changes_level_time_chickens(c(x), daysSeqString, hoursSeqString)
        edges <- edges %>%
          mutate(title = sprintf("From: %s, to: %s<br />%s mov", from, to, value))
        
        visNetwork(nodes, edges, main = sprintf("<span class='chickens-network-title'>%s</span>", chicken_names_c[[x]]), height = "100%", width = "100%") %>%
          visEdges(
            arrows =list(
              to = list(enabled = T, scaleFactor = .01), 
              scaling = list(min = 1, max = 3)
            ), 
            arrowStrikethrough = F,
            smooth = list(enabled = T)
          ) %>%
          visInteraction(dragNodes = T, dragView = T, zoomView = T, tooltipDelay = 0) %>%  
          visLayout(randomSeed = 25)
      } else if(input$movementsTypeGraph == 'pie') {
        
        validate(
          need(length(seq(input$movementsHourRange[1], input$movementsHourRange[2], 1)) > 1, 'Choose at least one hour range!')
        )
        
        hoursSeqString <- as.character(seq(input$movementsHourRange[1], input$movementsHourRange[2] - 1, 1))
        
        daysSeqString <- as.character(seq(as.Date(input$movementsDay, "%Y-%m-%d"), as.Date(input$movementsDay, "%Y-%m-%d"), "day"))
        movements_pie <- total_changes_level_time_chickens(c(x), daysSeqString, hoursSeqString)
        
        movements_pie <- movements_pie[order(movements_pie$to),]
        movements_internalGraph <- movements_pie[,c("to", "value")] %>%
          group_by(to) %>%
          summarize(total = sum(value))
        
        highchart(height = 300) %>%
          hc_title(
            text = chicken_names_c[[x]],
            style = list(
              fontSize = "12px"
            )
          ) %>%
          hc_add_series_labels_values(
            movements_internalGraph$to,
            movements_internalGraph$total,
            type = "pie", size = '60%',
            allowPointSelect = FALSE,
            cursor = 'pointer',
            showInLegend = T,
            dataLabels = list(enabled = FALSE),
            colors = unname(levelColorPaletteSparkline[unique(movements_internalGraph$to)]),
            tooltip = list(
              headerFormat = '<span style="font-size: 10px">To level: {point.key}</span><br/>'
            )
          ) %>%
          hc_add_series_labels_values(
            movements_pie$from,
            movements_pie$value,
            type = "pie",
            size = '100%',
            innerSize = "60%",
            allowPointSelect = FALSE,
            cursor = 'pointer',
            showInLegend = F,
            dataLabels = list(enabled = FALSE),
            colors = unname(levelColorPaletteSparkline[movements_pie$from]),
            tooltip = list(
              headerFormat = '<span style="font-size: 10px">From level: {point.key}</span><br/>'
            )
          ) %>%
          hc_tooltip(
            pointFormat = '<span style="color:{point.color}">\u25CF</span><b>{point.y} mov</b><br/>'
          ) %>%
          hc_plotOptions(
            pie = list(
              point = list(
                events = list(legendItemClick = JS('function(){return false;}'))
              )
            )
          )
      } else if(input$movementsTypeGraph == 'heatmap') {
        heatMapTotalMovementsData <- movementsData %>%
          filter(chicken == x & day == input$movementsDay) %>%
          group_by(hour, current_L) %>%
          summarize(count = n())
        
        hchart(heatMapTotalMovementsData, "heatmap", hcaes(x = hour, y = current_L, value = count)) %>%
          hc_colorAxis(stops = color_stops(10, rev(inferno(10))), type = "logarithmic") %>%
          hc_xAxis(
            tickInterval = 1,
            min = 0,
            max = 23,
            title = list(text = "Hour")
          ) %>%
          hc_title(
            text = chicken_names_c[[x]],
            style = list(
              fontSize = "12px"
            )
          ) %>%
          hc_yAxis(
            tickInterval = 1,
            min = 1,
            max = 5,
            endOnTick = FALSE,
            title = list(text = "Level")
          ) %>%
          hc_tooltip(
            headerFormat = sprintf("<b>%s</b><br>", chicken_names_c[[input$movementsChicken]]),
            pointFormat = "<b>Level</b>: {point.y}<br><b>Hour</b>: {point.x}<br><b>Total</b>: {point.value} mov"
          ) %>%
          hc_legend(layout = "horizontal", align = "right", margin = 5, padding = 3)
      }
    }) %>% hw_grid(rowheight = 300, ncol = 3) %>% browsable()
  })
  
  ##########################################
  ##########################################
  ##
  ## Movements Heatmap General
  ##
  ##########################################
  ##########################################
  output$chickenHeatMapMovementsUI <- renderUI({
    req(input$movementsTypeGraph == 'heatmap')
    
    dataHeatMap <- NULL
    dataHeatMapTable <- data.frame(matrix(nrow = 0, ncol = 7))
    
    titleText <- "All days"
    if(input$tabBoxMovements == "byChicken") {
      req(input$movementsChicken)
      
      dataHeatMap <- movementsData %>%
        filter(chicken == input$movementsChicken) %>%
        group_by(hour, current_L) %>%
        summarize(count = n())
    } else {
      req(input$movementsDay)
      
      titleText <- "All chickens"
      dataHeatMap <- movementsData %>%
        filter(day == input$movementsDay) %>%
        group_by(hour, current_L) %>%
        summarize(count = n())
    }
    
    for(i in 0:23) {
      heatMapVector <- rep(0, 7)
  
      hourHeatMap <- dataHeatMap %>%
        filter(hour == i)
      
      heatMapVector[1] <- i
      
      for(y in 1:5) {
        levelHeatMap <- subset(hourHeatMap, current_L == y)
        
        if(nrow(levelHeatMap)) {
          heatMapVector[y + 1] <- levelHeatMap$count[1]
        }
      }
      
      heatMapVector[7] <- sum(heatMapVector[2:6])
      dataHeatMapTable <- rbind(dataHeatMapTable, heatMapVector)
    }
    
    colnames(dataHeatMapTable) <- c("Hour", "1", "2", "3", "4", "5", "Total")
    
    # View(dataHeatMapTable)
    # View(dataHeatMap)
    column(width = 12, class = "chickens-fix-column-padding",
      box(width = 6,
        format_table(dataHeatMapTable, list(
          '1' = color_bar("#A4BAE8"),
          '2' = color_bar("#ED9C88"),
          '3' = color_bar("#FFCC7F"),
          '4' = color_bar("#87CA8B"),
          '5' = color_bar("#B2D47F"),
          'Total' = color_bar("#E0E0E0")
        ), align = c('l', 'r', 'r', 'r', 'r', 'r', 'r'), col.names = c("Hour", "Level 1", "Level 2", "Level 3", "Level 4", "Level 5", "Total"), table.attr = "class=\"table table-condensed chickens-movements-heatmap\"") %>%
          htmltools::HTML() %>%
          div() %>%
          spk_add_deps()
      ),
      box(width = 6,
        column(width = 12, class = "chickens-fix-column-padding",
          hchart(dataHeatMap, "heatmap", hcaes(x = hour, y = current_L, value = count)) %>%
            hc_colorAxis(stops = color_stops(10, rev(inferno(10))), type = "logarithmic") %>%
            hc_plotOptions(
              heatmap = list(
                dataLabels = list(
                  enabled = input$movementsShowLabels
                )
              )
            ) %>%
            hc_xAxis(
              tickInterval = 1,
              min = 0,
              max = 23,
              title = list(text = "Hour")
            ) %>%
            hc_title(
              text = titleText,
              style = list(
                fontSize = "12px"
              )
            ) %>%
            hc_yAxis(
              tickInterval = 1,
              min = 1,
              max = 5,
              endOnTick = FALSE,
              title = list(text = "Level")
            ) %>%
            hc_tooltip(
              headerFormat = sprintf("<b>%s</b><br>", input$movementsDay),
              pointFormat = "<b>Level</b>: {point.y}<br><b>Hour</b>: {point.x}<br><b>Total</b>: {point.value} mov"
            ) %>%
            hc_legend(
              layout = "horizontal", align = "right"
            )     
        )
      )
    )
  })
}