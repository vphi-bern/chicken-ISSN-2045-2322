timeSeriesTabItemUI <- function(id, title = "Time Series") {
  ns <- NS(id)
  
  tabItem(id,
    fluidRow(
      tabBox(id = ns("tabBoxTimeSeries"), width = 9, title = textOutput(ns("timeSeriesTabBoxTitle")),
        tabPanel("All days", value = "allDays",
          dygraphOutput(outputId = ns("chickenGraph"), height = "300px")
        ),
        tabPanel("By chicken", value = "byChicken",
          uiOutput(ns("chickenByChickenGraphUI"))
        ),
        tabPanel("By day", value = "byDay",
          uiOutput(ns("chickenByDayGraphUI"))
        )
      ),
      box(width = 3, title = "Controls",
        conditionalPanel(condition = sprintf("input['%s'] == 'allDays' || input['%s'] == 'byChicken'", ns("tabBoxTimeSeries"), ns("tabBoxTimeSeries")),
          selectInput(inputId = ns("timeSeriesChicken"), "Chicken", choices = chicken_names, selected = "chicken_1")
        ),
        conditionalPanel(condition = sprintf("input['%s'] == 'allDays'", ns("tabBoxTimeSeries")),
          uiOutput(ns("sliderDateRangeSelectorUI"))
        ),
        conditionalPanel(condition = sprintf("input['%s'] == 'byChicken'", ns("tabBoxTimeSeries")),
          uiOutput(ns("dateToShowUI"))
        ),
        conditionalPanel(condition = sprintf("input['%s'] == 'byDay'", ns("tabBoxTimeSeries")),
          selectInput(ns("timeSeriesDay"), "Day", choices = uniqueDays$time, selected = uniqueDays$time[1]),
          uiOutput(ns("chickenToShowUI"))
        ),
        conditionalPanel(condition = sprintf("input['%s'] == 'byDay' || input['%s'] == 'byChicken'", ns("tabBoxTimeSeries"), ns("tabBoxTimeSeries")),
          switchInput(inputId = ns("graphOrdinal"), size = "mini", label = "Ordinal xaxis", offLabel = "No", onLabel = "Yes", value = TRUE) 
        )
      ),
      conditionalPanel(condition = sprintf("input['%s'] == 'allDays'", ns("tabBoxTimeSeries")),
        uiOutput(ns("chickenToShowTotalTimeUI")),
        uiOutput(ns("chickenToShowMovementsUI"))
      )
    )
  )
}

timeSeriesTabItem <- function(input, output, session) {
  
  ns <- session$ns
  
  ########################################################
  # Render Main Box Title
  ########################################################
  output$timeSeriesTabBoxTitle <- renderText({
    if(input$tabBoxTimeSeries == "allDays" || input$tabBoxTimeSeries == "byChicken") {
      chicken_names_c[[input$timeSeriesChicken]]
    } else {
      input$timeSeriesDay
    }
  })

  ##########################################
  ##########################################
  ##
  ## All days grap per chicken
  ##
  ##########################################
  ##########################################
  output$chickenGraph <- renderDygraph({
    req(input$timeSeriesDateRange)

    shinyjs::html("chickenGraph", "Loading...")

    dataPlot <- timeSeriesData[timeSeriesData[input$timeSeriesChicken] != 0, c("time", input$timeSeriesChicken, "day")]

    colnames(dataPlot) <- c("time", "level", "day")

    dataPlot <- dataPlot %>%
      filter(day >= as.Date(input$timeSeriesDateRange[1], "%Y-%m-%d") & day <= as.Date(input$timeSeriesDateRange[2], "%Y-%m-%d"))

    row.names(dataPlot) <- dataPlot$time

    dygraph(dataPlot["level"]) %>%
      dySeries("level", stepPlot = TRUE, drawPoints = FALSE, label = chicken_names_c[[input$timeSeriesChicken]]) %>%
      dyAxis("y", label = "Level", valueRange = c(1, NULL), axisLabelFormatter = 'function(d){if(Math.floor(d) == d) return d; else return "";}') %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"))
  })

  output$sliderDateRangeSelectorUI <- renderUI({
    dataChicken <- timeSeriesData[timeSeriesData[input$timeSeriesChicken] != 0, c("time", input$timeSeriesChicken)]
    colnames(dataChicken) <- c("time", "level")

    sliderInput(inputId = ns("timeSeriesDateRange"), "Choose Date Range:",
      min = as.Date(dataChicken$time[1], "%Y-%m-%d"),
      max = as.Date(dataChicken$time[nrow(dataChicken)], "%Y-%m-%d"),
      value = c(as.Date(dataChicken$time[1], "%Y-%m-%d"), as.Date(dataChicken$time[nrow(dataChicken)], "%Y-%m-%d"))

      # dateRangeInput("date_range", "Choose Date Range:",
      #   min = as.Date(dataChicken$time[1], "%Y-%m-%d"),
      #   start = as.Date(dataChicken$time[1], "%Y-%m-%d"),
      #   max = as.Date(dataChicken$time[nrow(dataChicken)], "%Y-%m-%d"),
      #   end = as.Date(dataChicken$time[nrow(dataChicken)], "%Y-%m-%d"),
      #   format = "dd-mm-yyyy"
    )
  })

  output$dateToShowUI <- renderUI({
    req(input$timeSeriesChicken)
    pickerInput(inputId = ns("timeSerieDayToShow"), label = "Choose Date:", multiple = TRUE,
      options = list('live-search' = TRUE, 'actions-box' = TRUE, title = "Select"),
      choices = unique_days_per_chicken(input$timeSeriesChicken),
      selected = unique_days_per_chicken(input$timeSeriesChicken)
    )
  })


  ##########################################
  ##########################################
  ##
  ## By chicken
  ##
  ##########################################
  ##########################################
  output$chickenByChickenGraphUI <- renderUI({
    
    validate(
      need(input$timeSerieDayToShow, 'Check at least one day!')
    )

    cantGraphs <- length(input$timeSerieDayToShow)
    graphSize <- cantGraphs * 120

    if(cantGraphs < 5) {
      graphSize <- 600
    }

    highchartOutput(outputId = ns("chickenByChickenGraph"), height = sprintf("%spx", graphSize))
  })

  output$chickenByChickenGraph <- renderHighchart({

    req(all(sort(input$timeSerieDayToShow) %in% unique_days_per_chicken(input$timeSeriesChicken)))

    shinyjs::html("chickenByChickenGraph", "Loading...")

    dataPlot <- timeSeriesData[timeSeriesData[input$timeSeriesChicken] != 0, c("time", input$timeSeriesChicken, "day")]
    colnames(dataPlot) <- c("time", "level", "day")

    dataPlot <- dataPlot %>%
      mutate(hour_time = as.POSIXct(strptime(paste("2016-08-20", format(as.POSIXct(time), "%H:%M:%S"), sep = " "), "%Y-%m-%d %H:%M:%S"), "GMT"))

    allDaysGraph <- highchart(type = "stock") %>%
      hc_chart(zoomType = "x", useUTC = FALSE) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_legend(enabled = TRUE) %>%
      hc_navigator(enabled = FALSE) %>%
      hc_xAxis(
        type = 'datetime',
        ordinal = input$graphOrdinal,
        dateTimeLabelFormats = list(
          second = '%H:%M:%S',
          minute = '%H:%M',
          hour = '%H:%M',
          day = '%H:%M'
        )
      ) %>%
      hc_yAxis(
        min = 1,
        max = 5,
        minPadding = 0,
        maxPadding = 1,
        startOnTick = FALSE,
        endOnTick = FALSE
      ) %>%
      hc_exporting(
        enabled = TRUE,
        sourceWidth = 600,
        sourceHeight = 850,
        fallbackToExportServer = FALSE
      ) %>%
      hc_plotOptions(
        series = list(
          events = list(legendItemClick = 'function(){return false;}')
        )
      ) %>%
      hc_rangeSelector(enabled = FALSE) %>%
      hc_tooltip(
        # crosshairs = TRUE,
        shared = TRUE,
        xDateFormat = '%H:%M:%S'
      ) %>%
      hc_yAxis_multiples(
        create_yaxis(naxis = length(input$timeSerieDayToShow), heights = rep(1, length(input$timeSerieDayToShow)), title = list(text = 'Levels'), allowDecimals = FALSE, min = 1, max = 6, startOnTick = F, tickInterval = 1)
      )

    yaxisSerie <- 0
    for(dayShow in input$timeSerieDayToShow) {
      chickenDataPerDay <- dataPlot %>%
        filter(day == dayShow)

      chickenDataPerDayTS <- xts(chickenDataPerDay$level, chickenDataPerDay$hour_time)

      allDaysGraph <- allDaysGraph %>%
        hc_add_series(chickenDataPerDayTS, name = dayShow, type = "line", yAxis = yaxisSerie, step = "left")

      yaxisSerie <- yaxisSerie + 1
    }
    allDaysGraph
  })

  ##########################################
  ##########################################
  ##
  ## By day
  ##
  ##########################################
  ##########################################
  output$chickenToShowUI <- renderUI({
    req(input$timeSeriesDay)
    pickerInput(inputId = ns("timeSerieChickenToShow"), label = "Choose chicken:", multiple = TRUE,
      options = list('live-search' = TRUE, 'actions-box' = TRUE, title = "Select"),
      choices = chicken_names[chicken_names_c[unique_chicken_per_day(input$timeSeriesDay)]],
      selected = chicken_names[chicken_names_c[unique_chicken_per_day(input$timeSeriesDay)]]
    )
  })

  output$chickenByDayGraphUI <- renderUI({
    
    validate(
      need(input$timeSerieChickenToShow, 'Check at least one chicken!')
    )

    cantGraphs <- length(input$timeSerieChickenToShow)
    graphSize <- cantGraphs * 120

    if(cantGraphs < 5) {
      graphSize <- 600
    }

    highchartOutput(outputId = ns("chickenByDayGraph"), height = sprintf("%spx", graphSize))
  })

  output$chickenByDayGraph <- renderHighchart({
    
    req(all(sort(input$timeSerieChickenToShow) %in% unique_chicken_per_day(input$timeSeriesDay)))

    shinyjs::html("chickenByDayGraph", "Loading...")

    dataPlot <- timeSeriesData[timeSeriesData$day == input$timeSeriesDay,]

    allChickensGraph <- highchart(type = "stock") %>%
      hc_chart(zoomType = "x", useUTC = FALSE) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_legend(enabled = TRUE) %>%
      hc_navigator(enabled = FALSE) %>%
      hc_xAxis(
        type = 'datetime',
        ordinal = input$graphOrdinal,
        dateTimeLabelFormats = list(
          second = '%H:%M:%S',
          minute = '%H:%M',
          hour = '%H:%M',
          day = '%H:%M'
        )
      ) %>%
      hc_yAxis(
        min = 1,
        max = 5,
        minPadding = 0,
        maxPadding = 1,
        startOnTick = FALSE,
        endOnTick = FALSE
      ) %>%
      hc_exporting(
        enabled = TRUE,
        sourceWidth = 600,
        sourceHeight = 850,
        fallbackToExportServer = FALSE
      ) %>%
      hc_plotOptions(
        series = list(
          events = list(legendItemClick = 'function(){return false;}')
        )
      ) %>%
      hc_rangeSelector(enabled = FALSE) %>%
      hc_tooltip(
        # crosshairs = TRUE,
        shared = TRUE,
        xDateFormat = '%H:%M:%S'
      ) %>%
      hc_yAxis_multiples(
        create_yaxis(naxis = length(input$timeSerieChickenToShow), heights = rep(1, length(input$timeSerieChickenToShow)), title = list(text = 'Levels'), allowDecimals = FALSE, min = 1, max = 6, startOnTick = F, tickInterval = 1)
      )

    yaxisSerie <- 0
    for(chickenShow in input$timeSerieChickenToShow) {
      daysDataPerChicken <- dataPlot[dataPlot[chickenShow] != 0, c("time", chickenShow)] %>%
        mutate(hour_time = as.POSIXct(strptime(time, "%Y-%m-%d %H:%M:%S"), "GMT"))

      chickenDataPerDayTS <- xts(daysDataPerChicken[chickenShow], daysDataPerChicken$hour_time)

      allChickensGraph <- allChickensGraph %>%
        hc_add_series(chickenDataPerDayTS, name = chicken_names_c[[chickenShow]], type = "line", yAxis = yaxisSerie, step = "left")

      yaxisSerie <- yaxisSerie + 1
    }
    allChickensGraph
  })

  ##########################################
  ##########################################
  ##
  ## Extra information total time
  ##
  ##########################################
  ##########################################
  output$chickenToShowTotalTimeUI <- renderUI({
    req(input$timeSeriesDateRange)

    shinyjs::html("chickenToShowExtraInfUI", "Loading...")

    daysSeq <- seq(as.Date(input$timeSeriesDateRange[1], "%Y-%m-%d"), as.Date(input$timeSeriesDateRange[2], "%Y-%m-%d"), "day")
    daysSeqString <- as.character(daysSeq)

    dataTableChicken <- timeSeriesData[, c(sprintf("chicken%s_%s", chicken_number_by_name(input$timeSeriesChicken), chicken_number_by_name(input$timeSeriesChicken)), "day")]
    colnames(dataTableChicken) <- c("level", "day")

    finalSummarizeLevel <- as.data.frame(matrix(1:5, nrow = 5, ncol = 1))
    colnames(finalSummarizeLevel) <- c("level")

    for(x in as.list(daysSeq)) {
      totalTimeData <- dataTableChicken %>%
        filter(day == format(x, "%Y-%m-%d")) %>%
        group_by(day, level) %>%
        summarize(count = n())

      colnames(totalTimeData) <- c("day", "level", format(x, "%Y-%m-%d"))
      finalSummarizeLevel <- merge(finalSummarizeLevel, subset(totalTimeData, select = -c(day)), all = TRUE)
      finalSummarizeLevel[,format(x, "%Y-%m-%d")][is.na(finalSummarizeLevel[,format(x, "%Y-%m-%d")])] <- 0
    }

    row.names(finalSummarizeLevel) <- finalSummarizeLevel$level

    finalSummarizeLevel <- subset(finalSummarizeLevel, select = -c(level))

    rowsSum <- data.frame(rowSums(finalSummarizeLevel, na.rm = FALSE))
    colnames(rowsSum) <- c("Total")
    rownames(rowsSum) <- c("Level 1", "Level 2", "Level 3", "Level 4", "Level 5")
    rowsSum$level <- c("1", "2", "3", "4", "5")
    rowsSum$color <- levelColorPaletteSparkline

    finalSummarizeLevel <- data.frame(t(finalSummarizeLevel))

    finalSummarizeLevel$Date <- rownames(finalSummarizeLevel)
    finalSummarizeLevel <- finalSummarizeLevel[c(6, 1, 2, 3, 4, 5)]
    rownames(finalSummarizeLevel) <- c()

    colnames(finalSummarizeLevel) <- c("Date", "L1", "L2", "L3", "L4", "L5")
    finalSummarizeLevel$graph <- with(finalSummarizeLevel, sprintf("%s", paste(L1, L2, L3, L4, L5, sep=",")))

    for(i in 1: nrow(finalSummarizeLevel)) {
      finalSummarizeLevel$graph[i] <- spk_chr(
        as.numeric(strsplit(finalSummarizeLevel$graph[i], ",")[[1]]), type = "pie", width = 30, height = 30
      )
    }

    colnames(finalSummarizeLevel) <- c("Date", "Level 1", "Level 2", "Level 3", "Level 4", "Level 5", "Graph")

    boxPlotDataTotalTime <- dataTableChicken %>%
      filter(day %in% daysSeqString) %>%
      group_by(day, level) %>%
      summarize(count = n())

    fluidRow(
      column(12,
        box(width = 12, title = "Total time",
          format_table(finalSummarizeLevel, list(
            'Level 1' = color_bar("#A4BAE8"),
            'Level 2' = color_bar("#ED9C88"),
            'Level 3' = color_bar("#FFCC7F"),
            'Level 4' = color_bar("#87CA8B"),
            'Level 5' = color_bar("#B2D47F")
          ), align = c('l', 'r', 'r', 'r', 'r', 'r', 'c')) %>%
            htmltools::HTML() %>%
            div() %>%
            spk_add_deps()
        ),
        

        box(width = 6, class = "chickens-plot-height",
          hchart(boxPlotDataTotalTime, "column", hcaes(x = "day", y = "count", group = "level")) %>%
            hc_chart(
              height = 300
            ) %>%
            hc_xAxis(
              title = list(text = ""),
              labels = list(
                formatter = JS("function(){return moment(this.value).format('DD-MM');}")
              )
            ) %>%
            hc_legend(margin = 6) %>%
            hc_yAxis(title = list(text = "Seconds")) %>%
            hc_tooltip(
              pointFormat = '<span style="color:{point.color}">\u25CF</span> Level {series.name}: <b>{point.y} sec</b><br/>'
            ) %>%
            hc_colors(unname(levelColorPaletteSparkline)) %>%
            hc_plotOptions(
              column = list(pointPadding = 0, groupPadding = 0)
            )
        ),

        box(width = 3,
          highchart(height = 300) %>%
            hc_colors(unname(levelColorPaletteSparkline)) %>%
            hc_chart(
              marginTop = 0,
              marginRight = 0,
              marginLeft = 0
            ) %>%
            hc_plotOptions(
              pie = list(
                allowPointSelect = FALSE,
                cursor = 'pointer',
                showInLegend = TRUE,
                dataLabels = list(enabled = FALSE)
              ),
              series = list(
                point = list(
                  events = list(legendItemClick = 'function(){return false;}')
                )
              )
            ) %>%
            hc_legend(margin = 6) %>%
            hc_tooltip(
              headerFormat = '<span style="font-size: 10px"><b>Level</b>: {point.key}</span><br/>',
              valueSuffix = ' sec'
            ) %>%
            hc_add_series(rowsSum, "pie", hcaes(y = "Total", name = "level"), name = "Time")
            # hc_responsive(
            #   rules = list(
            #     list(
            #       condition = list(maxWidth  = 150),
            #       chartOptions = list(
            #         legend = list(
            #           labelFormat = '{name}'
            #         )
            #       )
            #     )
            #   )
            # )
        ),
        box(width = 3, class = "chickens-plot-height",
          hcboxplot(x = boxPlotDataTotalTime$count, var = boxPlotDataTotalTime$level) %>%
            hc_colors(unname(levelColorPaletteSparkline)) %>%
            hc_plotOptions(
              boxplot = list(
                colorByPoint = TRUE
              )
            ) %>%
            hc_chart(type = "column", height = 300) %>%
            hc_tooltip(
              formatter = JS('function(){var output = "";if(this.series.name == "Series 1") {output = "<span>Maximum: " + this.point.high +"</span><br /><span>Upper quartile: " + this.point.q3 + "</span><br /><span>Median: " + this.point.median +"</span><br /><span>Lower quartile: " + this.point.q1 + "</span><br /><span>Minimum: " + this.point.low + "</span>"} else {output = "<span>" + this.y + "</span>"} return output;}')
            ) %>%
            hc_yAxis(
              min = 0,
              maxPadding = 0,
              startOnTick = TRUE,
              endOnTick = FALSE
            )
        )
      )
    )
  })

  #########################################
  #########################################
  #
  # Extra information movements
  #
  #########################################
  #########################################
  output$chickenToShowMovementsUI <- renderUI({
    req(input$timeSeriesDateRange)

    shinyjs::html("chickenToShowExtraInfUI", "Loading...")

    daysSeq <- seq(as.Date(input$timeSeriesDateRange[1], "%Y-%m-%d"), as.Date(input$timeSeriesDateRange[2], "%Y-%m-%d"), "day")
    daysSeqString <- as.character(daysSeq)

    dataTableChicken <- timeSeriesData[timeSeriesData[sprintf("chicken_%s", chicken_number_by_name(input$timeSeriesChicken))] != 0, c(sprintf("chicken_%s", chicken_number_by_name(input$timeSeriesChicken)), "day")]
    # dataTableChicken <- timeSeriesData[timeSeriesData["chicken_1"] != 0, c("chicken_1", "day")]
    colnames(dataTableChicken) <- c("level", "day")

    finalCountingLevel <- as.data.frame(matrix(1:5, nrow = 5, ncol = 1))
    colnames(finalCountingLevel) <- c("level")

    for(x in as.list(daysSeq)) {
      totalTimeData <- dataTableChicken %>%
        filter(day == format(x, "%Y-%m-%d")) %>%
        slice(2:n()) %>%
        group_by(day, level) %>%
        summarize(count = n())

      colnames(totalTimeData) <- c("day", "level", format(x, "%Y-%m-%d"))
      finalCountingLevel <- merge(finalCountingLevel, subset(totalTimeData, select = -c(day)), all = TRUE)
      finalCountingLevel[,format(x, "%Y-%m-%d")][is.na(finalCountingLevel[,format(x, "%Y-%m-%d")])] <- 0
    }

    row.names(finalCountingLevel) <- finalCountingLevel$level

    finalCountingLevel <- subset(finalCountingLevel, select = -c(level))

    rowsSum <- data.frame(rowSums(finalCountingLevel, na.rm = FALSE))
    colnames(rowsSum) <- c("Total")
    rownames(rowsSum) <- c("Level 1", "Level 2", "Level 3", "Level 4", "Level 5")
    rowsSum$level <- c("1", "2", "3", "4", "5")
    rowsSum$color <- levelColorPaletteSparkline

    finalCountingLevel <- data.frame(t(finalCountingLevel))

    finalCountingLevel$Date <- rownames(finalCountingLevel)
    finalCountingLevel <- finalCountingLevel[c(6, 1, 2, 3, 4, 5)]
    rownames(finalCountingLevel) <- c()

    colnames(finalCountingLevel) <- c("Date", "L1", "L2", "L3", "L4", "L5")
    finalCountingLevel$graph <- with(finalCountingLevel, sprintf("%s", paste(L1, L2, L3, L4, L5, sep = ",")))

    for(i in 1: nrow(finalCountingLevel)) {
      finalCountingLevel$graph[i] <- spk_chr(
        as.numeric(strsplit(finalCountingLevel$graph[i], ",")[[1]]), type = "pie", width = 30, height = 30
      )
    }

    colnames(finalCountingLevel) <- c("Date", "Level 1", "Level 2", "Level 3", "Level 4", "Level 5", "Graph")

    boxPlotDataMovements <- dataTableChicken %>%
      filter(day %in% daysSeqString) %>%
      group_by(day, level) %>%
      summarize(count = n())

    nodes <- data.frame(id = 1:5, label = 1:5, color = unname(levelColorPaletteSparkline), shape = "ellipse")
    edges <- total_changes_level_time_chickens(c("chicken_1"), daysSeqString, seq(0, 23, 1))

    edges <- edges %>%
      mutate(title = sprintf("From: %s, to: %s<br />%s mov", from, to, value))

    edges <- edges[order(edges$to),]
    row.names(edges) <- NULL
    edges_internalGraph <- edges[,c("to", "value")] %>%
      group_by(to) %>%
      summarize(total = sum(value))

    fluidRow(
      column(12,
        box(width = 12, title = "Movements",
          format_table(finalCountingLevel, list(
            'Level 1' = color_bar("#A4BAE8"),
            'Level 2' = color_bar("#ED9C88"),
            'Level 3' = color_bar("#FFCC7F"),
            'Level 4' = color_bar("#87CA8B"),
            'Level 5' = color_bar("#B2D47F")
          ), align = c('l', 'r', 'r', 'r', 'r', 'r', 'c')) %>%
          htmltools::HTML() %>%
          div() %>%
          spk_add_deps()
        ),
        
        box(width = 6, class = "chickens-plot-height",
          hchart(boxPlotDataMovements, "column", hcaes(x = "day", y = "count", group = "level")) %>%
            hc_chart(
              height = 300
            ) %>%
            hc_xAxis(
              title = list(text = ""),
              labels = list(
                formatter = JS("function(){return moment(this.value).format('DD-MM');}")
              )
            ) %>%
            hc_tooltip(
              pointFormat = '<span style="color:{point.color}">\u25CF</span> Level {series.name}: <b>{point.y} mov</b><br/>'
            ) %>%
            hc_legend(margin = 6) %>%
            hc_yAxis(title = list(text = "Movements")) %>%
            hc_colors(unname(levelColorPaletteSparkline)) %>%
            hc_plotOptions(
              column = list(pointPadding = 0, groupPadding = 0)
            )
        ),

        box(width = 3,
         highchart(height = 300) %>%
           hc_colors(unname(levelColorPaletteSparkline)) %>%
           hc_chart(
             marginTop = 0,
             marginRight = 0,
             marginLeft = 0
           ) %>%
           hc_plotOptions(
             pie = list(
               allowPointSelect = FALSE,
               cursor = 'pointer',
               showInLegend = TRUE,
               dataLabels = list(enabled = FALSE)
             ),
             series = list(
               point = list(
                 events = list(legendItemClick = 'function(){return false;}')
               )
             )
           ) %>%
           hc_legend(margin = 6) %>%
           hc_tooltip(
             headerFormat = '<span style="font-size: 10px"><b>Level</b>: {point.key}</span><br/>',
             valueSuffix = ' mov'
           ) %>%
           hc_add_series(rowsSum, "pie", hcaes(y = "Total", name = "level"), name = "Movements")
           # hc_responsive(
           #   rules = list(
           #     list(
           #       condition = list(maxWidth  = 150),
           #       chartOptions = list(
           #         legend = list(
           #           labelFormat = '{name}'
           #         )
           #       )
           #     )
           #   )
           # )
        ),

        box(width = 3, class = "chickens-plot-height",
          hcboxplot(x = boxPlotDataMovements$count, var = boxPlotDataMovements$level) %>%
            hc_colors(unname(levelColorPaletteSparkline)) %>%
            hc_plotOptions(
              boxplot = list(
                colorByPoint = TRUE
              )
            ) %>%
            hc_tooltip(
              formatter = JS('function(){var output = "";if(this.series.name == "Series 1") {output = "<span>Maximum: " + this.point.high +"</span><br /><span>Upper quartile: " + this.point.q3 + "</span><br /><span>Median: " + this.point.median +"</span><br /><span>Lower quartile: " + this.point.q1 + "</span><br /><span>Minimum: " + this.point.low + "</span>"} else {output = "<span>" + this.y + "</span>"} return output;}')
            ) %>%
            hc_chart(type = "column", height = 300) %>%
            hc_yAxis(
              min = 0,
              maxPadding = 0,
              startOnTick = TRUE,
              endOnTick = FALSE
            )
        ),


        box(width = 4,
          visNetwork(nodes, edges, height = "280px", width = "100%") %>%
            visEdges(
              arrows =list(to = list(enabled = T, scaleFactor = .01),
              scaling = list(min = 1, max = 3)), arrowStrikethrough = F,
              smooth = list(enabled = T)
            ) %>%
            visInteraction(dragNodes = TRUE, dragView = T, zoomView = T, tooltipDelay = 0) %>%
            visLayout(randomSeed = 25)
        ),

        box(width = 4,
          highchart(height = 300) %>%
            hc_add_series_labels_values(
              edges_internalGraph$to,
              edges_internalGraph$total,
              type = "pie", size = '60%',
              allowPointSelect = FALSE,
              cursor = 'pointer',
              showInLegend = T,
              dataLabels = list(enabled = FALSE),
              colors = unname(levelColorPaletteSparkline[unique(edges_internalGraph$to)]),
              tooltip = list(
                headerFormat = '<span style="font-size: 10px">To level: {point.key}</span><br/>'
              )
            ) %>%
            hc_add_series_labels_values(
              edges$from,
              edges$value,
              type = "pie",
              size = '100%',
              innerSize = "60%",
              allowPointSelect = FALSE,
              cursor = 'pointer',
              showInLegend = F,
              dataLabels = list(enabled = FALSE),
              colors = unname(levelColorPaletteSparkline[edges$from]),
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
        ),

        box(width = 4,
          format_table(edges %>% select(from, to, value), list(
            'value' = color_bar("#E0E0E0")
          ), align = c('l', 'l', 'r'), col.names = c("From", "To", "Movements")) %>%
            htmltools::HTML() %>%
            div() %>%
            spk_add_deps()
        )
      )
    )
  })
}