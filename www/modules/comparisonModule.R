comparisonTabItemUI <- function(id, title = "Comparison") {
  ns <- NS(id)
  
  tabItem(id,
    fluidRow(
      tabBox(id = ns("tabBoxComparison"), width = 9,
        tabPanel("Comparison", value = "comparison",
          uiOutput(ns("chickenComparisonTabBoxUI"))
        ),
        tabPanel("Coincidences", value = "coincidences",
          uiOutput(ns("chickenCoincidencesTabBoxUI"))
        )
      ),
      box(width = 3, title = "Controls",
        radioGroupButtons(inputId = ns("comparisonType"), choices = c("Multiple" = "multiple", "Individual" = "individual"), selected = "multiple", checkIcon = list(yes = tags$i(class = "fa fa-circle", style = "color: #286090"), no = tags$i(class = "fa fa-circle-o", style = "color: #286090")), justified = TRUE),
        conditionalPanel(condition = sprintf("input['%s'] == 'multiple'", ns("comparisonType")),
          selectizeInput(ns("comparisonChicken"), "Chicken", choices = chicken_names, selected = c("chicken_1", "chicken_2"), multiple = TRUE, options = list(plugins = list('remove_button'))),
          materialSwitch(ns("comparisonMultipleSelectAll"), label = "Select all chickens", value = FALSE, status = "primary", right = TRUE)
        ),
        conditionalPanel(condition = sprintf("input['%s'] == 'individual'", ns("comparisonType")),
          selectizeInput(ns("comparisonIndividualFixed"), "Chicken", choices = chicken_names, selected = c("chicken_1"), multiple = F),
          selectizeInput(ns("comparisonIndividualWith"), "Compare with", choices = chicken_names, selected = c("chicken_2", "chicken_3"), multiple = T, options = list(plugins = list('remove_button'))),
          materialSwitch(ns("comparisonIndividualSelectAll"), label = "Select all chickens", value = FALSE, status = "primary", right = TRUE)
        ),
        sliderInput(inputId = ns("comparisonDateRange"), "Choose Date Range:", min = as.Date(uniqueDays$time[1]), max = as.Date(uniqueDays$time[nrow(uniqueDays)]), value = c(as.Date(uniqueDays$time[1]), as.Date(uniqueDays$time[nrow(uniqueDays)]))),
        sliderInput(inputId = ns("comparisonHourRange"), "Choose Hour Range:", min = 0, max = 24, value = c(0, 24)),
        radioGroupButtons(inputId = ns("comparisonHourRangeIncExc"), choices = c("Include" = "include", "Exclude" = "exclude"), selected = "include", checkIcon = list(yes = tags$i(class = "fa fa-circle", style = "color: #286090"), no = tags$i(class = "fa fa-circle-o", style = "color: #286090")), justified = TRUE),
        conditionalPanel(condition = sprintf("input['%s'] == 'coincidences' && input['%s'] == 'multiple'", ns("tabBoxComparison"), ns("comparisonType")),
          checkboxGroupButtons(inputId = ns("comparisonCoincidencesLevel"), label = "Show level", choices = 1:5, selected = 1:5, checkIcon = list(yes = tags$i(class = "fa fa-check-square", style = "color: #286090"), no = tags$i(class = "fa fa-square-o", style = "color: #286090")), justified = TRUE)
        )
      )
    ),
    conditionalPanel(condition = sprintf("input['%s'] == 'comparison' && input['%s'] == 'multiple'", ns("tabBoxComparison"), ns("comparisonType")),
      uiOutput(ns("chickenCoincidencesExtraInformationUI"))
    ),
    conditionalPanel(condition = sprintf("input['%s'] == 'comparison' && input['%s'] == 'individual'", ns("tabBoxComparison"), ns("comparisonType")),
      uiOutput(ns("chickenComparisonIndivudualUI"))
    )
  )
}

comparisonTabItem <- function(input, output, session) {
  
  ns <- session$ns
  
  ##########################################
  ##########################################
  ##
  ## Time serie Graph - chicken comparison
  ##
  ##########################################
  ##########################################
  output$chickenComparisonTabBoxUI <- renderUI({
    
    # req(input$comparisonType)
    cantGraphs <- 1
    if(input$comparisonType == "multiple") {
      cantGraphs <- length(input$comparisonChicken)
      graphSize <- cantGraphs * 120
    }
    
    if(cantGraphs < 4) {
      graphSize <- 400
    }
    
    highchartOutput(outputId = ns("chickenComparisonTimeSerieGraph"), height = sprintf("%spx", graphSize))
  })
  
  output$chickenComparisonTimeSerieGraph <- renderHighchart({
    
    req(input$comparisonDateRange)
    # req(length(input$comparisonChicken) > 1)
    # req(input$comparisonHourRangeIncExc)
    
    shinyjs::html("chickenComparisonTimeSerieGraph", "Loading...")
    
    daysSeqString <<- as.character(seq(as.Date(input$comparisonDateRange[1], "%Y-%m-%d"), as.Date(input$comparisonDateRange[2], "%Y-%m-%d"), "day"))
    hoursSeqString <- as.character(seq(input$comparisonHourRange[1], input$comparisonHourRange[2] - 1, 1))
    
    if(input$comparisonHourRangeIncExc == "exclude") {
      allDayHoursRange <- as.character(seq(0, 23, 1))
      hoursSeqString <- allDayHoursRange[!allDayHoursRange %in% hoursSeqString]
    }
    
    if(input$comparisonType == "multiple") {
      validate(
        need(length(input$comparisonChicken) > 1, 'Check at least two chickens!'),
        need(length(seq(input$comparisonHourRange[1], input$comparisonHourRange[2], 1)) > 1, 'Choose at least one hour range!')
      )
      
      dataPlot <- timeSeriesData[timeSeriesData$day %in% daysSeqString,]
      
      coincidences <- coincidences_levels_chicken(change_chickens_name_format(input$comparisonChicken), daysSeqString, hoursSeqString)
      # coincidences <- separate_range_coincidences(coincidences)
      
      coincidencesBands <- lapply(1:nrow(coincidences), function(i) {
        period <- coincidences[i,]
        list(from = as.numeric(as.POSIXct(period$from, "GMT", format="%Y-%m-%d %H:%M:%S")) * 1000, to = as.numeric(as.POSIXct(period$to, "GMT", format="%Y-%m-%d %H:%M:%S")) * 1000, color = unname(levelColorPaletteSparkline[period$level]))
      })
      
      allChickensGraphUI <- highchart(type = "stock") %>%
        hc_chart(zoomType = "x", useUTC = FALSE) %>%
        hc_add_theme(hc_theme_smpl()) %>%
        hc_legend(enabled = TRUE) %>%
        hc_navigator(enabled = FALSE) %>%
        hc_xAxis(
          type = 'datetime',
          plotBands = coincidencesBands,
          ordinal = FALSE,
          dateTimeLabelFormats = list(
            second = '%H:%M:%S',
            minute = '%H:%M:%S',
            hour = '%H:%M',
            day = '%e. %b',
            week = '%e. %b',
            month = '%e. %b'
          )
        ) %>%
        hc_yAxis(
          min = 1,
          max = 5,
          minPadding = 0,
          maxPadding = 1,
          allowDecimals = FALSE,
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
            events = list(legendItemClick = 'function(){return false;}'),
            # dataGrouping = FALSE
            dataGrouping = list(
              approximation = "open"
            )
          ),
          line = list(
            # lineWidth = 1,
            step = "left"
          )
        ) %>%
        hc_rangeSelector(enabled = FALSE) %>%
        hc_tooltip(
          crosshairs = TRUE,
          shared = FALSE,
          dateTimeLabelFormats = list(
            millisecond = '%d/%m %H:%M:%S',
            second = '%d/%m %H:%M:%S',
            minute = '%d/%m %H:%M:%S',
            hour = '%d/%m %H:%M',
            day = '%d/%m %H:%M',
            week = '%Y-%m-%d',
            month = '%Y-%m-%d',
            year = '%Y-%m-%d'
          ),
          xDateFormat = '%d/%m %H:%M:%S'
        ) %>%
        hc_yAxis_multiples(
          create_yaxis(naxis = length(input$comparisonChicken), heights = rep(1, length(input$comparisonChicken)), title = list(text = 'Levels'), allowDecimals = FALSE, min = 1, max = 6, startOnTick = F, tickInterval = 1)
        )
      
      yaxisSerie <- 0
      for(chickenShow in input$comparisonChicken) {
        daysDataPerChicken <- dataPlot[dataPlot[chickenShow] != 0, c("time", chickenShow)] %>%
          mutate(hour_time = as.POSIXct(strptime(time, "%Y-%m-%d %H:%M:%S"), "GMT"))
        
        chickenDataPerDayTS <- xts(daysDataPerChicken[chickenShow], daysDataPerChicken$hour_time)
        
        allChickensGraphUI <- allChickensGraphUI %>%
          hc_add_series(chickenDataPerDayTS, name = chicken_names_c[[chickenShow]], type = "line", yAxis = yaxisSerie, step = "left")
        
        yaxisSerie <- yaxisSerie + 1
      }
      
      allChickensGraphUI
    } else {
      validate(
        need(length(input$comparisonIndividualWith) >= 1, 'Check at least one chicken!')
      )
      
      coincidencesHeatMap <- data.frame(matrix(nrow = 0, ncol = 3))
      for(chicken in input$comparisonIndividualWith) {
        if(chicken != input$comparisonIndividualFixed) {
          coincidences <- coincidences_levels_chicken(change_chickens_name_format(c(input$comparisonIndividualFixed, chicken)), daysSeqString)
          coincidences <- separate_range_coincidences(coincidences) 
          
          coincidences <- coincidences %>%
            mutate(day = format(as.Date(from), "%Y-%m-%d")) %>%
            mutate(time = as.numeric(strptime(to, "%Y-%m-%d %H:%M:%S") - strptime(from, "%Y-%m-%d %H:%M:%S"), units = "secs")) %>%
            filter(time > 0) %>%
            group_by(day) %>%
            summarize(total = sum(time))
          
          coincidences$chicken <- chicken_number_by_name(chicken)
          
          coincidencesHeatMap <- rbind(coincidencesHeatMap, coincidences)
        }
      }
      
      hchart(coincidencesHeatMap, "heatmap", hcaes(x = day, y = chicken, value = total)) %>%
        hc_colorAxis(stops = color_stops(10, rev(inferno(10))), type = "logarithmic") %>%
        hc_plotOptions(
          heatmap = list(
            dataLabels = list(
              enabled = FALSE
            )
          )
        ) %>%
        hc_xAxis(
          title = list(text = "Day")
        ) %>%
        hc_yAxis(
          endOnTick = FALSE,
          title = list(text = "Chicken")
        ) %>%
        # hc_tooltip(
        #   headerFormat = sprintf("<b>%s</b><br>", input$movementsDay),
        #   pointFormat = "<b>Level</b>: {point.y}<br><b>Hour</b>: {point.x}<br><b>Total</b>: {point.value} mov"
        # ) %>%
        hc_legend(
          layout = "horizontal", align = "right"
        )
      # dataPlot <- timeSeriesData[timeSeriesData$day %in% daysSeqString, c("time", input$comparisonIndividualFixed)]
      # colnames(dataPlot) <- c("time", "chicken")
      # dataPlot <- dataPlot %>%
      #   filter(chicken != 0) %>%
      #   mutate(hour_time = as.POSIXct(strptime(time, "%Y-%m-%d %H:%M:%S"), "GMT"))
      # 
      # chickenDataPerDayTS <- xts(dataPlot$chicken, dataPlot$hour_time)
      # 
      # highchart(type = "stock") %>%
      #   hc_chart(zoomType = "x", useUTC = FALSE) %>%
      #   hc_add_theme(hc_theme_smpl()) %>%
      #   hc_legend(enabled = TRUE) %>%
      #   hc_navigator(enabled = FALSE) %>%
      #   hc_xAxis(
      #     type = 'datetime',
      #     ordinal = FALSE,
      #     dateTimeLabelFormats = list(
      #       second = '%H:%M:%S',
      #       minute = '%H:%M:%S',
      #       hour = '%H:%M',
      #       day = '%e. %b',
      #       week = '%e. %b',
      #       month = '%e. %b'
      #     )
      #   ) %>%
      #   hc_yAxis(
      #     min = 1,
      #     max = 5,
      #     minPadding = 0,
      #     maxPadding = 1,
      #     allowDecimals = FALSE,
      #     startOnTick = FALSE,
      #     endOnTick = FALSE
      #   ) %>%
      #   hc_exporting(
      #     enabled = TRUE,
      #     sourceWidth = 600,
      #     sourceHeight = 850,
      #     fallbackToExportServer = FALSE
      #   ) %>%
      #   hc_plotOptions(
      #     series = list(
      #       events = list(legendItemClick = 'function(){return false;}'),
      #       # dataGrouping = FALSE
      #       dataGrouping = list(
      #         approximation = "open"
      #       )
      #     ),
      #     line = list(
      #       step = "left"
      #     )
      #   ) %>%
      #   hc_rangeSelector(enabled = FALSE) %>%
      #   hc_tooltip(
      #     crosshairs = TRUE,
      #     shared = FALSE,
      #     dateTimeLabelFormats = list(
      #       millisecond = '%d/%m %H:%M:%S',
      #       second = '%d/%m %H:%M:%S',
      #       minute = '%d/%m %H:%M:%S',
      #       hour = '%d/%m %H:%M',
      #       day = '%d/%m %H:%M',
      #       week = '%Y-%m-%d',
      #       month = '%Y-%m-%d',
      #       year = '%Y-%m-%d'
      #     ),
      #     xDateFormat = '%d/%m %H:%M:%S'
      #   ) %>%
      #   
      #   # hc_chart(zoomType = "x", useUTC = FALSE) %>%
      #   # hc_add_theme(hc_theme_smpl()) %>%
      #   # hc_legend(enabled = TRUE) %>%
      #   # hc_navigator(enabled = FALSE) %>%
      #   # hc_xAxis(
      #   #   type = 'datetime',
      #   #   ordinal = FALSE,
      #   #   dateTimeLabelFormats = list(
      #   #     millisecond = '%d/%m %H:%M:%S',
      #   #     second = '%d/%m %H:%M:%S',
      #   #     minute = '%d/%m %H:%M:%S',
      #   #     hour = '%d/%m %H:%M',
      #   #     day = '%d/%m %H:%M',
      #   #     week = '%Y-%m-%d',
      #   #     month = '%Y-%m-%d',
      #   #     year = '%Y-%m-%d'
      #   #   )
      #   # ) %>%
      #   # hc_yAxis(
      #   #   min = 1,
      #   #   max = 5,
      #   #   minPadding = 0,
      #   #   maxPadding = 1,
      #   #   startOnTick = FALSE,
      #   #   endOnTick = FALSE
      #   # ) %>%
      #   # hc_exporting(
      #   #   enabled = TRUE,
      #   #   sourceWidth = 600,
      #   #   sourceHeight = 850,
      #   #   fallbackToExportServer = FALSE
      #   # ) %>%
      #   # hc_plotOptions(
      #   #   series = list(
      #   #     events = list(legendItemClick = 'function(){return false;}'),
      #   #     # dataGrouping = FALSE
      #   #     dataGrouping = list(
      #   #       approximation = "open"
      #   #     )
      #   #   ),
      #   #   line = list(
      #   #     # lineWidth = 1,
      #   #     step = "left"
      #   #   )
      #   # ) %>%
      #   # hc_rangeSelector(enabled = FALSE) %>%
      #   # hc_tooltip(
      #   #   # crosshairs = TRUE,
      #   #   shared = TRUE,
      #   #   xDateFormat = '%H:%M:%S'
      #   # ) %>%
      #   hc_add_series(chickenDataPerDayTS, name = chicken_names_c[[input$comparisonIndividualFixed]], type = "line")
    }
  })
  
  output$chickenCoincidencesTabBoxUI <- renderUI({
    req(input$comparisonDateRange)
    validate(
      need(length(input$comparisonChicken) > 1, 'Check at least two chickens!'),
      need(length(seq(input$comparisonHourRange[1], input$comparisonHourRange[2], 1)) > 1, 'Choose at least one hour range!'),
      need(length(input$comparisonCoincidencesLevel) >= 1, 'Choose at least one level!'),
      need(input$comparisonType == "multiple", 'Under construction!')
    )

    shinyjs::html("chickenCoincidencesTabBoxUI", "Loading...")

    daysSeqString <- as.character(seq(as.Date(input$comparisonDateRange[1], "%Y-%m-%d"), as.Date(input$comparisonDateRange[2], "%Y-%m-%d"), "day"))
    hoursSeqString <- as.character(seq(input$comparisonHourRange[1], input$comparisonHourRange[2] - 1, 1))
    
    if(input$comparisonHourRangeIncExc == "exclude") {
      allDayHoursRange <- as.character(seq(0, 23, 1))
      hoursSeqString <- allDayHoursRange[!allDayHoursRange %in% hoursSeqString]
    }

    coincidences <- coincidences_levels_chicken(change_chickens_name_format(input$comparisonChicken), daysSeqString, hoursSeqString)

    validate(
      need(nrow(coincidences) > 0, "No coincidences!")
    )
    
    coincidences <- separate_range_coincidences(coincidences)

    coincidences <- coincidences %>%
      mutate(time = as.numeric(strptime(to, "%Y-%m-%d %H:%M:%S") - strptime(from, "%Y-%m-%d %H:%M:%S"), units = "secs")) %>%
      filter(time > 0 & level %in% input$comparisonCoincidencesLevel)

    format_table(coincidences[,c(1, 2, 4, 3)], list('time' = color_bar("#A4BAE8")), align = c('l', 'l', 'r', 'r')) %>%
      htmltools::HTML() %>%
      div() %>%
      spk_add_deps()
  })
  
  output$chickenCoincidencesExtraInformationUI <- renderUI({
    req(input$comparisonDateRange)
    req(length(input$comparisonChicken) > 1)
    req(length(seq(input$comparisonHourRange[1], input$comparisonHourRange[2], 1)) > 1)

    daysSeqString <- as.character(seq(as.Date(input$comparisonDateRange[1], "%Y-%m-%d"), as.Date(input$comparisonDateRange[2], "%Y-%m-%d"), "day"))
    hoursSeqString <- as.character(seq(input$comparisonHourRange[1], input$comparisonHourRange[2] - 1, 1))
    
    if(input$comparisonHourRangeIncExc == "exclude") {
      allDayHoursRange <- as.character(seq(0, 23, 1))
      hoursSeqString <- allDayHoursRange[!allDayHoursRange %in% hoursSeqString]
    }
    
    coincidences <- coincidences_levels_chicken(change_chickens_name_format(input$comparisonChicken), daysSeqString, hoursSeqString)

    validate(
      need(nrow(coincidences) > 0, "No coincidences!")
    )

    allCoincidences <- separate_range_coincidences(coincidences)

    coincidencesSummary <- allCoincidences %>%
      mutate(time = as.numeric(strptime(to, "%Y-%m-%d %H:%M:%S") - strptime(from, "%Y-%m-%d %H:%M:%S"), units = "secs")) %>%
      filter(time > 0) %>%
      group_by(level) %>%
      summarize(total = sum(time))

    allCoincidences <- allCoincidences %>%
      mutate(day = format(as.Date(from), "%Y-%m-%d")) %>%
      mutate(time = as.numeric(strptime(to, "%Y-%m-%d %H:%M:%S") - strptime(from, "%Y-%m-%d %H:%M:%S"), units = "secs")) %>%
      filter(time > 0) %>%
      group_by(day, level) %>%
      summarize(total = sum(time))

    # Summary table per day
    finalSummarizeLevel <- as.data.frame(matrix(1:5, nrow = 5, ncol = 1))
    colnames(finalSummarizeLevel) <- c("level")

    for(x in as.list(daysSeqString)) {
      totalTimeData <- allCoincidences %>%
        filter(day == x)

      if(nrow(totalTimeData) > 0) {
        colnames(totalTimeData) <- c("day", "level", x)
        finalSummarizeLevel <- merge(finalSummarizeLevel, subset(totalTimeData, select = -c(day)), all = TRUE)
        finalSummarizeLevel[,x][is.na(finalSummarizeLevel[,x])] <- 0
      }
    }

    row.names(finalSummarizeLevel) <- finalSummarizeLevel$level
    finalSummarizeLevel <- subset(finalSummarizeLevel, select = -c(level))

    finalSummarizeLevel <- data.frame(t(finalSummarizeLevel))

    finalSummarizeLevel$Date <- rownames(finalSummarizeLevel)
    finalSummarizeLevel <- finalSummarizeLevel[c(6, 1, 2, 3, 4, 5)]
    rownames(finalSummarizeLevel) <- c()

    colnames(finalSummarizeLevel) <- c("Date", "L1", "L2", "L3", "L4", "L5")
    finalSummarizeLevel$graph <- with(finalSummarizeLevel, sprintf("%s", paste(L1, L2, L3, L4, L5, sep = ",")))

    for(i in 1: nrow(finalSummarizeLevel)) {
      finalSummarizeLevel$graph[i] <- spk_chr(
        as.numeric(strsplit(finalSummarizeLevel$graph[i], ",")[[1]]), type = "pie", width = 30, height = 30
      )
    }

    fluidRow(
      box(width = 12,
        format_table(finalSummarizeLevel, list(
          'L1' = color_bar("#A4BAE8"),
          'L2' = color_bar("#ED9C88"),
          'L3' = color_bar("#FFCC7F"),
          'L4' = color_bar("#87CA8B"),
          'L5' = color_bar("#B2D47F")
        ), align = c('l', 'r', 'r', 'r', 'r', 'r', 'c')) %>%
          htmltools::HTML() %>%
          div() %>%
          spk_add_deps()
      ),

      box(width = 3,
        highchart(height = 300) %>%
          hc_colors(unname(levelColorPaletteSparkline[coincidencesSummary$level])) %>%
          hc_tooltip(
            headerFormat = '<span style="font-size: 10px"><b>Level</b>: {point.key}</span><br/>',
            # pointFormat = '<span style="color:{point.color}">\u25CF</span>{series.name}: <b>{point.y} sec</b>'
            valueSuffix = ' sec'
          ) %>%
          hc_plotOptions(
            pie = list(
              cursor = 'pointer',
              showInLegend = TRUE,
              dataLabels = list(enabled = FALSE)
            )
          ) %>%
          hc_add_series(coincidencesSummary, "pie", hcaes(y = total, name = level), name = "Total Time")
      ),

      box(width = 4, class = "chickens-plot-height",
        hchart(allCoincidences, "column", hcaes(x = day, y = total, group = level)) %>%
          hc_chart(
            height = 300
          ) %>%
          hc_xAxis(
            labels = list(
              formatter = JS("function(){return moment(this.value).format('DD-MM');}")
            ),
            title = list(text = "")
          ) %>%
          hc_tooltip(
            pointFormat = '<span style="color:{point.color}">\u25CF</span> Level {series.name}: <b>{point.y} sec</b><br/>'
          ) %>%
          hc_legend(
            margin = 6
          ) %>%
          hc_yAxis(
            title = list(text = "")
          ) %>%
          hc_colors(unname(levelColorPaletteSparkline[sort(unique(allCoincidences$level))])) %>%
          hc_plotOptions(
            column = list(pointPadding = 0, groupPadding = 0)
          )
      ),

      box(width = 5, class = "chickens-plot-height",
        hchart(allCoincidences, "column", hcaes(x = level, y = total, group = day)) %>%
          hc_chart(
            height = 300
          ) %>%
          hc_xAxis(
            title = list(text = ""),
            allowDecimals = FALSE
          ) %>%
          hc_tooltip(
            valueSuffix = ' sec',
            headerFormat = '<span style="font-size: 10px"><b>Level</b>: {point.key}</span><br/>'
            # pointFormat = '<span style="color:{point.color}">\u25CF</span> Level {series.name}: <b>{point.y} mov</b><br/>'
          ) %>%
          hc_legend(
            labelFormatter = JS("function(){return moment(this.name).format('DD-MM');}"),
            margin = 6
          ) %>%
          hc_yAxis(
            title = list(text = "")
          ) %>%
          hc_plotOptions(
            # series = list(pointPlacement = "between"),
            column = list(pointPadding = 0, groupPadding = 0)
          )
      )
    )
  })
  
  output$chickenComparisonIndivudualUI <- renderUI({
    
    validate(
      need(length(input$comparisonIndividualWith) >= 1, 'Check at least one chicken!')
    )
    
    chickens <- input$comparisonIndividualWith
    fixedChicken <- input$comparisonIndividualFixed
    
    daysSeqString <- as.character(seq(as.Date(input$comparisonDateRange[1], "%Y-%m-%d"), as.Date(input$comparisonDateRange[2], "%Y-%m-%d"), "day"))
    hoursSeqString <- as.character(seq(input$comparisonHourRange[1], input$comparisonHourRange[2] - 1, 1))
    
    if(input$comparisonHourRangeIncExc == "exclude") {
      allDayHoursRange <- as.character(seq(0, 23, 1))
      hoursSeqString <- allDayHoursRange[!allDayHoursRange %in% hoursSeqString]
    }

    fluidRow(
      lapply(chickens, function(i) {
        
        if(fixedChicken != i) {
          coincidences <- coincidences_levels_chicken(change_chickens_name_format(c(fixedChicken, i)), daysSeqString, hoursSeqString)
          
          validate(
            need(nrow(coincidences) > 0, "No coincidences!")
          )
          
          allCoincidences <- separate_range_coincidences(coincidences)
          
          coincidencesSummary <- allCoincidences %>%
            mutate(time = as.numeric(strptime(to, "%Y-%m-%d %H:%M:%S") - strptime(from, "%Y-%m-%d %H:%M:%S"), units = "secs")) %>%
            filter(time > 0) %>%
            group_by(level) %>%
            summarize(total = sum(time))
          
          allCoincidences <- allCoincidences %>%
            mutate(day = format(as.Date(from), "%Y-%m-%d")) %>%
            mutate(time = as.numeric(strptime(to, "%Y-%m-%d %H:%M:%S") - strptime(from, "%Y-%m-%d %H:%M:%S"), units = "secs")) %>%
            filter(time > 0) %>%
            group_by(day, level) %>%
            summarize(total = sum(time))
          
          column(width = 12, class = "chickens-fix-column-padding",
             box(width = 3, class = "chickens-plot-height-short", title = chicken_names_c[[i]],
               highchart(height = 262) %>%
                 hc_colors(unname(levelColorPaletteSparkline[coincidencesSummary$level])) %>%
                 hc_tooltip(
                   headerFormat = '<span style="font-size: 10px"><b>Level</b>: {point.key}</span><br/>',
                   # pointFormat = '<span style="color:{point.color}">\u25CF</span>{series.name}: <b>{point.y} sec</b>'
                   valueSuffix = ' sec'
                 ) %>%
                 hc_plotOptions(
                   pie = list(
                     cursor = 'pointer',
                     showInLegend = TRUE,
                     dataLabels = list(enabled = FALSE)
                   )
                 ) %>%
                 hc_add_series(coincidencesSummary, "pie", hcaes(y = total, name = level), name = "Total Time")
             ),
             box(width = 4, class = "chickens-plot-height",
               hchart(allCoincidences, "column", hcaes(x = day, y = total, group = level)) %>%
                 hc_chart(
                   height = 300
                 ) %>%
                 hc_xAxis(
                   labels = list(
                     formatter = JS("function(){return moment(this.value).format('DD-MM');}")
                   ),
                   title = list(text = "")
                 ) %>%
                 hc_tooltip(
                   pointFormat = '<span style="color:{point.color}">\u25CF</span> Level {series.name}: <b>{point.y} sec</b><br/>'
                 ) %>%
                 hc_legend(
                   margin = 6
                 ) %>%
                 hc_yAxis(
                   title = list(text = "")
                 ) %>%
                 hc_colors(unname(levelColorPaletteSparkline[sort(unique(allCoincidences$level))])) %>%
                 hc_plotOptions(
                   column = list(pointPadding = 0, groupPadding = 0)
                 )
             ),
             box(width = 5, class = "chickens-plot-height",
               hchart(allCoincidences, "column", hcaes(x = level, y = total, group = day)) %>%
                 hc_chart(
                   height = 300
                 ) %>%
                 hc_xAxis(
                   title = list(text = ""),
                   allowDecimals = FALSE
                 ) %>%
                 hc_tooltip(
                   valueSuffix = ' sec',
                   headerFormat = '<span style="font-size: 10px"><b>Level</b>: {point.key}</span><br/>'
                   # pointFormat = '<span style="color:{point.color}">\u25CF</span> Level {series.name}: <b>{point.y} mov</b><br/>'
                 ) %>%
                 hc_legend(
                   labelFormatter = JS("function(){return moment(this.name).format('DD-MM');}"),
                   margin = 6
                 ) %>%
                 hc_yAxis(
                   title = list(text = "")
                 ) %>%
                 hc_plotOptions(
                   # series = list(pointPlacement = "between"),
                   column = list(pointPadding = 0, groupPadding = 0)
                 )
             )
          )
        }
      })
    )
  })
  
  ##########################################
  ##########################################
  ##
  ## Individual: Select all chickens
  ##
  ##########################################
  ##########################################
  observeEvent(input$comparisonIndividualSelectAll, {
    if(input$comparisonIndividualSelectAll) {
      updateSelectInput(session = session, inputId = "comparisonIndividualWith", selected = chicken_names)
    }
  })
  
  ##########################################
  ##########################################
  ##
  ## Multiple: Select all chickens
  ##
  ##########################################
  ##########################################
  observeEvent(input$comparisonMultipleSelectAll, {
    if(input$comparisonMultipleSelectAll) {
      updateSelectInput(session = session, inputId = "comparisonChicken", selected = chicken_names)
    }
  })
}