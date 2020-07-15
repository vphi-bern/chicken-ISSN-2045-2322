shinyServer(function(input, output, session) {
  
  ##########################################
  ##########################################
  ##
  ## Render Modules
  ##
  ##########################################
  ##########################################
  
  ## Time Series
  ##########################################
  timeSeriesModulesOutput <- callModule(timeSeriesTabItem, "timeSeries")
  
  ## Total Time
  ##########################################
  totalTimeModulesOutput <- callModule(totalTimeTabItem, "totalTime")
  
  ## Movements
  ##########################################
  movementsModulesOutput <- callModule(movementsTabItem, "movements")
  
  ## Comparison
  ##########################################
  comparisonModulesOutput <- callModule(comparisonTabItem, "comparison")
  
  ## Comparison
  ##########################################
  # statisticsModulesOutput <- callModule(statisticsTabItem, "statistics")
})