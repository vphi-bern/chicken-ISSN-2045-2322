shinyServer(function(input, output, session) {
  
  ##########################################
  ##########################################
  ##
  ## Render Modules
  ##
  ##########################################
  ##########################################
  
  ## Project information
  ##########################################
  output$projectInformation <- renderUI({
    div(
      tags$h3("Abstract", class = "chickens-tab-title"),
      tags$p(HTML("We sought to objectively quantify and compare the recorded movement and location patterns of laying hens within a commercial system. Using a custom tracking system, we monitored the location within five zones of a commercial aviary for 13 hens within a flock of 225 animals for a contiguous period of 11 days. Most hens manifested a hen-specific pattern that was (visually) highly consistent across days, though, within that consistency, manifested stark differences between hens. Three different methods were used to classify individual daily datasets into groups based on their similarity: (i) Linear Discriminant Analysis based on six summary variables (transitions into each zone) and total transitions; (ii) Hierarchical Clustering, a na&#239;ve clustering analysis technique, applied to summary variables and iii) Hierarchical Clustering applied to dissimilarity matrices produced by Dynamic Time Warping. The three methods correctly classified more than 85% of the hen days and provided a unique means to assess behaviour of a system indicating a considerable degree of complexity and structure. We believe the current effort is the first to document these location and movement patterns within a large, complex commercial system with a large potential to influence the assessment of animal welfare, health, and productivity.")),
      tags$h3("How to cite", class = "chickens-tab-title"),
      HTML("<p>Rufener, C., Berezowski, J., Maximiano Sousa, F., Abreu, Y., Asher, L., Toscano, M.J., 2018. Finding hens in a haystack: consistency of movement patterns within and across individual laying hens maintained in large groups. Sci. Rep. 8. doi: <a href='https://doi.org/10.1038/s41598-018-29962-x' target='_blank'>10.1038/s41598-018-29962-x</a>"),
      tags$h4("About the app"),
      HTML("This app was built using <a href='http://www.r-project.org/' target='_blank'>R</a> and <a href='http://shiny.rstudio.com/' target='_blank'>Shiny</a>. All graphics were made with <a href='https://cran.rstudio.com/web/packages/dplyr/index.html' target='_blank'>dplyr</a> and <a href='https://cran.r-project.org/web/packages/highcharter/index.html' target='_blank'>highcharter</a> packages.<br /><a href='http://www.vphi.ch' target='_blank'>Veterinary Public Health Institute (VPHI)</a>, Universit&#228;t Bern.<br />&#169; All rights reserved")
    )
  })
  
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
