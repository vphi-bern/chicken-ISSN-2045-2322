dashboardPage(
  dashboardHeader(title = "Chickens", titleWidth = 180),
  dashboardSidebar(width = 180,
    sidebarMenu(id = "sidebarmenu",
      menuItem("Time series graphs", tabName = "timeSeries", icon = icon("area-chart")),
      menuItem("Total time", tabName = "totalTime", icon = icon("pie-chart")),
      menuItem("Movements", tabName = "movements", icon = icon("exchange")),
      menuItem("Comparison", tabName = "comparison", icon = icon("balance-scale"))
      # menuItem("Statistics", tabName = "statistics", icon = icon("line-chart"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "shortcut icon", href = "favicon.ico"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css")
    ),
    tabItems(
      timeSeriesTabItemUI("timeSeries", title = "Time Series"),
      totalTimeTabItemUI("totalTime", title = "Total Time"),
      movementsTabItemUI("movements", title = "Movements"),
      comparisonTabItemUI("comparison", title = "Comparison")
      # statisticsTabItemUI("statistics", title = "Statistics")
    )
  )
)