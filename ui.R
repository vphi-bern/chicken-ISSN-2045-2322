dashboardPage(
  dashboardHeader(title = "Chickens", titleWidth = 180),
  dashboardSidebar(width = 180,
    sidebarMenu(id = "sidebarmenu",
      menuItem("Project", tabName = "projectDescription", icon = icon("book")),
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
      tabItem(tabName = "projectDescription",
        fluidRow(
          box(width = 12, title = "Finding hens in a haystack: Consistency of movement patterns within and across individual laying hens maintained in large groups",
            uiOutput("projectInformation")
          )
        )
      ),
      timeSeriesTabItemUI("timeSeries", title = "Time Series"),
      totalTimeTabItemUI("totalTime", title = "Total Time"),
      movementsTabItemUI("movements", title = "Movements"),
      comparisonTabItemUI("comparison", title = "Comparison")
      # statisticsTabItemUI("statistics", title = "Statistics")
    )
  )
)