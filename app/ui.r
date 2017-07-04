shinyUI(
  
  navbarPage(id="main", windowTitle = "Friss analytics", position = "fixed-top", title = NULL, header = FrissHeader,
             
     tabPanel(title = "dashboard", value="Dashboard", icon = icon("dashboard"),
              addMessageBoxToPage(),
              frontPanelModuleUI("Dashboard")
     ),
     
     tabPanel(title = "filter", icon = icon("filter"), value="Filter",filterModuleUI("Filter")),
     
     navbarMenu("reports", icon = icon("bar-chart-o"),
                
                tabPanel(title = "hits", value='HitsModule',icon = icon("bar-chart-o"),hitsModuleUI("HitsModule")),
                
                tabPanel(title = "flow charts",value="FlowCharts", icon = icon("bar-chart-o"), flowChartModuleUI('FlowCharts'))
     
    )
  )
)
