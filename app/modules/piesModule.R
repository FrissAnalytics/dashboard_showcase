###
### The gauge module renders a gauge with a title and statistics below the gauge
###
piesModuleUI <- function(id){
  
  # set namespace via id
  ns <- NS(id)
  
  tagList(uiOutput(ns("Pies")))
}

# module server
piesModule <- function(input, output, session, moduleData){

  ns <- session$ns
  
  #### Render pies ----
  output$Pies <- renderUI({
    
    L <- list(
      br(),
      
      fluidRow(
        column(3,
               h4("Process", style = "text-align:center"),
               FrissC3PieChartOutput(ns("PieProcess"))
        ),
        column(3,
               h4("Label", style = "text-align:center"),
               FrissC3PieChartOutput(ns("PieLabel"))
        ),
        column(3,
               h4("Product", style = "text-align:center"),
               FrissC3PieChartOutput(ns("PieProduct"))
        ),
        column(3, style = "overflow: hidden;",
               h4("Branch", style = "text-align:center"),
               FrissC3PieChartOutput(ns("PieBranch"))
        )
      )
    )
    
    return(L)
    
  })
  
  # pie charts
  
  PieWidth <- "50%"
  
  output$PieProcess <-  renderFrissC3PieChart({

    lstData <- moduleData()
    FrissC3PieChart(lstData$procesCounts,height=250,width = PieWidth,maxGroups=5,legendPosition='right',dataHidden=diff)
  })
  
  output$PieLabel <-  renderFrissC3PieChart({
    
    lstData <- moduleData()
    FrissC3PieChart(lstData$labelCounts,height=250,width = PieWidth,maxGroups=5,legendPosition='right')
  })
  
  output$PieProduct <-  renderFrissC3PieChart({
    
    lstData <- moduleData()
    FrissC3PieChart(lstData$productCounts,height=250,width = PieWidth, maxGroups=5,legendPosition='right')
  })
  
  output$PieBranch <-  renderFrissC3PieChart({
    
    lstData <- moduleData()
    FrissC3PieChart(lstData$brancheCounts,height=250,width = PieWidth, maxGroups=5,legendPosition='right')
  })

  returnItems <- reactive({
    # This check makes sure the filter app has been fully loaded
    # This way we prevent an unnecessary trigger
    if(is.null(input$PieProcess)) 
      return(NULL) 
    else 
      return(list(PieProcess = input$PieProcess,PieLabel = input$PieLabel,PieProduct = input$PieProduct,PieBranch = input$PieBranch))
  })
  
  return(returnItems)

}

