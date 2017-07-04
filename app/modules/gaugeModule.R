###
### The gauge module renders a gauge with a title and statistics below the gauge
###
gaugeModuleUI <- function(id){
  
  # set namespace via id
  ns <- NS(id)
  
  tagList(uiOutput(ns("GaugeWithStats")))
  
}

gaugeModule <- function(input,output,session,title,gaugeData){
  
  # get namespace based on session
  ns <- session$ns
  
  ### Render percentages underneath gauges-----
  output$GaugeAdditionalStats <- renderUI({
    
    lstData <- gaugeData()
    
    values <- lstData$values 
    
    values <- values[values$color %in% lstData$gaugeStatColors,]
    
    ShowColorInfo(values)
  })
  
  ### Render gauges ----
  output$Gauge <- renderFrissC3Gauge({
    lstData <- gaugeData()
    FrissC3Gauge(value = lstData$value, showMinMax = TRUE, color=lstData$color)
  })
  
  output$GaugeWithStats <- renderUI({

    L <- list(div( h4(title, style = "text-align:center"),
                   FrissC3GaugeOutput(ns("Gauge")), style = "width:75%; margin: auto",
                   uiOutput(ns("GaugeAdditionalStats"))))
    return(L)
    
  })
}

#' dashboard front panel helper function
#'
#' @description Creates ui component to which shows color information underneath gauges
ShowColorInfo <- function(values){
  nColor <- length(colors)
  
  colors <- values$color
  
  displayColors <- colors
  displayColors[displayColors=="AMBER"] <- "ORANGE"
  
  values <- values$pct
  nColor <- length(colors)

  L <- fluidRow(style = "text-align: center",
                lapply(1:nColor, function(i){
                  column(3,
                         div(style = "color: 'black'",colors[i]),
                         h1("-",style = paste("font-size:300%; margin:1px; line-height:3px; color:",displayColors[i])),
                         h4(values[i])
                  )
                })
  )
  return(L)
}