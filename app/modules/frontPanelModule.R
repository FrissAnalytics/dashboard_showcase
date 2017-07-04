source("modules/gaugeModule.R")

# module ui
frontPanelModuleUI <- function(id){

  # set namespace via id
  ns <- NS(id)

  tagList(

    h3("dashboard"),

    fluidRow(
      column(3, uiOutput(ns("SelectResult"))),
      column(6, br(), helpText(textOutput(ns("RecordsInRange"))))
    ),

    fluidRow(
      column(3,
             gaugeModuleUI(ns("Gauge1"))
      ),
      column(3,
             gaugeModuleUI(ns("Gauge2"))
      ),
      column(3,
             gaugeModuleUI(ns("Gauge3"))
      ),
      column(3,
             gaugeModuleUI(ns("Gauge4"))
      )
    ),

    piesModuleUI(ns("Pies")),

    fluidRow(
      column(12, FrissC3LineBarChartOutput(ns('lineBarChart'), height = "300px"))
    ),

    fluidRow( id = "Dashboard-BrushInfo",
      column(offset = 3, 9, br(), helpText(textOutput(ns("BrushedRecordsInRange"))))
    ),

    fluidRow(
      div(id=ns("switch"), style="max-width: 150px", FrissSwitch(ns('chkPercentage'),'Use percentage')),
      column(12, FrissC3StackedAreaChartOutput(ns('stackedAreaChart')))
    )
  )
}

# module server
frontPanelModule <- function(input, output, session,
                             moduleData,
                             helpReactive,
                             helpData,
                             nextPage,
                             DefaultStartColor = 'RED',
                             DefaultColors=list('RED','GREEN','AMBER')){

# initialization ----
  ns <- session$ns

  RV <- reactiveValues(selectedColor = DefaultStartColor)

  #### Inits introJS dynamic values ----
  dynamicValues <- reactive({
    
    lstReturn <- list()
    
    lstReturn['%GAUGECOLOR%']      <- input$ResultType
    
    gauge2Value <- Gauge2Data()$value
    
    if(length(gauge2Value)==0)
      gauge2Value = 0
    
    lstReturn['%GAUGEPERCENTAGE%'] <- gauge2Value
    
    return(lstReturn)
  })
  
  ### Make sure the proper help is loaded.
  source("modules/helpLogic.R",local=TRUE)

# end initialization ----

# start gauge modules ----
  Gauge1Data <- reactive({
    lstGaugeData <- constructGaugeData(input$ResultType,brushedData())
    return(lstGaugeData)
  })

  Gauge2Data <- reactive({
    lstGaugeData <- constructGaugeData(input$ResultType,brushedData(),90)
    return(lstGaugeData)
  })

  Gauge3Data <- reactive({
    lstGaugeData <- constructGaugeData(input$ResultType,brushedData(),30)
    return(lstGaugeData)
  })

  Gauge4Data <- reactive({
    lstGaugeData <- constructGaugeData(input$ResultType,brushedData(),7)
    return(lstGaugeData)
  })

  Gauge1 <- callModule(gaugeModule, "Gauge1",title="Overall",     Gauge1Data)
  Gauge2 <- callModule(gaugeModule, "Gauge2",title="Last 90 days",Gauge2Data)
  Gauge3 <- callModule(gaugeModule, "Gauge3",title="Last 30 days",Gauge3Data)
  Gauge4 <- callModule(gaugeModule, "Gauge4",title="Last 7 days" ,Gauge4Data)
# end gauge modules ----

# start Pies modules ----
  piesData <- reactive({
    list(procesCounts=GetC3PieCounts(brushedData(),'proces',5),
                              labelCounts=GetC3PieCounts(brushedData(),'label',5),
                              brancheCounts=GetC3PieCounts(brushedData(),'branche',5),
                              productCounts=GetC3PieCounts(brushedData(),'product',5))})
  Pies     <- callModule(piesModule,"Pies",piesData)
# end Pies modules ----

### Reactives ---  
  
  ###
  ### Get current brush
  ###
  ### Not the bottleneck
  brush <- reactive({

    input$lineBarChart

      if(!is.null(input$lineBarChart)){

        lstData <- moduleData()
        
        minDate <- input$lineBarChart[1]
        maxDate <- input$lineBarChart[2]

        retBrush    <- lstData$DD.Signaleringen$detectiedatum >= minDate &
                       lstData$DD.Signaleringen$detectiedatum <= maxDate

        FrissC3Charts:::FrissC3ChartZoom(session,ns("stackedAreaChart"),minDate,maxDate)

      }else{
        retBrush <- TRUE
      }

      return(retBrush)
  })
  
  brushedData <- reactive({
    lstData <- moduleData()
    lstData$DD.Signaleringen[brush(),]
  })
  
  ###
  ### ChartTimeData: data for the line bar chart
  ###
  ### 1 second
  chartTimeData <- reactive({
    
    lstData <- moduleData()
    
    if(is.null(lstData$DD.Signaleringen)) return()
    
    retChartTimeData <- createChartCounts(lstData$DD.Signaleringen,'eindargumentatie',FALSE)
    
    return(retChartTimeData)
  })
  
  ###
  ### The line bar chart data contains additional totals that are calculated here
  ###
  
  lineChartData <- reactive({
    if(is.null(chartTimeData())) return()
    
    Counts <- chartTimeData()[,-1]
    
    if(class(Counts)=='numeric'){
      Counts = as.data.frame(Counts)
      names(Counts) = names(chartTimeData())[-1]
    }
    
    Total  <- apply(Counts,1,sum,na.rm = TRUE)
    Counts <- round(100*Counts / Total, 3)
    retData   <- data.frame(Time = chartTimeData()[1], Counts, Total)
    
    return(retData)
  })
  
  ###
  ### StreamChartData is a brushed version of ChartTimeData
  ###
  
  streamChartDataPercentage <- reactive({
    
    if(is.null(chartTimeData() )) return()
    
    retData                                <- chartTimeData()[,2:ncol(chartTimeData())]
    row.totals                             <- apply(retData,1,sum)
    
    retData           <-   round((retData/row.totals)*100,4)
    retData$Time      <-   chartTimeData()$Time
    
    return(retData)
  })
### End reactives   


#outputs ----
  # stackedAreaChart ----

  output$stackedAreaChart <-  renderFrissC3StackedAreaChart({

    if(!input$chkPercentage){
      lineData <- chartTimeData()[,!names(chartTimeData())%in%c('Time','Total')]
    }else{
      lineData <- streamChartDataPercentage()[,!names(streamChartDataPercentage())%in%c('Time','Total')]
    }

    colors   <- getColors(lineData)

    FrissC3StackedAreaChart(timeData=as.character(chartTimeData()$Time),
                            lineData=lineData,
                            color=colors,height=200)
  })

  ###
  ### Render line bar chart
  ###
  output$lineBarChart <- renderFrissC3LineBarChart({
    
    
    lineData <- lineChartData()[,!names(lineChartData())%in%c('Time','Total')]
    colors   <- getColors(lineData)
    
    FrissC3LineBarChart(as.character(lineChartData()$Time),
                        lineChartData()$Total,
                        lineData=lineData,
                        color=c('gray',colors),
                        showSubChart=TRUE,debounce=200,height=300,yLabel1="Total counts",yLabel2="Percentage",xLabel="Week")
  })
  
  #### Records in range in text on top of the page ----
  output$RecordsInRange <- renderText({
    
    lstData <- moduleData()
    
    if(is.null(lstData$DD.Signaleringen)) return()
    
    nRecords <- nrow(lstData$DD.Signaleringen)
    nAlerts  <- nrow(DD.Signaleringen)
    
    PortfolioStart = min(DD.Signaleringen$detectiedatum)
    PortfolioEnd   = max(DD.Signaleringen$detectiedatum)
    
    TextMessage <- paste0(nRecords,
                          " (", round(100*nRecords/nAlerts,3),
                          "%) records are in filter range out of ",nAlerts,".",
                          " Portfolio start: ", PortfolioStart, ", portfolio end: ", PortfolioEnd," (YYYY-MM-DD).")
    return(TextMessage)
  })
  
  ### End result selector ----
  output$SelectResult <- renderUI({
    
    lstData <- moduleData()
    
    if(is.null(lstData$DD.Signaleringen)) return()
    Choices <- unique(lstData$DD.Signaleringen[,'eindargumentatie'])
    
    isolate({selectedColor <- RV$selectedColor})
    
    if(!selectedColor %in% Choices)
      selectedColor <- Choices[1]
    
    selectizeInput(ns("ResultType"),"result:",choices = Choices, selected = selectedColor, width = "100px")
  })
  
  output$BrushedRecordsInRange <- renderText({
    
    lstData <- moduleData()
    
    if(is.null(lstData$DD.Signaleringen)) return()
    
    nRecords <- nrow(lstData$DD.Signaleringen)
    nAlerts  <- nrow(DD.Signaleringen)
    
    PortfolioStart = min(lstData$DD.Signaleringen$detectiedatum[brush()])
    PortfolioEnd   = max(lstData$DD.Signaleringen$detectiedatum[brush()])
    
    if(length(brush())==1){
      nBrushedRecords <- nrow(lstData$DD.Signaleringen)
    }else{
      nBrushedRecords <- sum(brush())
    }
    
    TextMessage <- paste0(nBrushedRecords, " (", round(100*nBrushedRecords/nRecords,3),
                          "%) records are in brushed range out of ", nRecords,".",
                          " Selected range start: ", PortfolioStart, ", selected range end: ", PortfolioEnd,".")
    return(TextMessage)
  })
  
# ----

### Initialiy we have a default selected color. The value of input$ResultType will be stored in this selected color so that the selected color
### will not change back to the initial default color after an update
  observeEvent(input$ResultType,{
    RV$selectedColor <- input$ResultType
  })


}

#' Calculates the percentages of diffenret endresults for a specified period
#'
#' @param DD.Signaleringen data frame with signaleringen table
#' @param period the relevant period as the number of days. For example if period = 7 the stats will be calculated of the last 7 days
#' @return data frame with two columns color and pct. Each endresult has an entry in color with a correpsonding percentage in pct
calcGaugeStats <- function(DD.Signaleringen,period=NA){
  nAlerts <- nrow(DD.Signaleringen)

  ind <- TRUE

  if(!is.na(period)){
    ind     <- DD.Signaleringen$Index <= period + min(DD.Signaleringen$Index)
    nAlerts <- sum(ind)
  }

  tt <- DD.Signaleringen %>% filter(ind) %>% group_by(eindargumentatie) %>%
                             summarise(pct =n())%>% mutate(pct = pct/nAlerts,pct = round(pct*100,2))

  tt <- as.data.frame(tt,stringsAsFactors = FALSE)
  names(tt) <- c("color","pct")

  return(tt)
}

#' Constructs a list with all data needed to render a gauge
#'
#' @param ResultType contains the selected color
#' @param DD.Signaleringen data frame with a dump from the signaleringen table
#' @param period the relevant period as the number of days. For example if period = 7 the stats will be calculated of the last 7 days
#' @return a named list with: values          : a list with gauge stats as calculated by calcGaugeStats
#'                            value           : the main value to display in the gauge.
#'                            color           : the color/endresult to display in the gauge.
#'                            gaugeStatColors : list with colors/endresults to display underneath the gauge.
constructGaugeData <- function(ResultType,DD.Signaleringen,period=NA){
  if(is.null(ResultType))return()

  color  <- ResultType
  values <- calcGaugeStats(DD.Signaleringen,period)
  value  <- values[values$color==color,'pct']
  gaugeStatColors <- list("RED","GREEN","AMBER","BLACK")

  return(list(values=values,value=value,color=color,gaugeStatColors=gaugeStatColors))
}

# helper function Pie charts
GetC3PieCounts <- function(DD.Signaleringen,field,maxGroups){

  counts <- DD.Signaleringen %>% group_by_(field) %>% summarise(count =n()) %>% arrange(desc(count))

  maxGroups <- min(nrow(counts),maxGroups)

  counts <- counts[1:maxGroups,]

  rest   <- nrow(DD.Signaleringen) - sum(counts$count)

  if(rest>0){
    counts <- rbind(counts,c("other",rest))
    counts <- as.data.frame(counts)
  }

  names(counts) <- c("name","count")

  PieData           <- data.frame(t(counts$count))
  colnames(PieData) <- counts$name
  return(PieData)
}

#' Makes a list of sensible colors based on column names of the supplied argument
#'
#' @param lineData A data frame with chart data where the column names are mostly colors
#' @return a list with of the column names of lineData where the names that are not colors are subsituted with a brewwed color
#'
getColors <- function(lineData){

  colors                                <- colnames(lineData)
  colors[tolower(colors) == 'amber' ]   <- 'orange'
  colors[tolower(colors) == 'unmapped'] <- 'gray'
  colors[tolower(colors) == 'other' ]   <- 'gray'
  indInvalid                            <- !(tolower(colors) %in% c('red','green','orange','yellow','black','gray','grey'))

  # brewer.pal returns at minimum 3 values, hence the indexing of the result
  colors[indInvalid] <- brewer.pal(max(3,sum(indInvalid)), 'Accent')[1:sum(indInvalid)]
  return(colors)
}

#' Creates aggregated counts per week of the different end results
#'
#' @param DD.Signaleringen data frame with alerts data
#' @param colorField string containing the column name to use for the end result.
#' This can be either be the score based arguement or the actual textual argument.
#' @param useJsTime boolean indicating wheater the time should be expressed as a numeric js value.
#' Else  the date will be formated according to "%Y %U %u"
#' @returns Dataframe with aggregated counts. Each row represents a week and each column an end result.
createChartCounts <- function(DD.Signaleringen,colorField,useJsTime=FALSE){

  x      <- data.frame(eindargumentatie = DD.Signaleringen[,colorField], Week = DD.Signaleringen$Week, Year = DD.Signaleringen$Year)
  q      <- ddply(x, c("Year","Week","eindargumentatie"), summarise, N = length(eindargumentatie))
  x      <- melt(q, measure.vars = c("N"))
  DD     <- dcast(x, formula = "Year +  Week ~ eindargumentatie")
  DD     <- DD[ DD$Week != 53,]
  if(useJsTime)
    Time <- 1000*as.numeric(as.POSIXlt(paste(DD$Year,DD$Week,"7"), format = "%Y %U %u"))
  else
    Time   <- as.Date(as.POSIXlt(paste(DD$Year,DD$Week,"7"), format = "%Y %U %u"))

  Counts <- DD[,-c(1:2)]
  Counts[is.na(Counts)] <- 0

  if(class(Counts)=='numeric'){
    Counts = as.data.frame(Counts)
    names(Counts) = names(DD)[-c(1:2)]
  }

  ### Make sure colors are ordered according to friss ranking
  isolate({
    ChartTimeData <- data.frame(Time = Time, Counts)
    cols <- colnames(ChartTimeData)
    cols <- cols[2:length(cols)]
    pos  <- frissColorSort(cols)
    ChartTimeData   <- ChartTimeData[,c(1,pos+1)]
  })


  return(ChartTimeData)
}