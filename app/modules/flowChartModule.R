# module ui
flowChartModuleUI <- function(id){

  # set namespace via id
  ns <- NS(id)

  tagList(

    h3("flow chart"),

    br(),

    h3("data overview"),

    fluidRow(
      column(3,
             div(id = ns("Selector1"),
                 selectizeInput(inputId  = ns("SankeyStreams"),"input streams",
                                choices  = c("proces","label","product","branche"),
                                selected = c("branche","product","proces"),
                                multiple = TRUE,
                                options  = list(plugins = list('remove_button','drag_drop')), width = "100%")
             )
      ),

      column(3,
             numericInput(ns("minHits"),"min number of hits", min = 1, value = 100,step = 1)
      )
    ),

    br(),

    htmlOutput(ns("SankeyChart1"))

  )

}

# module server
flowChartModule <- function(input, output, session, moduleData,helpReactive,
                            helpData,nextPage){

  # get namespace based on session
  ns <- session$ns

  # helper function to get Sankey chart counts
  GetSankeyCounts <- function(l1,l2){
    x   <- data.frame(From = as.character(l1),
                      To   = as.character(l2))
    dat <- plyr::count(x)
    colnames(dat) <- c("From","To","Weight")
    return(dat)
  }

  ### Make sure the proper help is loaded.
  source("modules/helpLogic.R",local=TRUE)


  #### Sankey data---
  chartData <- reactive({

    lstData <- moduleData()
    
    if(is.null(input$SankeyStreams)) return()
    if(is.null(input$minHits)) return()

    Streams     <- input$SankeyStreams
    NrOfStreams <- length(Streams)

    # we need at least 2 streams
    if(NrOfStreams < 2) return()

    # create stream pairs
    StreamPairs     <- data.frame(Stream1 = Streams[-NrOfStreams],Stream2 = Streams[-1],stringsAsFactors = FALSE)
    NrOfStreamPairs <- nrow(StreamPairs)

    # for each pair compute counts, store pairs in data
    data <- NULL


    for(i in 1:NrOfStreamPairs){

      # vector 1
      name1  <- as.character(StreamPairs$Stream1[i])
      v1     <- as.character(lstData$DD.Signaleringen[,name1])

      # add prefix to prevent cycles e.g. when product and label have an overlap in names
      m      <- match(name1,c("proces","label","product","branche"))
      prefix <- c("proc","lab","prod","branche")[m]
      v1     <- paste(prefix,v1,sep="_")

      # vector 2
      name2  <- as.character(StreamPairs$Stream2[i])
      v2     <- as.character(lstData$DD.Signaleringen[,name2])

      m      <- match(name2,c("proces","label","product","branche"))
      prefix <- c("proc","lab","prod","branche")[m]
      v2     <- paste(prefix,v2,sep="_")

      Counts <- GetSankeyCounts(v1,v2)

      # update data
      data   <- rbind(data,Counts)
    }

    # just in case, remove duplicates
    data  <- unique(data)

    # remove small streams, at least 1 hit
    Min <- ifelse(input$minHits < 0, 1, input$minHits)

    data  <- data[data$Weight > Min,]

    return(data)

  })

  #### ----
  output$SankeyChart1 <- renderGvis({
    
    Data  <- chartData()
    
    Chart <- gvisSankey(Data, from = "From", to = "To", weight = "Weight",
                        options = list(width = "98%", height = "400px"),chartid = "1" )

    # remove google messages
    Chart$html$header  <- NULL
    Chart$html$footer  <- NULL
    Chart$html$caption <- NULL

    # return chart
    return(Chart)

  })

  # indicator selection
  output$Indicators <- renderUI({

    lstData <- moduleData()
    
    if(is.null(lstData$DD.LogIndicator)) return()

    x <- unique(lstData$DD.LogIndicator$naam)

    selectizeInput(ns("mIndicators"),"indicators",choices = x, multiple = TRUE, selected=x[1],
                   options = list(plugins = list('remove_button')), width = "98%")

  })


  #### Sankey data 2 ---
  chartData2 <- reactive({

    lstData <- moduleData()
    
    if(is.null(input$SankeyStreams)) return()
    if(is.null(input$minHits)) return()
    if(is.null(input$mIndicators)) return()

    # take subset of LogIndicator based on selected indicators
    m    <- is.element(lstData$DD.LogIndicator$naam,input$mIndicators)

    v1   <- lstData$DD.LogIndicator$naam[m]
    v1   <- paste("ind",v1,sep="_")

    v2   <- lstData$DD.LogIndicator$argument[m]
    v2   <- paste("arg",v2,sep="_")

    data <- GetSankeyCounts(v1,v2)

    # link to structure
    v3   <- lstData$DD.LogIndicator[m,input$SankeyStreams3]

    # add prefix to prevent cycles e.g. when product and label have an overlap in names
    m      <- match(input$SankeyStreams3,c("proces","label","product","branche"))
    prefix <- c("proc","lab","prod","branche")[m]
    v3     <- paste(prefix,v3,sep="_")

    # get counts
    data2 <- GetSankeyCounts(v2,v3)

    # combine
    data  <- rbind(data,data2)

    return(data)
  })


  #### ----
  output$SankeyChart2 <- renderGvis({

    Data2 <- chartData2()
    
    if(is.null(Data2)) return()

    Chart <- gvisSankey(Data2, from = "From", to = "To", weight = "Weight",
                        options = list(width = "98%", height = "800px"),chartid = "2" )

    # remove google messages
    Chart$html$header  <- NULL
    Chart$html$footer  <- NULL
    Chart$html$caption <- NULL

    # return chart
    return(Chart)

  })
}