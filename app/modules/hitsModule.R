# hitsModule ui
hitsModuleUI <- function(id){

  ns <- NS(id)

  tagList(

    h3("hits"),

    fluidRow(

      column(3,
         div(id = "typeSelector",
         selectInput(inputId   = ns("TypeHit"),
                     label     = "type hit",
                     choices   = c("Argument","Indicator"),
                     selected  = "Argument",
                     multiple  = FALSE,
                     selectize = TRUE))
      ),

      column(3,
         uiOutput(ns("HitColumn"))
      ),

      column(3,
         div(id = "dispMode",
         selectInput(inputId   = ns("HitColumnDisplayMode"),
                     label     = "display mode",
                     choices   = c("absolute counts","percentages per row","percentages per column","overall percentages"),
                     selected  = "absolute counts",
                     multiple  = FALSE,
                     selectize = TRUE))
      )
    ),

    hr(),

    DT::dataTableOutput(ns("HitTable")),

    br()

  )

}

# hitsModule server
hitsModule <- function(input, output, session, moduleData,helpReactive,
                       helpData,nextPage){

  ns <- session$ns

  ### Make sure the proper help is loaded.
  source("modules/helpLogic.R",local=TRUE)
  
  TableData <- reactive({

    lstData <- moduleData() 
    
    if(is.null(input$TypeHit)) return()
    if(is.null(input$siHitColumn)) return()
    if(is.null(lstData$DD.LogIndicator)) return()
    if(is.null(input$HitColumnDisplayMode)) return()

    Select       <- ifelse(input$TypeHit == "Argument","argument","naam")
    var1         <- lstData$DD.LogIndicator[,Select]
    var2         <- lstData$DD.LogIndicator[,input$siHitColumn]
    tt           <- table(var1,var2)

    # display mode
    if(input$HitColumnDisplayMode == "percentages per row"){
      tt = round(100*prop.table(tt,1),3)
    }

    if(input$HitColumnDisplayMode == "percentages per column"){
      tt = round(100*prop.table(tt,2),3)
    }

    if(input$HitColumnDisplayMode == "overall percentages"){
      tt = round(100*prop.table(tt),3)
    }

    dd            <- data.frame(rownames(tt),as.data.frame.matrix(tt),check.names = FALSE)
    names(dd)[1]  <- input$TypeHit
    row.names(dd) <- NULL
    
    return(dd)

  })

  # hit column
  output$HitColumn <- renderUI({
    
    lstData <- moduleData() 
    
    if(is.null(lstData$DD.LogIndicator)) return()

    Choices <- list( Structure = c(line="branche"),
                     Result1   = c('end argument'="eindargumentatie"),
                     Time      = c("Year","Quarter","MonthName","WeekDay"))

    selectInput(ns("siHitColumn"), "group", choices = Choices, selected = "eindargumentatie", multiple = FALSE, selectize = TRUE, width = NULL)
  })

  # arguments/indicators vs group
  output$HitTable <- DT::renderDataTable({
    datatable(TableData(), options = list(rownames=FALSE , server = TRUE, options = list(pageLength = 10, scrollX = TRUE , lengthMenu = list(c(1, 3, 5, 10, 25, 50, 100, -1), c('1','3','5','10','25','50', '100', 'all')))))
  })

}
