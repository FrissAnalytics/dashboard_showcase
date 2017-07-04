library(FrissButtonGroup)

# module ui
filterModuleUI <- function(id){
  
  # set namespace via id
  ns <- NS(id)
  
  tagList(
    
    h3("filter"),
    
    fluidRow(
      column(9,
             helpText(textOutput(ns("RecordsInFilterRange")))
      )
    ),
    
    fluidRow(
      
      column(3
      ),
      
      column(3,
        actionButton(ns("apply"), "apply", style = "background-color:green;color:white"),
        actionButton(ns("reset"), "reset", style = "background-color: rgb(255, 127, 14); color:white")
      )
    
    ),
    
    br(),
    
    fluidRow(
      column(2, h4("Structure")),
      column(2, offset = 8, div( style = "float: right"))),
    
    hr(),
    
    fluidRow(
      column(3,uiOutput(ns("FilterProcess"))),
      column(3,uiOutput(ns("FilterLabel"))),
      column(3,uiOutput(ns("FilterProduct"))),
      column(3,uiOutput(ns("FilterBranch")))
    ),
    
    hr(),
    
    h4("Time"),
    
    fluidRow(
      column(3, uiOutput(ns("FilterYear"))),
      column(3, uiOutput(ns("FilterQuarter"))),
      column(3, uiOutput(ns("FilterMonth"))),
      column(3, uiOutput(ns("FilterDay")))
    ),
    
    br(),
    
    hr(),
    
    fluidRow(
      column(3, uiOutput(ns("FreeFormTime1")))
    ),
    
    hr(),
    
    h4("Score"),
    
    fluidRow(
      column(3, uiOutput(ns("ScoreFilter"))),
      column(3, uiOutput(ns("EindoordeelFilter")))
    ),
    
    br()

  )
}

# module server
filterModule <- function(input, output, session, moduleData,helpReactive,
                         helpData,nextPage){
  
  # get namespace based on session
  ns <- session$ns
  
  RV <- reactiveValues(saveFilter=0,reset=0,nRecords=NULL) # saveFilter is a trigger for the block that saves the current filter.
                                                           # reset is a trigger to clear the current selection
  
  ### Make sure the proper help is loaded.
  source("modules/helpLogic.R",local=TRUE)
  
  # moduleData is the reactive expression with all data provided form the main app.
  # This block extracts all relevant data
  observe({
  
    lstData <- moduleData()
    
    isolate({
      RV$items               <- lstData$items
      RV$selected.items      <- lstData$selected.items
      RV$implied.items       <- lstData$implied.items
      RV$indicators          <- lstData$indicators
      RV$indicators.selected <- lstData$indicators.selected
      RV$arguments           <- lstData$arguments
      RV$arguments.selected  <- lstData$arguments.selected
      RV$dataFilters         <- lstData$storedFilters$dataFilters
      RV$defaultFilter       <- lstData$storedFilters$defaultFilter
      RV$selectedFilter      <- lstData$storedFilters$defaultFilter
      RV$nRecords            <- lstData$nRecords
    })
  })
  
### Build ui components ----
  output$FilterProcess <- renderUI({
    
    items          <- RV$items$proces
    selectedItems  <- RV$selected.items$proces
    implied.items  <- RV$implied.items$proces
    
    buttonGroup(ns("proces"),items,selectedItems = selectedItems,label="proces",implied.items = implied.items)
  })

  output$FilterLabel <- renderUI({
    
    items          <- RV$items$label
    selectedItems  <- RV$selected.items$label
    implied.items  <- RV$implied.items$label
    
    buttonGroup(ns("label"),items,selectedItems = selectedItems,label="label",implied.items = implied.items)
  })
  
  output$FilterProduct <- renderUI({
    
    items          <- RV$items$product
    selectedItems  <- RV$selected.items$product
    implied.items  <- RV$implied.items$product
    
    buttonGroup(ns("product"),items,selectedItems = selectedItems,label="product",implied.items = implied.items)
  })
  
  output$FilterBranch <- renderUI({
    
    items          <- RV$items$branche
    selectedItems  <- RV$selected.items$branche
    implied.items  <- RV$implied.items$branche
    
    buttonGroup(ns("branche"),items,selectedItems = selectedItems,label="branche",implied.items = implied.items)
  })
  
  # time based filters
  output$FilterYear <- renderUI({
    items          <- RV$items$Year
    selectedItems  <- RV$selected.items$Year
    implied.items  <- RV$implied.items$Year
    buttonGroup(ns("fYear"),lstItems=items,selectedItems=selectedItems,label='year',implied.items = implied.items)
  })
  
  output$FilterQuarter <- renderUI({
    items          <- RV$items$Quarter
    selectedItems  <- RV$selected.items$Quarter
    implied.items  <- RV$implied.items$Quarter
    buttonGroup(ns("fQuarter"),lstItems=items,selectedItems=selectedItems,label='quarter',implied.items = implied.items)
  })
  
  output$FilterMonth <- renderUI({
    items          <- RV$items$MonthName
    selectedItems  <- RV$selected.items$MonthName
    implied.items  <- RV$implied.items$MonthName
    buttonGroup(ns("fMonth"),lstItems=items,selectedItems=selectedItems,label='month',implied.items = implied.items)
  })
  
  output$FilterDay <- renderUI({
    items          <- RV$items$WeekDay
    selectedItems  <- RV$selected.items$WeekDay
    implied.items  <- RV$implied.items$WeekDay
    buttonGroup(ns("fWeekDay"),lstItems=items,selectedItems=selectedItems,label='week day',implied.items = implied.items)
  })

  # free form time filter based on dateRangeInput
  output$FreeFormTime1 <- renderUI({
    PortfolioStart = RV$items$PortfolioStart
    PortfolioEnd   = RV$items$PortfolioEnd
    
    fluidRow(column(6,dateRangeInput(ns("DateRange"), "date range:",start = PortfolioStart,end = PortfolioEnd)),
             column(6,radioButtons(ns("DateRangeUsage"),label=NULL,choices=c("None"="None","Use lower bound"="Lower","User upper bound"="Upper","Use both"="Both"),selected="None")))
  })
  
  # score based filter
  output$ScoreFilter <- renderUI({
    Max <- RV$items$ScoreMax
    fluidRow(column(6,sliderInput(ns("siScoreFilter"), "friss score:", min = 0, max = Max, value = c(0,Max), step = 1)),
             column(6,radioButtons(ns("ScoreFilterUsage"),label=NULL,choices=c("None"="None","Use lower bound"="Lower","User upper bound"="Upper","Use both"="Both"),selected="None"))
    )
  })
  
  # judgement based filter
  output$EindoordeelFilter <- renderUI({
    items          <- RV$items$eindargumentatie
    selectedItems  <- RV$selected.items$eindargumentatie
    implied.items  <- RV$implied.items$eindargumentatie
    
    buttonGroup(ns("fEindargumentatie"),lstItems=items,selectedItems=selectedItems,label='end result',implied.items = implied.items)
  })
  
  # indicator
  output$Indicator <- renderUI({
    Choices  <- RV$indicators
    Selected <- RV$indicators.selected
    selectInput(ns("fIndicator"),"indicator:",selected = Selected, choices = Choices , width = "100%", multiple = TRUE)
  })
  
  # argument
  output$Argument <- renderUI({
    Choices  <- RV$arguments
    Selected <- RV$arguments.selected
    selectInput(ns("fArgument"),"argument:",selected = Selected, choices = Choices , width = "100%", multiple = TRUE)
  })
  
  # update selectize with available filters
  observe({
    isolate({selected=RV$selectedFilter})
    updateSelectInput(session,"filterName",choices=c("None",names(RV$dataFilters)),selected = selected)
  })
  
  ###
  ### Renders the action buttons besides the filter name selectize
  ### We make sure only currently saved filters can be loaded or removed
  ###
  output$filterButtons <- renderUI({
    
    filter.exists <- input$filterName %in% names(RV$dataFilters)
    
    ###
    ### We cannot simply add an attribute disabled that is TRUE or FALSE.
    ### A button will be disabled when it has an attribute called 'disabled' wheater its TRUE or FALSE.
    ###
    if(input$filterName=="None"){
      btnLoad    <- actionButton(ns("load")    , "load"          , style="success")
      btnSave    <- actionButton(ns("save")    , "save"          , style="success", disabled=TRUE)
      btnDelete  <- actionButton(ns("delete")  , "delete"        , style="success", disabled=TRUE)
      btnDefault <- actionButton(ns("default") , "set as default", style="success")
    }else if(!filter.exists) {
      btnLoad    <- actionButton(ns("load")   , "load"           , style="success", disabled=TRUE)
      btnSave    <- actionButton(ns("save")   , "save"           , style="success")
      btnDelete  <- actionButton(ns("delete") , "delete"         , style="success", disabled=TRUE)
      btnDefault <- actionButton(ns("default"), "set as default" , style="success", disabled=TRUE)
    }else{ 
      btnLoad    <- actionButton(ns("load")   , "load"           , style="success")
      btnSave    <- actionButton(ns("save")   , "save"           , style="success")
      btnDelete  <- actionButton(ns("delete") , "delete"         , style="success")
      btnDefault <- actionButton(ns("default"), "set as default" , style="success")
    } 
    
    list(btnLoad,btnSave,btnDelete,btnDefault)
  })
  
  ### Display which filter is the default filter, for now it is a simple div with text.
  output$defaultFilter <- renderUI({
    div(paste0("Current default filter: ",RV$defaultFilter))
  })
### End build ui components ----  

### Records in range ----
  # message on filter status
  output$RecordsInFilterRange <- renderText({
    
    nAlerts <- nrow(DD.Signaleringen)
    
    nRecords <- RV$nRecords
    if(is.null(nRecords) || nRecords == 1)
      nRecords <- nAlerts
    
    isolate({
      PortfolioStart = min(DD.Signaleringen$detectiedatum)
      PortfolioEnd   = max(DD.Signaleringen$detectiedatum)
      
      TextMessage <- paste0("Currently ", nRecords,
                            " (", round(100*nRecords/nAlerts,3),
                            "%) records are in filter range out of ",nAlerts,".",
                            " Portfolio start: ", PortfolioStart, ", portfolio end: ", PortfolioEnd, ",",
                            " current selection: ",input$DateRange[1] , " to ", input$DateRange[2], ".")
    })
    
    return(TextMessage)
  })
###
  
### Start reset controls ----    
  
  ###
  ### Update filter selections on reset
  ### We do this trought RV$reset because a reset can also be triggered by loading the special 'None' filter from the list
  ###
  observeEvent(input$reset,{
  
    RV$reset <- RV$reset + 1
  })   
  
  ###
  ### This block resets all filter controls
  ###
  observeEvent(RV$reset,{
    resetButtonGroup(session, 'proces')
    
    resetButtonGroup(session, 'label')
    
    resetButtonGroup(session, 'product')
    
    resetButtonGroup(session, 'branche')
    
    resetButtonGroup(session, 'fYear')
    
    resetButtonGroup(session, 'fQuarter')
    
    resetButtonGroup(session, 'fMonth')
    
    resetButtonGroup(session, 'fWeekDay')
    
    resetButtonGroup(session, 'fEindargumentatie')
    
    updateSelectizeInput(session,"fIndicator",selected=NA)
    
    updateSelectizeInput(session,"fArgument",selected=NA)
  })
### End reset controls ----    
    
### Start Deleting\Saving\Loading filter----
  
  ###
  ### This blocks loads a filter from the list with stored filters
  ### Important note: Loading a filter does not apply it, it only loads the selection in the filter module.
  ###
  observeEvent(input$load,{
    
    if(input$filterName=="None"){
      RV$reset <- RV$reset + 1
      RV$selected.items      <- RV$items
      RV$indicators.selected <- list()
      RV$arguments.selected  <- list()
    }else{
    
      currentFilter          <- RV$dataFilters[[input$filterName]]
      
      RV$selected.items      <- list(proces           = currentFilter$Signaleringen.Filter$proces$selected , 
                                     label            = currentFilter$Signaleringen.Filter$label$selected  ,
                                     product          = currentFilter$Signaleringen.Filter$product$selected,
                                     branche          = currentFilter$Signaleringen.Filter$branche$selected,
                                     Year             = currentFilter$Signaleringen.Filter$Year$selected,
                                     Quarter          = currentFilter$Signaleringen.Filter$Quarter$selected,
                                     MonthName        = currentFilter$Signaleringen.Filter$MonthName$selected, 
                                     WeekDay          = currentFilter$Signaleringen.Filter$WeekDay$selected,
                                     PortfolioStart   = currentFilter$Signaleringen.Filter$PortfolioStart,
                                     PortfolioEnd     = currentFilter$Signaleringen.Filter$PortfolioEnd,
                                     eindargumentatie = currentFilter$Signaleringen.Filter$eindargumentatie$selected)
  
      RV$indicators.selected <- currentFilter$IndicatorArgument.Filter$Indicator
      RV$arguments.selected  <- currentFilter$IndicatorArgument.Filter$Argument
      RV$implied.items       <- list()
    }
  })
  
  ###
  ### Saving the current filter
  ### RV$saveFilter is triggered when a new filter is saved or when a new filter is overwritten and this is explicitly approved by the user
  ###
  observeEvent(input$save,{
    
    ### Check if we are overwriting a filter, if so we display an appropriate message
    if(input$filterName %in% names(RV$dataFilters)){
      showConfirm(session,ns("confirmSave"),paste0("Do you want to overwrite the exsisting filter: ",input$filterName, " ? You cannot undo this action"))
    }else{
      RV$saveFilter = RV$saveFilter + 1
    }
    
  },priority = 10)
  
  ###
  ### This block is triggered when the user explicitly confirms an excisting filter should be overwritten. 
  ### If the user confirms we trigger RV$saveFilter to trigger the actual saving
  ### 
  observeEvent(input$confirmSave,{
    if(input$confirmSave==TRUE)
      RV$saveFilter = RV$saveFilter + 1
  })
  
  ###
  ### This block performs saving of the current filter
  ###
  observeEvent(RV$saveFilter,{
    
    ### Current selection
    currentFilter <- list(Signaleringen.Filter=RV$Signaleringen.Filter,IndicatorArgument.Filter=RV$IndicatorArgument.Filter) 
    
    if(RV$saveFilter>0){
      
      filterName <- input$filterName
      
      # Add filter to list with saved filters
      RV$dataFilters[[filterName]] <- currentFilter
      
      # We store the filters and the selected default (which can be 'None') in a list
      storedFilters <- list(dataFilters=RV$dataFilters,defaultFilter=RV$defaultFilter)
      
      # Check if any errors occur
      result = tryCatch({
        
        save(storedFilters,file=filterFile)
      
        storedFilters <<- storedFilters
        
        RV$selectedFilter <-filterName # Make sure we keep current selection
        
        showPopup(session,paste0("Filter <b>",filterName,"</b>succesfully saved"),title="Filter saved",okText="OK")
        
      }, warning = function(w) {
        war = gsub('\n','',as.character(w))
        showPopup(session,war,title="Warning")
        
      }, error = function(e) {
        err = gsub('\n','',as.character(e))
        showPopup(session,err,title="Error")
        
      }, finally = {
        
      })
    }
    
  })
  
  ###
  ### If we click the delete button we warn the user he is about to delete a filter and ask for confirmation
  ###
  observeEvent(input$delete,{
    
    if(!is.na(RV$defaultFilter) && input$filterName == RV$defaultFilter){
        showConfirm(session,ns("confirmDelete"),paste0("Are you sure you want to delete the filter <b>: ",input$filterName, "</b> ? Currently this is the default filter.You cannot undo this action"))      
    }else{
        showConfirm(session,ns("confirmDelete"),paste0("Are you sure you want to delete the filter <b>: ",input$filterName, "</b> ? You cannot undo this action"))
    }
  })
  
  ###
  ### If the user confirms the deletion we update the list with filters and store it to disk
  ###
  observeEvent(input$confirmDelete,{

    RV$dataFilters[[input$filterName]] <- NULL
    
    if(!is.na(RV$defaultFilter) && input$filterName == RV$defaultFilter){
      RV$defaultFilter <- "None"
    }
    
    storedFilters <- list(dataFilters=RV$dataFilters,defaultFilter=RV$defaultFilter)
    
    result = tryCatch({
      
      save(storedFilters,file=filterFile)
      
      storedFilters <<- storedFilters
      
    }, warning = function(w) {
      war = gsub('\n','',as.character(w))
      showPopup(session,war,title="Warning")
      
    }, error = function(e) {
      err = gsub('\n','',as.character(e))
      showPopup(session,err,title="Error")
      
    }, finally = {
      
    })
  })
  
  ###
  ### This block changes the default filter
  ### Chaching the default filter will imediatly store it
  ###
  observeEvent(input$default,{
    RV$defaultFilter <- input$filterName
    
    storedFilters <- list(dataFilters=RV$dataFilters,defaultFilter=RV$defaultFilter)
    
    result = tryCatch({
      
      save(storedFilters,file=filterFile)
    
      storedFilters <<- storedFilters
      
      showPopup(session,paste0("New default succesfully set to <b>",RV$defaultFilter,"</b"),title="Filter saved",okText="OK")
      
    }, warning = function(w) {
      war = gsub('\n','',as.character(w))
      showPopup(session,war,title="Warning")
      
    }, error = function(e) {
      err = gsub('\n','',as.character(e))
      showPopup(session,err,title="Error")
      
    }, finally = {
      
    })
  })
  
### End Deleting\Saving\Loading filter----
  
  ###
  ### This blocks builds the actual result that will be returned to the main app
  ### It contains a named list with:
  ###   Signaleringen.Filter    : A list with the complete current selection related to the DD.Signaleringen table.   
  ###   IndicatorArgument.Filter: A list with indicators and arguments that should be filtered out
  ###   storedFilters           : A list with the stored filters
  ###
  observe({
      RV$Signaleringen.Filter   <- list(proces=input$proces,label=input$label,product=input$product,branche=input$branche,
                                       Year=input$fYear,Quarter = input$fQuarter,MonthName = input$fMonth, WeekDay = input$fWeekDay,
                                       DateRange=input$DateRange,DateRangeUsage=input$DateRangeUsage,
                                       siScoreFilter=input$siScoreFilter,ScoreFilterUsage=input$ScoreFilterUsage,eindargumentatie=input$fEindargumentatie)
      
      RV$IndicatorArgument.Filter <- list(Indicator = input$fIndicator, Argument=input$fArgument)
      RV$storedFilters            <- list(dataFilters=RV$dataFilters,defaultFilter=RV$defaultFilter)
  },priority = 100)
  
  returnItems <- reactive({
    # This check makes sure the filter app has been fully loaded
    # This way we prevent an unnecessary trigger
    if(is.null(input$proces)) 
      return(NULL) 
    else 
      return(list(Signaleringen.Filter = RV$Signaleringen.Filter,selected.items=RV$selected.items,IndicatorArgument.Filter = RV$IndicatorArgument.Filter,storedFilters=RV$storedFilters))
    })
  
  return(returnItems)
}