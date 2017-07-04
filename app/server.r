shinyServer(function(input, output, session){

  ### This reactive lets the module know which page is currently active
  ### It is used to load the proper help data with the current dynamic values
  HelpReactive <- reactive({
    input$main
  })
  
  ###
  ### Modules
  ###
  
   # GlobalDataOverview
   callModule(frontPanelModule,"Dashboard",ModuleDataReactive,HelpReactive,helpData[["Dashboard"]],nextPage=NA)
  
   # FlowCharts
   callModule(module = flowChartModule   , id = "FlowCharts"      , ModuleDataReactive,HelpReactive,helpData[["FlowCharts"]],nextPage=NA)

   # HitsModule
   callModule(module = hitsModule, id = "HitsModule"      , ModuleDataReactive,HelpReactive,helpData[["HitsModule"]],nextPage=NA)

  ###
  ### filter and help system logic
  ###
  
  ModuleResults <- reactiveValues(filterResult = reactive({}))  # filterSelection is the reactive expression returned from the filter module
  # and contains the current selected groups
  
  RV <- reactiveValues(signaleringen.Filter = NULL,          # signaleringen.Filter contains the various groups that should be filtered out.
                       # It is extracted from the filterResult reactive.
                       storedFilters        = dataFilters, # contains list with saved filters. If a filter with saved filters (filters.RData)
                       # is present its contents will be put here. If not dataFilters is initialized with 
                       # an empty list in global.r
                       applyFilter          = FALSE,         # applyFilter indicates wheater the current filter should be applied
                       # applyFilter can become true when apply filter is clicked from the filter module OR
                       # when an initial default filter is loaded at startup.
                       mapping              = mapping,       # contains either a saved mapping if one is present or the default mapping
                       mapData              = TRUE,          # triggers mapping of the data
                       init                 = TRUE,          # Flag indicating we are starting the app. This flag will be set to false after the 
                       # initial filter is loaded
                       dynamicValues        = list()
  )
  
  # Mapped data contains the mapped unfiltered data.
  # We use this data to provide the filter app with all fields it can filter on
  MappedData <- reactiveValues(DD.Signaleringen = DD.Signaleringen)
  
  # This reactive is what is actualy passed on to the modules. When the filter is updated this reactive is triggered and all modules will be updated.
  # This reactive filters mappedData
  ModuleDataReactive <- reactive({
    
    # Make sure filter is not empty
    validFilter <- sum(RV$filterResult$Filter.signaleringen)>0
    
    if(validFilter){
      Filtered.DD.Signaleringen <- MappedData$DD.Signaleringen[RV$filterResult$Filter.signaleringen,]
      Filtered.DD.LogIndicator  <- MappedData$DD.LogIndicator [RV$filterResult$Filter.logindicator, ]
    }else{
      Filtered.DD.Signaleringen <- MappedData$DD.Signaleringen
      Filtered.DD.LogIndicator  <- MappedData$DD.LogIndicator 
    }
    
    return(list(DD.Signaleringen=Filtered.DD.Signaleringen,DD.LogIndicator=Filtered.DD.LogIndicator))
  })
  
  source("serverLogic/mappingLogic.R",local = TRUE)
  source("serverLogic/filterLogic.R", local = TRUE)
  
  ###
  ### This block makes sure help is automatically started if we click to the next page from the help
  ###
  observeEvent({input$autoStartHelp},{
    if(input$autoStartHelp==1){
      session$sendCustomMessage("startHelp",NA)
    }
  })
  
  
  
})


