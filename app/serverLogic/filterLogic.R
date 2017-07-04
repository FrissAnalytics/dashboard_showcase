###
### This block provides the filter app with a named list with the folowing items:
###   items:          a named list with all unique values and ranges per field in DD.Signaleringen we want to filter on
###   selected.items: items that are selected in the current filter, this can be from a current selection so the filter app remains its current state
###                   or it can be set from an initial default filter
###   implied.items:  items that are implicitly filtered out by the current selection after applying it. 
###                   This list is on of the results of a call to the applyFilter function
###   indicators:     a list with all unique indicators
###   arguments:      a list with all unique arguments
###   dataFilters:    a list with all filters storted in filters.RData
###   defaultFilter:  the default filter also stored in filters.RData
###
buildFilter <- reactive({
  
  # We need to update the filter when the mapping changes
  RV$mapping
  
  # Do not initialze filter until initialization is complete
  if(RV$init) return()
  
  ### This block has a dependency on the result of applyFilter which is stored in  RV$filterResult
  ### When we applied the filter we know which items are implied and we use this information to color the buttons
  implied.items <- RV$filterResult$implied.items
  
  isolate({
    items           <- list(proces     = unique(MappedData$DD.Signaleringen$proces),
                            label      = unique(MappedData$DD.Signaleringen$label),
                            product    = unique(MappedData$DD.Signaleringen$product),
                            branche    = unique(MappedData$DD.Signaleringen$branche),
                            Year       = unique(MappedData$DD.Signaleringen$Year),
                            Quarter    = unique(MappedData$DD.Signaleringen$Quarter),
                            MonthName  = unique(MappedData$DD.Signaleringen$MonthName),
                            WeekDay    = unique(MappedData$DD.Signaleringen$WeekDay),
                            PortfolioStart = min(MappedData$DD.Signaleringen$detectiedatum),
                            PortfolioEnd   = max(MappedData$DD.Signaleringen$detectiedatum),
                            ScoreMax       = max(MappedData$DD.Signaleringen$eindresultaat),
                            eindargumentatie = unique(MappedData$DD.Signaleringen$eindargumentatie)
    )
    
    indicators      <- list()
    arguments       <- list()
    
    ###
    ### The first time this block is called we have no results from the filter app
    ### This means that all items will be selected (i.e. empty filter) and no items are implied
    ###
    if(is.null(RV$signaleringen.Filter)){
      selected.items      <- items
      implied.items       <- list()
      indicators.selected <- list()
      arguments.selected  <- list()
    }else{
      
      ###
      ### Subsequent times this block is excecuted the filter app is already running
      ### This means we have a filter state and we can retrieve which items are selected
      ###
      
      selected.items  <- list(proces           = RV$signaleringen.Filter$proces$selected , 
                              label            = RV$signaleringen.Filter$label$selected  ,
                              product          = RV$signaleringen.Filter$product$selected,
                              branche          = RV$signaleringen.Filter$branche$selected,
                              Year             = RV$signaleringen.Filter$Year$selected,
                              Quarter          = RV$signaleringen.Filter$Quarter$selected,
                              MonthName        = RV$signaleringen.Filter$MonthName$selected, 
                              WeekDay          = RV$signaleringen.Filter$WeekDay$selected,
                              PortfolioStart   = RV$signaleringen.Filter$PortfolioStart,
                              PortfolioEnd     = RV$signaleringen.Filter$PortfolioEnd,
                              eindargumentatie = RV$signaleringen.Filter$eindargumentatie$selected)
      
      indicators.selected <- RV$IndicatorArgument.Filter$Indicator
      arguments.selected  <- RV$IndicatorArgument.Filter$Argument
    }
    
  })
  
  if(!is.null(RV$filterResult$Filter.signaleringen))
    nRecords = sum(RV$filterResult$Filter.signaleringen)
  else
    nRecords = NULL
  
  filterModuleData <- list(items = items,selected.items = selected.items,implied.items = implied.items,indicators=indicators,arguments=arguments,storedFilters=RV$storedFilters,indicators.selected=indicators.selected,arguments.selected=arguments.selected,nRecords=nRecords)

  return(filterModuleData)
})

ModuleResults$filterSelection <- callModule(module = filterModule, id = "Filter", buildFilter, HelpReactive,helpData[["Filter"]],nextPage=NA)

###
### Set apply filter flag to true when button is clicked
### The filter flag can also be set to true when an initial filter is loaded
###
applyFilter      <- observeEvent(input$"Filter-apply",{
  RV$applyFilter <- TRUE
})

###
### Get filter result from module
###
getFilterFromModule <- observeEvent({ModuleResults$filterSelection()},{
  
  filterResult <- ModuleResults$filterSelection()
  
  if(is.null(filterResult))
    return()
  
  ### The filter result contains all data from the buttongroups
  ### Each button group contains a list with 3 items: selected items, notselected items, and implicit items
  ### For filtering purposes only the notselected items are used: We want to explicitly filter out items that are not selected
  ### all other items are left in
  RV$signaleringen.Filter     <- ModuleResults$filterSelection()$Signaleringen.Filter
  ### The IndicatorArgument.Filter result contains two lists with indicators and arguments that should be filtered out
  RV$IndicatorArgument.Filter <- ModuleResults$filterSelection()$IndicatorArgument.Filter
  ### Keep stored filters up to date
  RV$storedFilters            <- ModuleResults$filterSelection()$storedFilters
})

###
### Load default filter at startup if it is available and trigger an apply by setting RV$applyFilter to TRUE
###
getDefaultFilter <- observeEvent(RV$init,{
  
  if(RV$init){
  
    defaultFilter <- RV$storedFilters[['defaultFilter']]
    
    if(!is.null(defaultFilter) && defaultFilter != 'None'){
      RV$signaleringen.Filter     <- RV$storedFilters$dataFilters[[defaultFilter]]$Signaleringen.Filter
      RV$IndicatorArgument.Filter <- RV$storedFilters$dataFilters[[defaultFilter]]$IndicatorArgument.Filter
      RV$applyFilter              <- TRUE # This causes the apply filter block to be executed
    }
    RV$init <- FALSE
  }
  
})

###
### Excecute filter when RV$applyFilter is set to TRUE
### RV$applyFilter becomes true when an intial default filter is loaded or when the apply button from the filter module is clicked
###
observeEvent({RV$applyFilter},{
  
  if(RV$applyFilter == TRUE && !is.null(RV$signaleringen.Filter)){

    # Applyfilter will apply the signaleringen filter to both the signaleringen and logindicator tables
    # It will return a named list with: TRUE/FALSE vectors for both the signaleringen and logindicator tables
    #                                   list with items that are implicitly filtered out
    RV$filterResult <- applyFilter(MappedData$DD.Signaleringen,MappedData$DD.LogIndicator,RV$signaleringen.Filter,RV$IndicatorArgument.Filter)
    
    isolate({
    # Filter reactive data
      
      validFilter <- sum(RV$filterResult$Filter.signaleringen)!=0
      
      if(validFilter)
        NotieAlert("Filter applied")
      
      # If the filter results in an empty selection do not execute it and display an appropriate message.
      if(!validFilter){
        showPopup(session,"The selection you made is not valid because it contains no data. Change the filter and try again.","Error") 
      }else{
        # The actual filtering is performed in the ModuleDataReactive reactive. In this block we filter the mapped data once the filter
        # has been updated. This block only performs the filtering if the filter does not result in an empty filter
      }
    })
  }
  RV$applyFilter <- FALSE
},priority=10) # Make the priority lower then the block which executes the mapping

#' Applys alerts and indocator filters to alerts and indicator data
#' 
#' @param DD.Signaleringen data frame with dump from signaleringen table
#' @param DD.Logindicator data frame with dump from the logindicator table
#' @param filter named list of lists with filter to be applied
#' @param IndicatorArgument.Filter list with indicators and arguments that should be filtered out of the data
#' @returns a named list with: Filter.signaleringen  : a TRUE/FALSE vector of lenght nrow(DD.Signaleringen) which indicates which records of DD.Signaleringen will be kept in
#'                             Filter.logindicator   : a TRUE/FALSE vector of lenght nrow(DD.Logindicator) which indicates which records of DD.LogIndicator should be kept in
#'                             impl.proces           : list with procceses that will be implicitly filtered out
#'                             impl.label            : list with labels that will be implicitly filtered out
#'                             impl.product          : list with products that will be implicitly filtered out
#'                             impl.branche          : list with branches that will be implicitly filtered out
#'                             impl.Year             : list with Years that will be implicitly filtered out
#'                             impl.Quarter          : list with Quarters that will be implicitly filtered out
#'                             impl.MonthName        : list with Months that will be implicitly filtered out
#'                             impl.WeekDay          : list with WeekDays that will be implicitly filtered out
#'                             impl.eindargumentatie : list with eindargumentaties that will be implicitly filtered out
                             
applyFilter <- function(DD.Signaleringen, DD.LogIndicator, Signaleringen.Filter, IndicatorArgument.Filter){
  
  SignaleringenfilterResult     <- applySignaleringenFilter(DD.Signaleringen,DD.LogIndicator,Signaleringen.Filter)
  indicatorArgumentFilterResult <- applyIndicatorArgumentFilter(DD.Signaleringen,DD.LogIndicator,IndicatorArgument.Filter)
  
  Filter.signaleringen          <- SignaleringenfilterResult$Filter.signaleringen & indicatorArgumentFilterResult$Filter.signaleringen
  Filter.logindicator           <- SignaleringenfilterResult$Filter.logindicator  & indicatorArgumentFilterResult$Filter.logindicator
  
  return(list(Filter.signaleringen=Filter.signaleringen,Filter.logindicator=Filter.logindicator,implied.items=SignaleringenfilterResult$implied.items))
}

#' Applys alerts filter to alerts and indicator data
#'
#' @param DD.Signaleringen data frame with dump from signaleringen table
#' @param Signaleringen.Filter named list of lists with filter to be applied
#' @returns a named list with: Filter.signaleringen  : a TRUE/FALSE vector of lenght nrow(DD.Signaleringen) which indicates which records of DD.Signaleringen will be kept in
#'                             Filter.logindicator   : a TRUE/FALSE vector of lenght nrow(DD.Logindicator) which indicates which records of DD.LogIndicator should be kept in
#'                             impl.proces           : list with procceses that will be implicitly filtered out
#'                             impl.label            : list with labels that will be implicitly filtered out
#'                             impl.product          : list with products that will be implicitly filtered out
#'                             impl.branche          : list with branches that will be implicitly filtered out
#'                             impl.Year             : list with Years that will be implicitly filtered out
#'                             impl.Quarter          : list with Quarters that will be implicitly filtered out
#'                             impl.MonthName        : list with Months that will be implicitly filtered out
#'                             impl.WeekDay          : list with WeekDays that will be implicitly filtered out
#'                             impl.eindargumentatie : list with eindargumentaties that will be implicitly filtered out
applySignaleringenFilter <- function(DD.Signaleringen,DD.LogIndicator,Signaleringen.Filter){
  
  ### Build individual TRUE/FALSE vectors
  F1     <- !is.element(DD.Signaleringen$proces,Signaleringen.Filter$proces$notSelected)
  F2     <- !is.element(DD.Signaleringen$label,Signaleringen.Filter$label$notSelected)
  F3     <- !is.element(DD.Signaleringen$product,Signaleringen.Filter$product$notSelected)
  F4     <- !is.element(DD.Signaleringen$branche,Signaleringen.Filter$branche$notSelected)
  
  F5     <- !is.element(DD.Signaleringen$Year,Signaleringen.Filter$Year$notSelected)
  F6     <- !is.element(DD.Signaleringen$Quarter,Signaleringen.Filter$Quarter$notSelected)
  F7     <- !is.element(DD.Signaleringen$MonthName,Signaleringen.Filter$MonthName$notSelected)
  F8     <- !is.element(DD.Signaleringen$WeekDay,Signaleringen.Filter$WeekDay$notSelected)
  
  if(Signaleringen.Filter$DateRangeUsage=="Both"){
    F9    <- DD.Signaleringen$detectiedatum >= as.Date(ymd(Signaleringen.Filter$DateRange[1])) &
      DD.Signaleringen$detectiedatum <= as.Date(ymd(Signaleringen.Filter$DateRange[2]))
  }else if(Signaleringen.Filter$DateRangeUsage=="Lower"){
    F9    <- DD.Signaleringen$detectiedatum >= as.Date(ymd(Signaleringen.Filter$DateRange[1])) 
  }else if(Signaleringen.Filter$DateRangeUsage=="Upper"){
    F9    <-  DD.Signaleringen$detectiedatum <= as.Date(ymd(Signaleringen.Filter$DateRange[2])) 
  }else{
    F9    <- TRUE
  }
  
  if(Signaleringen.Filter$ScoreFilterUsage=="Both"){
    F10    <- as.numeric(DD.Signaleringen$eindresultaat) >= Signaleringen.Filter$siScoreFilter[1] &
      as.numeric(DD.Signaleringen$eindresultaat) <= Signaleringen.Filter$siScoreFilter[2]
  }else if(Signaleringen.Filter$ScoreFilterUsage=="Lower"){
    F10   <- as.numeric(DD.Signaleringen$eindresultaat) >= Signaleringen.Filter$siScoreFilter[1]
  }else if(Signaleringen.Filter$ScoreFilterUsage=="Upper"){
    F10   <- as.numeric(DD.Signaleringen$eindresultaat) <= Signaleringen.Filter$siScoreFilter[2]
  }else{
    F10 <- TRUE
  }
  
  
#   F10 <- switch(Signaleringen.Filter$ScoreFilterUsage,
#            Both  = as.numeric(DD.Signaleringen$eindresultaat) >= Signaleringen.Filter$siScoreFilter[1] & 
#                    as.numeric(DD.Signaleringen$eindresultaat) <= Signaleringen.Filter$siScoreFilter[2],
#            Lower = as.numeric(DD.Signaleringen$eindresultaat) >= Signaleringen.Filter$siScoreFilter[1],
#            Upper = as.numeric(DD.Signaleringen$eindresultaat) <= Signaleringen.Filter$siScoreFilter[2],
#            Default = TRUE
#          )
#   

  F11    <- !is.element(DD.Signaleringen$eindargumentatie,Signaleringen.Filter$eindargumentatie$notSelected)
  
  # Get TRUE/FALSE vector for signaleringen
  Filter.signaleringen <- F1 & F2 & F3 & F4 & F5 & F6 & F7 & F8 & F9 & F10 & F11
  
  # Get TRUE/FALSE vector for logindicator
  alertIDs.in                 <- DD.Signaleringen$signaleringid[Filter.signaleringen]
  Filter.logindicator         <- DD.LogIndicator$signaleringid %in% alertIDs.in
  
  # Get all items that are implicitly filtered out
  impl.proces            <- setdiff(unique(DD.Signaleringen$proces [F1])           ,unique(DD.Signaleringen$proces           [     F2 & F3 & F4 & F5 & F6 & F7 & F8 & F9 & F10 & F11]))
  impl.label             <- setdiff(unique(DD.Signaleringen$label  [F2])           ,unique(DD.Signaleringen$label            [F1      & F3 & F4 & F5 & F6 & F7 & F8 & F9 & F10 & F11]))
  impl.product           <- setdiff(unique(DD.Signaleringen$product[F3])           ,unique(DD.Signaleringen$product          [F1 & F2      & F4 & F5 & F6 & F7 & F8 & F9 & F10 & F11]))
  impl.branche           <- setdiff(unique(DD.Signaleringen$branche[F4])           ,unique(DD.Signaleringen$branche          [F1 & F2 & F3      & F5 & F6 & F7 & F8 & F9 & F10 & F11]))
  impl.Year              <- setdiff(unique(DD.Signaleringen$Year   [F5])           ,unique(DD.Signaleringen$Year             [F1 & F2 & F3 & F4      & F6 & F7 & F8 & F9 & F10 & F11]))
  impl.Quarter           <- setdiff(unique(DD.Signaleringen$Quarter[F6])           ,unique(DD.Signaleringen$Quarter          [F1 & F2 & F3 & F4 & F5      & F7 & F8 & F9 & F10 & F11]))
  impl.MonthName         <- setdiff(unique(DD.Signaleringen$MonthName[F7])         ,unique(DD.Signaleringen$MonthName        [F1 & F2 & F3 & F4 & F5 & F6      & F8 & F9 & F10 & F11]))
  impl.WeekDay           <- setdiff(unique(DD.Signaleringen$WeekDay[F8])           ,unique(DD.Signaleringen$WeekDay          [F1 & F2 & F3 & F4 & F5 & F6 & F7 &      F9 & F10 & F11]))
  impl.eindargumentatie  <- setdiff(unique(DD.Signaleringen$eindargumentatie[F11]) ,unique(DD.Signaleringen$eindargumentatie [F1 & F2 & F3 & F4 & F5 & F6 & F7 & F8 & F9 & F10 ]))
  
  
  # Put everything in lists and return
  implied.items <- list(proces=impl.proces,label=impl.label,product=impl.product,branche=impl.branche,
                        Year=impl.Year,Quarter=impl.Quarter,MonthName=impl.MonthName,WeekDay=impl.WeekDay,eindargumentatie=impl.eindargumentatie)
  
  return(list(Filter.signaleringen = Filter.signaleringen,
              Filter.logindicator  = Filter.logindicator,
              implied.items        = implied.items      ))
}

#' Applys indicator and argument filter to alerts and indicator data
#' 
#' @param DD.Signaleringen data frame with dump from signaleringen table
#' @param DD.Logindicator data frame with dump from the logindicator table
#' @param IndicatorArgument.Filter list with indicators and arguments that should be filtered out of the data
#' @returns 
applyIndicatorArgumentFilter <- function(DD.Signaleringen,DD.LogIndicator,IndicatorArgument.Filter){
  
  # exclude indicator and arguments specified in filter
  if(!is.null(IndicatorArgument.Filter$Indicator)){
    m1 <- is.element(DD.LogIndicator$naam,IndicatorArgument.Filter$Indicator)
  } else{
    m1 <- FALSE
  }
  
  if(!is.null(IndicatorArgument.Filter$Argument)){
    m2 <- is.element(DD.LogIndicator$argument,IndicatorArgument.Filter$Argument)
  } else{
    m2 <- FALSE
  } 
  
  # TRUE/FALSE vector for logindicator table
  Filter.logindicator <- !m1 & !m2
  
  # ids that pass filter
  signaleringidRemove  <- DD.LogIndicator$signaleringid[m1 | m2]
  
  # TRUE /FALSE vector for signaleringen table
  Filter.signaleringen <- !DD.Signaleringen$signaleringid %in% signaleringidRemove
         
  return(list(Filter.signaleringen=Filter.signaleringen,Filter.logindicator=Filter.logindicator))
}