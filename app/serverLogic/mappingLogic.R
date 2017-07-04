###
### Whenever the mapping changes the Mapped data object is updated
### Subsequently the ModuleDataReactive will be triggered and all modules using the this reactive will be updated
###
observeEvent({RV$mapping},
             {
                MappedData$DD.Signaleringen      <- DD.Signaleringen
                MappedData$DD.LogIndicator       <- DD.LogIndicator
                
             }
             ,priority=100) # set high priority such that mapping is always performed before filtering


#' helper function to perform data result mapping
#' mapping: data frame containing the mapping
#' Data: Data to map, this can be eiter DD.Signaleringen or DD.Logindicator
#' MaxNrOfUniqueEntries: Maximum alowed maximum values. This is to prevent fields with many unique values from being loaded in the app
PerformMapping <- function(mapping,Data, MaxNrOfUniqueEntries = 100){
  
  ###
  Columns <- c("label","proces","branche","product")
  nCols   <- length(Columns)
  
  for(i in 1:nCols){
    
    SelectedColumn    <- Columns[i]
    NrOfUniqueEntries <- length(unique(Data[,SelectedColumn]))
    
    if( NrOfUniqueEntries > MaxNrOfUniqueEntries){
      Data[,SelectedColumn] <- SelectedColumn
    }
  }
  
  ### Perform mapping
  nMappings  <- nrow(mapping)
  
  uFields    <- unique(mapping$field)
  fields_org <- paste0(uFields,"_org")
  
  Data[,fields_org] <- Data[,uFields]
  
  # Apply mapping to currently loaded data
  nVars   <- length(unique(uFields))
  
  for(i in 1:nVars){
    
    selectedVar <- as.character(uFields[i])
    
    posMap      <- match(Data[,selectedVar],mapping$from)
    posMap      <- posMap[!is.na(posMap)]
    indData     <- Data[,selectedVar] %in% mapping$from[posMap]
    
    posTo      <- match(Data[,selectedVar],mapping$to)
    posTo      <- posTo[!is.na(posTo)]
    indAlreadyMapped <- Data[,selectedVar] %in% mapping$from[posTo]
    
    # Only set endresult fields to unmapped if no mappings is available
    if(selectedVar=='eindargumentatie')
      Data[!indAlreadyMapped,selectedVar]          <- 'UNMAPPED'
    
    Data[indData,selectedVar]   <- as.character(mapping$to[posMap])
    
  }
  
  return(Data)
}

# Wrap data to provide to the mapping module in a reactive
# This reactive provides the mapping module with the current mapping and an export of the mapping based on the data
# The data can contain more values then are present in the mapping so the current mapping and mapping export can differ
mappingReactive <- reactive({
  
  isolate({mapping.export <- createMappingExport(MappedData$DD.Signaleringen,RV$mapping)})
  
  return(list(mapping=RV$mapping,mapping.export=mapping.export))})

### 
### Helper function to create downloadable mapping
###
createMappingExport <- function(DD.Signaleringen,mapping,MaxNrOfUniqueEntries=100){
  
  ### Remove columns with many unique values
  Columns <- c("label","proces","branche","product")
  nCols   <- length(Columns)
  
  for(i in 1:nCols){
    
    SelectedColumn    <- Columns[i]
    NrOfUniqueEntries <- length(unique(DD.Signaleringen[,SelectedColumn]))
    
    if( NrOfUniqueEntries > MaxNrOfUniqueEntries){
      DD.Signaleringen[,SelectedColumn] <- SelectedColumn
    }
  }
  
  mappingFields <- unique(c("label","proces","branche","product","eindargumentatie",unique(as.character(mapping$field))))
  
  mapping.export <- data.frame(field=character(),from=character(),to=character())
  
  nMappingFields <- length(mappingFields)
  
  for(i in 1:nMappingFields){
    
    orgField <- paste0(mappingFields[i],"_org")
    
    if(!orgField %in% names(DD.Signaleringen))
      orgField <- mappingFields[i]
    
    tbl <- unique(DD.Signaleringen[,c(orgField,mappingFields[i])])
    
    tbl <- cbind(mappingFields[i],tbl)
    names(tbl) <- c("field","from","to")
    
    mapping.export <- rbind(mapping.export,tbl)
  }
  
  return(mapping.export)
  
}
