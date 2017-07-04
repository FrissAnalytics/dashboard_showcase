if(!exists("dynamicValues"))
  dynamicValues <- reactive({list()})


###
### Check if this module is the currently opened module and load the appropriate help
### This piece of code is equal for all modules with a dynamic help content
###
observeEvent({
  helpReactive()
  dynamicValues()
},{

  ### Get the id of the current module
  id <- session$ns("")
  id <- substr(id,1,nchar(id)-1)
  
  ### Get the current help which is propagated by the main app
  currentPage = helpReactive()
  
  if(is.null(currentPage))return()
  
  ### If this module is the currently active module and we have help data 
  ### load the help with the proper dynamic values
  if(currentPage==id){
    if(is.null(helpData)){
      #shinyjs::disable("btnHelp")
    }else{
      #shinyjs::enable("btnHelp")
      
      initIntroJS(session,helpData,dynamicValues(),nextPage=nextPage)
    }
  }
})