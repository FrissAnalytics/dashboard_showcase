rm(list=ls())

# public R libraries
library(shiny)
library(googleVis)
library(plyr)
library(dplyr)
library(reshape2)
library(DT)
library(RColorBrewer)

# custom R libraries
library(FrissC3Charts)
library(FrissMessageBox)
library(FrissIntroJS)
library(FrissSwitch)
library(FrissNotie)

# module definitions
source("modules/filterModule.R")
source("modules/piesModule.R")
source("modules/frontPanelModule.R")
source("modules/flowChartModule.R")
source("modules/hitsModule.R")

# helper routines
source("helpers.r")

# temp header
FrissHeader <- list(

  tags$a(href = "http://www.friss.eu/en", tags$img(src="friss_small2.svg", id = "FrissLogo")),

  singleton(includeScript("www/underscore.js")),
  singleton(includeScript("www/jquery-ui.js")),
  singleton(includeScript("www/shinyState.js")),
  singleton(includeScript("www/d3.js")),
  singleton(includeCSS("www/friss.css")),
  singleton(includeCSS("www/app.css")),

  # when shiny is busy the following panels will create an overlay which will hide the main content until shiny is done computing
  div(id = "busy"),
  div(id = "BusyDIV"),

  # add help
  addIntroJS(useVoice=FALSE),

  # add notie type messages
  addNotieToPage()
)

load("dashboard_data.RData")

# init
dataFilters   <-  list()
mapping       <-  list()

# init help
source("initHelp.R")
