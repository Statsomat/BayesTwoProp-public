MAX_FILE_SIZE_MB <- 15
options(shiny.maxRequestSize = MAX_FILE_SIZE_MB*1024^2)
options(shiny.sanitize.errors = TRUE)
# options(shiny.reactlog = TRUE) # enables reactive log
# shiny::reactiveConsole(TRUE)


library(shiny)
library(rmarkdown)
library(data.table)
library(readr)
library(shinydisconnect)
library(shinyMatrix)
library(LearnBayes)

source("helpers/chooser.R")
source("helpers/Functions.R")
