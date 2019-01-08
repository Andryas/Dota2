library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyalert)
library(shinydashboard)
library(shinydashboardPlus)
library(mongolite)
library(DT)

# ------------------------------------------------------------------------------
# Mongo
# ------------------------------------------------------------------------------
m <- mongo("match", "dota")
m2 <- mongo("player", "dota")

# m2$info()$stats$count
# m2$aggregate('[{ "$project": {"_id": "$_id", "total": {"$size": "$details"}} }]')
# ls()
