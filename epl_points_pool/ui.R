#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

ui <- basicPage(
    h2("EPL Table"),
    DT::dataTableOutput("epl_table"),
    
    h2("Points Pool Table"),
    DT::dataTableOutput("owner_table")
)
