#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(shinydashboard)
suiciderates <- read.csv("master.csv")
countries <- suiciderates %>%
  distinct(country)

# Define UI for application that draws a histogram
library(shinydashboard)
ui <- dashboardPage(dashboardHeader(
  title = "Suicides Rates Insights",
  titleWidth = 350
),
                    dashboardSidebar(
                      menuItemOutput(outputId = "Country")
                      #selectInput("Country",
                      #            "Select the Country",
                      #            choices = countries)
                    ),
                    dashboardBody(
                     tabsetPanel(id = "tabs",
                                  tabPanel(
                                    "Global Suicide Rates",
                                    fluidRow(plotOutput("plot10")),
                                    br(),
                                    fluidRow(column(6,plotOutput("plot11")),
                                             column(6,plotOutput("plot12"))),
                                    br(),
                                    fluidRow(column(6,plotOutput("plot13")),
                                             column(6,plotOutput("plot14")))),                                  
                      tabPanel(
                        "Suicide Per Country",
                        value = 'suicideSummary',
                        fluidRow(plotOutput("plot1")),
                        br(),
                        fluidRow(column(6, plotOutput("plot2")),
                                 column(6, plotOutput("plot3"))),
                        br(),
                        fluidRow(column(6, plotOutput("plot4")),
                                 column(6, plotOutput("plot5")))
                      ),
                      tabPanel("Global Suicide Analysis",
                               fluidRow(column(6,plotOutput("plot6")),
                                        column(6,plotOutput("plot7"))),
                               br(),
                               fluidRow(column(6,plotOutput("plot8")),
                                        column(6,plotOutput("plot9"))))
                      
                      
                    )))
