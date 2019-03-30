library(shiny)
library(shinythemes)
library(shinydashboard)
library(tidyverse)
library(forcats)
library(reshape2)
library(readr)

library(plotly)
library(leaflet)
library(dygraphs)
library(xts)
library(sp)

library(datasets)
library(wordcloud)
library(maps)
library(dplyr)
library(tidytext)
library(ggplot2)


shinyUI(dashboardPage(skin = "purple", 
                      dashboardHeader(title = "412 Food Rescue", 
                                      titleWidth = 200),
                      dashboardSidebar(
                        sidebarMenu(
                          menuItem("Frequency of Donations", tabName = "part1", icon = icon("part1")),
                          menuItem("Rescues vs. Distance", tabName = "part2", icon = icon("part2")),
                          menuItem("Distribution of Distance", tabName = "part3", icon = icon("part3")),
                          menuItem("Distribution of Poverty Rate", tabName = "part4", icon = icon("part4")),
                          menuItem("Distribution of Median Income", tabName = "part5", icon = icon("part5")),
                          menuItem("Distribution of Food Access", tabName = "part6", icon = icon("part6")),
                          menuItem("Rescues vs. Involved Recipients", tabName = "part7", icon = icon("part7"))
                        )
                      ),
                      dashboardBody(
                        # Boxes need to be put in a row (or column)
                        tabItems(
                          # First tab content
                          tabItem(tabName = "part1", 
                                  fluidPage(
                                    fluidRow(
                                      plotlyOutput(outputId = "donation_per_recipient", height = "500px"),
                                      checkboxInput(inputId = "recurring",
                                                    label = "Show only weekly",
                                                    value = FALSE),
                                      checkboxInput(inputId = "ad_hoc",
                                                    label = "Show only one-time",
                                                    value = FALSE),
                                      br(),
                                      br(),
                                      h4("Statistical Summary"),
                                      br(),
                                      DT::dataTableOutput(outputId = "frequency")
                                    )
                                  )
                                  
                          ),
                          tabItem(tabName = "part2", 
                                  plotlyOutput(outputId = "rescues_vs_distance", height = "500px")
                          ),
                          tabItem(tabName = "part3", 
                                  fluidPage(
                                    fluidRow(
                                      plotlyOutput(outputId = "distribution_of_distance", height = "500px"),
                                      br(),
                                      br(),
                                      h4("Statistical Summary"),
                                      br(),
                                      DT::dataTableOutput(outputId = "distance_summary")
                                    )
                                  )
                          ),
                          tabItem(tabName = "part4", 
                                  fluidPage(
                                    fluidRow(
                                      plotlyOutput(outputId = "distribution_of_poverty_rate", height = "500px"),
                                      br(),
                                      br(),
                                      h4("Statistical Summary"),
                                      br(),
                                      DT::dataTableOutput(outputId = "poverty_summary")
                                    )
                                  )
                          ),
                          tabItem(tabName = "part5", 
                                  fluidPage(
                                    fluidRow(
                                      plotlyOutput(outputId = "distribution_of_median_income", height = "500px"),
                                      br(),
                                      br(),
                                      h4("Statistical Summary"),
                                      br(),
                                      DT::dataTableOutput(outputId = "income_summary")
                                    )
                                  )
                          ),
                          tabItem(tabName = "part6", 
                                  fluidPage(
                                    fluidRow(
                                      plotlyOutput(outputId = "distribution_of_food_access", height = "500px"),
                                      br(),
                                      br(),
                                      h4("Statistical Summary"),
                                      br(),
                                      DT::dataTableOutput(outputId = "food_access_summary")
                                    )
                                  )
                          ),
                          tabItem(tabName = "part7", 
                                  plotlyOutput(outputId = "involved_recipients", height = "500px"),
                                  checkboxInput(inputId = "recurring_2",
                                                label = "Show only weekly",
                                                value = FALSE),
                                  checkboxInput(inputId = "ad_hoc_2",
                                                label = "Show only one-time",
                                                value = FALSE)
                          )
                        )
                      )
)
)