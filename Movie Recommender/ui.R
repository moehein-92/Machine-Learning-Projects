## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)
library(tidyr)
library(tidyverse)

source('functions/helpers.R')


# Genre list
all_genre = c("Action", "Adventure", "Animation", 
               "Children's", "Comedy", "Crime",
               "Documentary", "Drama", "Fantasy",
               "Film-Noir", "Horror", "Musical", 
               "Mystery", "Romance", "Sci-Fi", 
               "Thriller", "War", "Western")


shinyUI(
    dashboardPage(
          skin = "blue",
          dashboardHeader(title = "Movie Recommender"),
          
          dashboardSidebar(collapsed=FALSE, disable=FALSE,
            sidebarMenu(
              menuItem("By Genre", tabName = "dashboard", icon = icon("dashboard")),
              menuItem("By Preference", icon = icon("th"), tabName = "widgets",
                       badgeLabel = "new", badgeColor = "green")
            )
          ),
          
          dashboardBody(includeCSS("css/books.css"),
            tabItems(
              tabItem(tabName = "dashboard",
                      h2("Get Recommendation by Genre"),
                      
                      #start
                      fluidRow(
                        box(width = 12, title = "Recommendation by Genre", status = "info", solidHeader = TRUE, collapsible = FALSE,
                            selectInput("selected_genre", 'Select your favorite genre from the dropdown Menu', all_genre),
                            br(),
                            withBusyIndicatorUI(
                                  actionButton("genre_btn", "Get Recommendations", class = "btn-warning")
                            ),
                            br(),
                            tableOutput("rec_genre_results"),
                            br()
                        )
                      )
                      
                      #close
                      
                      
              ),
              
              tabItem(tabName = "widgets",
                      h2("Get Recommendation Based on your Rating"),
                      #open
                      
                      fluidRow(
                        box(width = 12, title = "Step 1: Rate as many movies as possible", status = "info", solidHeader = TRUE, collapsible = FALSE,
                            div(class = "rateitems",
                                uiOutput('ratings')
                            )
                        )
                      ),
                      fluidRow(
                        useShinyjs(),
                        box(
                          width = 12, status = "info", solidHeader = TRUE,
                          title = "Step 2: Discover movies you might like",
                          br(),
                          withBusyIndicatorUI(
                            actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                          ),
                          br(),
                          tableOutput("results")
                        )
                      )
                  
                      #close
              )
            )
          )
      
          
    )
) 

