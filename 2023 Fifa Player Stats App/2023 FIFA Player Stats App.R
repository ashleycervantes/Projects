library(shiny)
library(tidyverse)
library(shinythemes)

source("R/expand_positions.R")

fifa = read_csv("data/fifa.csv") %>%
  expand_positions()


# UI
ui = fluidPage(
  theme = shinythemes::shinytheme("flatly"),  
  tags$head(
    tags$style(HTML("
      .player-card { 
        border: 1px solid #ddd; 
        border-radius: 10px; 
        box-shadow: 2px 2px 10px #aaa; 
        padding: 15px; 
        margin: 10px;
      }
      .player-photo img {
        border-radius: 10px;
        width: 100%;               /* Prevent image stretching */
        max-width: 100%;           /* Fit within the container */
        max-height: 600px;         /* Limit height */
        object-fit: contain;       /* Maintain aspect ratio */
        image-rendering: optimizeQuality; /* Preserve quality */
      }
      .inline-flag {
        height: 20px;
        width: auto;
        margin-right: 5px;
      }
    "))
  ),
  navbarPage(
    "2023 FIFA Player Stats App",
    tabPanel("Player Stats",
             sidebarLayout(
               sidebarPanel(
                 selectInput("club", "Club:", choices = c("All", sort(unique(fifa$Club)))),
                 uiOutput("nationalityInput"),
                 uiOutput("playerInput"),
                 checkboxInput("filterPlayer", "Filter Table To Player", FALSE) 
               ),
               mainPanel(
                 h3("Player Details"),
                 fluidRow(
                   column(4,
                          div(class = "player-card",
                              div(class = "player-photo", uiOutput("playerPhoto"))
                          )
                   ),
                   column(8,
                          div(class = "player-card",
                              h3(textOutput("playerName"), style = "color: #d35400; font-weight: bold;"),
                              p(tags$b("Age:"), textOutput("playerAge", inline = TRUE)),
                              p(tags$b("Height:"), textOutput("playerHeight", inline = TRUE)),
                              p(tags$b("Weight:"), textOutput("playerWeight", inline = TRUE)),
                              p(tags$b("Club:"), uiOutput("playerClubUI")),
                              p(tags$b("Nationality:"), uiOutput("playerNationalityUI")),
                              p(tags$b("Position:"), textOutput("playerPosition", inline = TRUE)),
                              p(tags$b("Jersey Number:"), textOutput("playerJersey", inline = TRUE)),
                              p(tags$b("Overall Rating:"), textOutput("playerOverall", inline = TRUE))
                          )
                   )
                 )
               )
             )),
    tabPanel("Table",
             fluidRow(
               column(12, dataTableOutput("filteredTable"))
             )),
    tabPanel("About", fluidRow(column(12, includeMarkdown("about.Rmd"))))
  )
)

# Server
server = function(input, output, session) {
  
  # Filter by Club
  filteredClub = reactive({
    if (input$club == "All") fifa else fifa %>% filter(Club == input$club)
  })
  
  # Filter by Nationality
  output$nationalityInput = renderUI({
    req(filteredClub())
    nationalities = sort(unique(filteredClub()$Nationality))
    selectInput("nationality", "Nationality:", choices = c("All", nationalities))
  })
  
  filteredNationality = reactive({
    if (input$nationality == "All") filteredClub() else filteredClub() %>% filter(Nationality == input$nationality)
  })
  
  output$playerInput = renderUI({
    req(filteredNationality())
    players <- sort(unique(filteredNationality()$Name))
    selectInput("player", "Player:", choices = players)
  })
  
  selectedPlayer = reactive({
    req(input$player)
    filteredNationality() %>% filter(Name == input$player)
  })
  
  # Player Photos
  output$playerPhoto = renderUI({
    req(selectedPlayer())
    photo = selectedPlayer()$Photo
    div(style = "text-align: center;", 
        tags$img(src = photo, 
                 alt = "Player Photo Unavailable", 
                 style = "width: 100%; max-width: 500px; height: auto;"))
  })
  
  # Nationality with Flag
  output$playerNationalityUI = renderUI({
    req(selectedPlayer())
    flag = selectedPlayer()$Flag
    nationality = selectedPlayer()$Nationality
    tags$span(tags$img(src = flag, class = "inline-flag"), nationality)
  })
  
  # Club with Flag
  output$playerClubUI = renderUI({
    req(selectedPlayer())
    club_logo = selectedPlayer()$`Club Logo`
    club = selectedPlayer()$Club
    tags$span(tags$img(src = club_logo, class = "inline-flag"), club)
  })
  
  # Text Outputs
  output$playerName = renderText({ selectedPlayer()$Name })
  output$playerAge = renderText({ selectedPlayer()$Age })
  output$playerHeight = renderText({ selectedPlayer()$Height })
  output$playerWeight = renderText({ selectedPlayer()$Weight })
  output$playerPosition = renderText({ selectedPlayer()$Position })
  output$playerJersey = renderText({ selectedPlayer() %>% pull(`Jersey Number`) })
  output$playerOverall = renderText({ selectedPlayer() %>% pull(`Overall Rating`) })
  
  # Filtered Table
  output$filteredTable = renderDataTable({
    if (input$filterPlayer) {
      req(selectedPlayer())
      selectedPlayer()
    } else {
      filteredNationality()
    }
  })
}

shinyApp(ui, server)

