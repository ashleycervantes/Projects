library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(tidyr)
library(readr)

# --- 1. FULL STADIUM DATA ---
stadium_df <- tibble::tibble(
  stadium = c(
    rep("Angel Stadium", 5), rep("Busch Stadium", 5), rep("Chase Stadium", 5),
    rep("Citi Stadium", 5), rep("Citizens Bank Park", 5), rep("Comerica Park", 5),
    rep("Coors Park", 5), rep("Dodger Stadium", 5), rep("Fenway Park", 5),
    rep("Globe Life Field", 5), rep("Great American Ball Park", 5), rep("Kauffman Stadium", 5),
    rep("Marlins Park", 5), rep("Target Field", 5), rep("Minute Maid Park", 5),
    rep("Nationals Park", 5), rep("Oriole Park at Camden Yards", 5), rep("Truist Park", 5),
    rep("Yankee Stadium", 5), rep("PNC Park", 5), rep("Rogers Centre", 5),
    rep("Petco Park", 5), rep("Guaranteed Rate Field", 5), rep("Progressive Field", 5),
    rep("Wrigley Field", 5), rep("American Family Field", 5), rep("Oracle Park", 5),
    rep("T-Mobile Park", 5)
  ),
  location = rep(c("LF", "LCF", "CF", "RCF", "RF"), times = 28),
  distance = c(
    330,389,396,365,330, 336,375,400,375,335, 328,412,407,414,335,
    335,370,408,380,330, 329,381,401,398,330, 345,370,420,365,330,
    347,420,415,424,350, 330,385,395,385,330, 310,390,420,380,302,
    329,372,407,274,326, 328,379,404,370,325, 330,385,410,385,330,
    344,386,407,392,335, 339,377,404,367,328, 315,362,409,373,326,
    336,377,402,370,335, 333,410,400,373,318, 335,385,400,375,325,
    318,399,408,385,314, 325,410,399,375,320, 328,375,404,375,328,
    336,386,396,391,322, 330,377,400,372,335, 325,410,405,375,325,
    355,368,400,368,353, 344,371,400,374,345, 339,364,399,421,309,
    331,390,405,387,327
  )
)
stadium_df_wide <- stadium_df %>% pivot_wider(names_from = location, values_from = distance)

# --- 2. DATA LOADING ---
load_pbp <- function() {
  files <- c("savant_data (14).csv", "savant_data (15).csv", "savant_data (16).csv")
  if(!all(file.exists(files))) return(NULL)
  lapply(files, read_csv) %>% bind_rows() %>%
    mutate(
      player_name_clean = ifelse(grepl(",", player_name),
                                 paste(trimws(sub(".*,", "", player_name)), trimws(sub(",.*", "", player_name))),
                                 player_name),
      game_year = year(game_date)
    ) %>% filter(!is.na(hc_x), !is.na(hc_y))
}

load_summary <- function() {
  if(!file.exists("batterstats22-24.csv")) return(NULL)
  read_csv("batterstats22-24.csv") %>%
    mutate(player_name_clean = sub("(.*), (.*)", "\\2 \\1", `last_name, first_name`)) %>%
    rename(game_year = year)
}

batter_data <- load_pbp()
summary_data <- load_summary()

# --- 3. UI ---
ui <- page_navbar(
  title = "MLB Statcast Dashboard",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  nav_panel("Spray Chart & Metrics",
            layout_sidebar(
              sidebar = sidebar(
                title = "Selection Menu",
                selectInput("batter", "Select Batter:", choices = NULL),
                selectInput("stadium", "Stadium Overlay:", choices = sort(unique(stadium_df_wide$stadium))),
                checkboxGroupInput("year", "Season(s):", choices = NULL)
              ),
              layout_columns(
                fill = FALSE,
                value_box(title = "xwOBA", value = textOutput("xwoba_text"), showcase = bs_icon("graph-up-arrow"), theme = "primary"),
                value_box(title = "Barrel %", value = textOutput("barrel_text"), showcase = bs_icon("fire"), theme = "danger"),
                value_box(title = "Hard Hit %", value = textOutput("hardhit_text"), showcase = bs_icon("lightning-fill"), theme = "warning"),
                value_box(title = "K %", value = textOutput("k_text"), showcase = bs_icon("X-circle"), theme = "secondary")
              ),
              navset_card_tab(
                nav_panel("Visual Spray Chart", plotlyOutput("spray_plot", height = "550px")),
                nav_panel("Hit Breakdown", tableOutput("event_table")),
                nav_panel("Full Season Stats", tableOutput("full_stats_table"))
              )
            )
  )
)

# --- 4. SERVER ---
server <- function(input, output, session) {
  
  # FIX: Only show batters who are in the summary_data (the leaderboard)
  observe({
    req(summary_data)
    updateSelectInput(session, "batter", choices = sort(unique(summary_data$player_name_clean)))
  })
  
  observeEvent(input$batter, {
    req(input$batter)
    yrs <- summary_data %>% filter(player_name_clean == input$batter) %>% distinct(game_year) %>% pull()
    updateCheckboxGroupInput(session, "year", choices = sort(yrs), selected = max(yrs))
  })
  
  filtered_pbp <- reactive({
    req(input$batter, input$year)
    batter_data %>% filter(player_name_clean == input$batter, game_year %in% input$year)
  })
  
  filtered_summary <- reactive({
    req(input$batter, input$year, summary_data)
    summary_data %>% 
      filter(player_name_clean == input$batter, game_year %in% input$year) %>%
      summarise(
        avg_xwoba = mean(xwoba, na.rm=T),
        avg_barrel = mean(barrel_batted_rate, na.rm=T),
        avg_hardhit = mean(hard_hit_percent, na.rm=T),
        avg_k = mean(k_percent, na.rm=T)
      )
  })
  
  output$xwoba_text <- renderText({ sprintf("%.3f", filtered_summary()$avg_xwoba) })
  output$barrel_text <- renderText({ paste0(round(filtered_summary()$avg_barrel, 1), "%") })
  output$hardhit_text <- renderText({ paste0(round(filtered_summary()$avg_hardhit, 1), "%") })
  output$k_text <- renderText({ paste0(round(filtered_summary()$avg_k, 1), "%") })
  
  output$spray_plot <- renderPlotly({
    df <- filtered_pbp()
    req(nrow(df) > 0)
    df <- df %>% mutate(sx = (hc_x - 125.42) * 2.5, sy = (198.27 - hc_y) * 2.5)
    row <- stadium_df_wide %>% filter(stadium == input$stadium)
    overlay <- tibble(
      x = c(0, -row$LF/1.41, -row$LCF/1.2, 0, row$RCF/1.2, row$RF/1.41, 0),
      y = c(0, row$LF/1.41, row$CF, row$CF, row$CF, row$RF/1.41, 0)
    )
    p <- ggplot(df, aes(x = sx, y = sy, color = events)) +
      geom_point(alpha = 0.6, size = 1.5) +
      geom_path(data = overlay, aes(x=x, y=y), inherit.aes = FALSE, color = "black", size = 0.8) +
      coord_fixed() + theme_minimal() + facet_wrap(~game_year) +
      theme(axis.title = element_blank(), axis.text = element_blank(), panel.grid = element_blank())
    ggplotly(p)
  })
  
  output$event_table <- renderTable({
    filtered_pbp() %>% count(events) %>% mutate(Pct = paste0(round(n/sum(n)*100, 1), "%"))
  })
  
  output$full_stats_table <- renderTable({
    req(summary_data)
    summary_data %>% filter(player_name_clean == input$batter, game_year %in% input$year) %>%
      select(game_year, pa, xwoba, barrel_batted_rate, hard_hit_percent, k_percent)
  })
}

shinyApp(ui, server)
