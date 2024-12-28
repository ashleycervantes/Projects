library(readr)
library(tidyverse)
library(shiny)
library(leaflet)

mural=read_csv("C:/Users/Ashley Cervantes/Downloads/Mural_Registry_20241126.csv", locale = locale(encoding = "UTF-8"))    ###change pathway of csv file based on your windows/apple device
mural_images=read_csv("C:/Users/Ashley Cervantes/Downloads/mural images.csv", locale = locale(encoding = "UTF-8"))        ###change pathway of csv file based on your windows/apple device

murals_description=mural %>%
  filter(!is.na(`Description of Artwork`))

murals_media = murals_description %>%
  mutate(
    Media = str_to_title(trimws(Media)), 
    Media = case_when(
      str_detect(Media, "Spray Paint|Spraypaint|Spray") ~ "Spray Paint",
      str_detect(Media, "Mosaic|Mosaic / Tile|Mosaic Tiles") ~ "Mosaic",
      str_detect(Media, "Paint And Mixed Media|Paint And Spray Paint") ~ "Mixed Media",
      str_detect(Media, "Acrylic And Exterior Latex Paint|Acrylic Paint") ~ "Acrylic Paint",
      str_detect(Media, "Paint|Painting") ~ "Paint",
      TRUE ~ Media  
    )
  ) %>%
  mutate(
    `Description of Artwork` = gsub("[^\\p{L}\\p{N}\\p{Zs}]", "", `Description of Artwork`, perl = TRUE),
    `Artist Credit` = gsub("[^\\p{L}\\p{N}\\p{Zs}]", "", `Artist Credit`, perl = TRUE),
    `Artist Credit` = gsub("ï½", "", `Artist Credit`),
    `Description of Artwork` = gsub("ï½", "", `Description of Artwork`)
  ) %>%
  filter(!is.na(Media))


merged_df = merge(murals_media, mural_images, by = "Mural Registration ID")


library(shiny)
library(leaflet)
library(dplyr)

ui = fluidPage(
  titlePanel("Chicago Mural Map"),
  p("Explore the vibrant Chicago mural map! Use the dropdowns below to filter murals by media type, ward, or year the murals were installed. 
    Wards refers to a specific geographical area or district within a city that is represented by one or more members of a city council."),
  
  fluidRow(
    column(
      width = 4,  
      selectInput(
        "mediaFilter", 
        "Filter by Media Type:", 
        choices = c("All", sort(unique(na.omit(merged_df$Media)))),  
        selected = "All"
      )
    ),
    column(
      width = 4,  
      selectInput(
        "wardFilter", 
        "Filter by Ward:", 
        choices = c("All", sort(as.numeric(unique(merged_df$Ward)))),  
        selected = "All"
      )
    ),
    column(
      width = 4,
      sliderInput(
        "yearFilter",
        "Filter by Year Installed:",
        min = min(merged_df$`Year Installed`, na.rm = TRUE),
        max = max(merged_df$`Year Installed`, na.rm = TRUE),
        value = c(min(merged_df$`Year Installed`, na.rm = TRUE), max(merged_df$`Year Installed`, na.rm = TRUE)),
        step = 1
      )
    )
  ),
  
  fluidRow(
    leafletOutput("muralMap", height = "800px")
  )
)

server = function(input, output, session) {
  output$muralMap = renderLeaflet({
    req(input$mediaFilter, input$wardFilter, input$yearFilter) 
    
    filtered_data = merged_df
    
    if (input$mediaFilter != "All") {
      filtered_data = filtered_data[filtered_data$Media == input$mediaFilter, ]
    }
    if (input$wardFilter != "All") {
      filtered_data = filtered_data[filtered_data$Ward == as.numeric(input$wardFilter), ]
    }
    if (length(input$yearFilter) == 2) {
      filtered_data = filtered_data[filtered_data$`Year Installed` >= input$yearFilter[1] &
                                      filtered_data$`Year Installed` <= input$yearFilter[2], ]
    }
    
    filtered_data = filtered_data %>%
      filter(
        !is.na(Longitude), !is.na(Latitude),
        Longitude >= -87.9401, Longitude <= -87.5237,
        Latitude >= 41.6445, Latitude <= 42.0230
      )
   
    if (nrow(filtered_data) == 0) {
      showNotification("No murals found for the selected filters.", type = "warning")
      return(
        leaflet() %>%
          addTiles() %>%
          setView(lng = -87.7, lat = 41.8, zoom = 10) 
      )
    }

    bounds <- list(
      lng_min = min(filtered_data$Longitude, na.rm = TRUE),
      lng_max = max(filtered_data$Longitude, na.rm = TRUE),
      lat_min = min(filtered_data$Latitude, na.rm = TRUE),
      lat_max = max(filtered_data$Latitude, na.rm = TRUE)
    )

    if (nrow(filtered_data) == 1) {
      leaflet(filtered_data) %>%
        addTiles() %>%
        setView(
          lng = filtered_data$Longitude[1],
          lat = filtered_data$Latitude[1],
          zoom = 14
        ) %>%
        addCircleMarkers(
          ~Longitude, ~Latitude,
          popup = ~paste0(
            ifelse(
              !is.na(`Image Link`) & `Image Link` != "",
              paste0("<b>Image:</b><br><img src='", `Image Link`, "' style='width:200px;height:auto;'><br>"),
              "<b>Image:</b> Not Accessible<br>"
            ),
            "<b>Artwork Title:</b> ", `Artwork Title`, "<br>",
            "<b>Artist:</b> ", `Artist Credit`, "<br>",
            "<b>Year Installed:</b> ", `Year Installed`, "<br>",
            "<b>Ward:</b> ", Ward, "<br>",
            "<b>Description:</b> ", `Description of Artwork`
          ),
          radius = 10,
          color = "blue",
          fill = TRUE,
          fillOpacity = 0.8
        )
    } else {
      media_colors = colorFactor(
        palette = "Set1",
        domain = merged_df$Media
      )
      
      leaflet(filtered_data) %>%
        addTiles() %>%
        fitBounds(
          lng1 = bounds$lng_min, lng2 = bounds$lng_max,
          lat1 = bounds$lat_min, lat2 = bounds$lat_max
        ) %>%
        addCircleMarkers(
          ~Longitude, ~Latitude,
          popup = ~paste0(
            ifelse(
              !is.na(`Image Link`) & `Image Link` != "",
              paste0("<b>Image:</b><br><img src='", `Image Link`, "' style='width:200px;height:auto;'><br>"),
              "<b>Image:</b> Not Accessible<br>"
            ),
            "<b>Artwork Title:</b> ", `Artwork Title`, "<br>",
            "<b>Artist:</b> ", `Artist Credit`, "<br>",
            "<b>Year Installed:</b> ", `Year Installed`, "<br>",
            "<b>Ward:</b> ", Ward, "<br>",
            "<b>Description:</b> ", `Description of Artwork`
          ),
          radius = 10,
          color = ~media_colors(Media),
          fill = TRUE,
          fillOpacity = 0.8
        ) %>%
        addLegend(
          position = "bottomright",
          pal = media_colors,
          values = ~Media,
          title = "Media Type",
          opacity = 1
        )
    }
  })
}

shinyApp(ui = ui, server = server)



