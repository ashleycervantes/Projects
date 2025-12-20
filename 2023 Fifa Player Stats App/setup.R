library(tidyverse)
# Load FIFA data
fifa = read_csv("C:/Users/Ashley Cervantes/Downloads/FIFA23_official_data.csv")

# Data Cleaning and Transformation
fifa = fifa %>%
  mutate(
    Name = gsub("^\\d+\\s*", "", Name),
    Club = gsub("^\\d+\\.\\s*", "", Club)
  ) %>%
  mutate(
    Height = as.numeric(gsub("[^0-9]", "", Height)),  
    Weight = as.numeric(gsub("[^0-9]", "", Weight))
  ) %>%
  mutate(
    Height_ft = floor(Height * 0.0328084),  
    Height_in = round((Height * 0.0328084 - Height_ft) * 12),  
    Height = paste0(Height_ft, "'", Height_in, "\"")
  ) %>%
  mutate(
    Weight = round(Weight * 2.20462, 1)
  ) %>%
  mutate(
    Position = gsub(".*>", "", Position)
  ) %>%
  select(Name, Age, Photo, Nationality, Flag, Overall, Club, `Club Logo`, 
         Position, Height, Weight, `Kit Number`) %>%
  rename(`Jersey Number` = `Kit Number`, 
         `Overall Rating` = Overall
  ) 

write.csv(x=fifa, file = "data/fifa.csv")



#basic idea for inputs
fifa %>%
  filter(Club == "Manchester United") %>%
  filter(Nationality == Germany)  %>%
  filter(Player == "K. Demirbay") 
  #want slider input of age

##basic output ideas 
#want to show name, age, height, 
#weight, club, nationality, 
#position, jersey number, overall rating on the right side

#want use of photos 
#player photo 
#nationality photo 
#club photo

