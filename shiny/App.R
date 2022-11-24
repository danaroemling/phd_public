# This needs three parts, UI, Server and a call to the app function

# ---- Preliminaries ----
# Load libraries
library(shiny) # app
library(stringr) # regex
library(dplyr) # regex
library(tidyverse) # mapping
library(rworldmap) # mapping

# Load data
corpus <- read.csv(file = 'full_matrix_for_filtering.csv')
longlat <- read.csv(file = 'gsa_geo_filtered.csv')
token_at_location <- read.csv(file = 'tokens_at_location.csv')
colnames(token_at_location) <- c("City", "Tokencount") 

# Get coordinates for GSA
worldMap <- getMap()
DACH <- c("Germany", "Austria", "Switzerland")
DACH_map <- which(worldMap$NAME%in%DACH)
DACH_coord <- lapply(DACH_map, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})
DACH_coord <- do.call("rbind", DACH_coord)


# ---- UI ----
# Define component one: UI
ui <- pageWithSidebar(titlePanel(
  h1("Mapping Features in the German-speaking Area", align = "center")),
  sidebarPanel(
    textInput("my_text", "What should we map?", placeholder = "Enter Feature Here"),
    verbatimTextOutput("value"),
    actionButton("button1", "Create Map"),
    actionButton("reset", "Reset")
    ),
  mainPanel(
    plotOutput("My_Plot"),
    tags$a(href = "https://aclanthology.org/D18-1469/", "Data Source: Hovy & Purschke (2018)", target = "_blank")
    )
)



# ---- Server ----
# Define component two: Server
server <- function(input, output) {
  output$value <- renderText({ input$my_text })
  
  # important to note that this solution is not the best way of doing this
  # ideally you would use reactive() instead of the observeEvent.
  observeEvent(input$button1, {
               one_word <- corpus %>% filter(word == "ich")
               colnames(one_word) <- c("Token", "City", "Frequency")
               merger_one <- merge(one_word, longlat, by ="City")
               merger <- merge(merger_one, token_at_location, by ="City")
               merger$relfreq <- (merger$Frequency/merger$Tokencount)
               merger$relfreq1000 <- (merger$relfreq*1000)
               colnames(merger) <- c("City", "Token", "Frequency", "lon", "lat", "TokenCount", "RelativeFrequency", "RelativeFrequencyThousand")
               
               output$My_Plot <- renderPlot({
                    ggplot() + geom_polygon(data = DACH_coord, 
                                            aes(x = long, y = lat, group = region),
                                            colour = "black", 
                                            size = 0.1, 
                                            fill = "gray95") + 
                      coord_map(xlim = c(4.5, 17),  
                                ylim = c(45.5, 55)) + 
                      theme_minimal() +  
                      geom_point(data = merger, 
                                 aes(x = lon, y = lat, size = RelativeFrequencyThousand),
                                 colour = "darkorchid4",
                                 alpha = 0.7)  +
                      guides(color = "none") +
                      labs(size="Relative\nFrequency") +
                      theme(axis.title.x = element_blank(), 
                            axis.title.y = element_blank(),
                            axis.text.x = element_blank(),
                            axis.text.y = element_blank(),
                            panel.grid.major = element_blank(),
                            plot.title = element_text(hjust = 0.5))
                  })
  })
  
  observeEvent(input$reset,{
    My_Plot <- NULL
    output$My_Plot <- renderPlot({
      My_Plot
    })
  })
}


# ---- Run App ----
# this is the third part / end bit
shinyApp(ui = ui, server = server)


