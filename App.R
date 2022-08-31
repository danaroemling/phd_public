# This needs three parts, UI, Server and a call to the app function

# ---- Preliminaries ----
# Load libraries
library(shiny) # app
library(stringr) # regex
library(dplyr) # regex
library(tidyverse) # mapping
library(rworldmap) # mapping

# Load data
corpus <- read.csv(file = 'total.csv')
longlat <- read.csv(file = 'all_cities.csv')

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
  
  # define function
  regcount <- function(corp, word1, case = FALSE, neg = FALSE){
    word1 <- paste('\\b', word1, '\\b', sep ="") 
    word1 <- regex(word1, ignore_case = !case)
    hits1 <- str_which(string = corp,  pat = word1,  negate = neg)   
    cities1<- corpus$location[hits1] 
    c1<-as.data.frame(table(cities1))
    c1[is.na(c1)] <- 0
    output <- c()
    output <- c1
    return(output)
  }
  
  
  output$value <- renderText({ input$my_text })
  
  observeEvent(input$button1, {
               reg_output <- regcount(corp = corpus$message, word1 = input$my_text, case = FALSE)
               colnames(reg_output) <- c("City", "Frequency")
               merger <- merge(reg_output, longlat, by.x ="City", by.y = "city")
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
                                 aes(x = lng, y = lat, size = Frequency),
                                 colour = "darkorchid4",
                                 alpha = 0.7)  +
                      guides(color = "none") +
                      labs(size="Frequency") +
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


