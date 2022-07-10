#load the requisite packages -----------------
library(shiny)
library(ggplot2)
library(DT)
library(dplyr)

#load the data ---------------------------
load('counties.RData')
attach(counties)

#define the ui -----------------------------

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "x",
        label = "X-axis:",
        choices = c('County' = 'name')
      ),
      
      #for y axis
      selectInput(
        inputId = "y",
        label = "Y-axis:",
        choices = c('Male', 'Female', 'Intersex', 'Total')
      ),
      
      #for coloring the plot
      selectInput(
        inputId = "z",
        label = "Color by:",
        choices = c('County' = 'name', 'Male', 'Female', 'Intersex', 'Total')
      ),
      
      #for making a reactive plot and table
      selectInput(
        inputId = "q",
        label = "The county names",
        choices = c("Mombasa", "Kwale", "Kilifi", "Tana River", "Lamu",
                    "Taita-Taveta", "Garissa", "Wajir", "Mandera", "Marsabit",
                    "Isiolo", "Meru", "Tharaka-Nithi", "Embu", "Kitui",
                    "Machakos", "Makueni", "Nyandarua", "Nyeri", "Kirinyaga",
                    "Murang'a", "Kiambu", "Turkana", "West Pokot", "Samburu",
                    "Trans Nzoia", "Uasin Gishu", "Elgeyo-Marakwet", "Nandi", "Baringo",
                    "Laikipia", "Nakuru", "Narok", "Kajiado", "Kericho",
                    "Bomet", "Kakamega", "Vihiga", "Bungoma", "Busia",
                    "Siaya", "Kisumu", "Homa Bay", "Migori", "Kisii",
                    "Nyamira", "Nairobi" ),
        selected = "Mombasa",
        multiple = T
      ),
      
      #the download button for downloading selected county data
      downloadButton(
        outputId = "m",
        label = "Download selected counties"
      )
    ),
    
    #where the plots and tables will be drawn
    mainPanel(
      plotOutput(outputId = "county_plot"),
      DTOutput(outputId = "county_table")
    )
  )
)

#define the server ------------------

server <- function(input, output, session){
  
  selected <- reactive({
    req(input$q)
    counties %>%
      filter(name %in% input$q)
  })
  
  #plot the plots
  output$county_plot <- renderPlot(ggplot(selected(), aes_string(x = input$x, y = input$y, fill = input$z)) + 
                                    geom_bar(stat = 'identity') + 
                                     theme(axis.text.x = element_text(size = 10, angle = 60, hjust = 1, vjust = 1))) 
  
  #plot the data table
  output$county_table <- renderDT(selected(), options = list(pageLength = 10), rownames = F)
  
  #the download options
  
  output$m <- downloadHandler(
    filename = function(){
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file){
      write.csv(selected(), file)
    }
  )
}

#create the shiny app 
shinyApp(ui, server)






























