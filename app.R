# Load packages and data
library(shiny)
library(bslib)
library(surveyjoin)
library(sdmTMB)
library(fishyplots)
library(ggplot2)
library(dplyr)
data("predictions_afsc", package = "fishyplots")
data("predictions_nwfsc", package = "fishyplots")
data("predictions_pbs", package = "fishyplots")
data("vb_predictions", package = "fishyplots")
data("nwfsc_bio", package = "fishyplots")
data("afsc_bio", package = "fishyplots")
data("pbs_bio", package = "fishyplots")
nwfsc_bio <- nwfsc_bio |> select(-otosag_id)
all_data <- rbind(afsc_bio, nwfsc_bio, pbs_bio)

# Define UI ----

ui <- page_sidebar(
  title = "fishyplots",
  sidebar_width = 2,
  sidebar = sidebar(
    helpText("Plots from NOAA survey data."),
    
    selectInput(
      "species",
      label = "Choose a species",
      choices = c("None selected" = "", unique(all_data$common_name)),
      selected = "",
      selectize = TRUE
    )
  ),
  plotOutput("vbPlot"),
  plotOutput("lengthfreqPlot")
)

# Define server logic ----
server <- function(input, output, session) {
  
  output$modelPlot1 <- renderPlot({
    req(input$species != c("None selected", ""))
    fishmap(predictions_afsc, input$species)
  })
  
  output$modelPlot2 <- renderPlot({
    req(input$species != "None selected")
    fishmap(predictions_pbs, input$species)
  })
  
  output$modelPlot3 <- renderPlot({
    req(input$species != "None selected")
    fishmap(predictions_nwfsc, input$species)
  })
  
  # VB growth plots
  output$vbPlot <- renderPlot({
    req(input$species != "None selected")
    plot_growth(all_data, vb_predictions, c("AFSC", "PBS", "NWFSC"), input$species)
  })
  
  # Length frequency plots
  output$lengthfreqPlot <- renderPlot({
    req(input$species != "None selected")
    data <- all_data |> filter(common_name == input$species)
    length_frequency(data, c("AFSC", "PBS", "NWFSC"), time_series = TRUE)
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
