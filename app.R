# Load packages
library(shiny)
library(bslib)
library(surveyjoin)
library(sdmTMB)
library(fishyplots)
library(ggplot2)
library(dplyr)
library(patchwork)

# Load data
data(nwfsc_bio)
data(afsc_bio)
data(pbs_bio)
data(predictions_afsc)
data(predictions_nwfsc)
data(predictions_pbs)
nwfsc_bio <- nwfsc_bio |> select(-otosag_id)
all_data <- rbind(afsc_bio, nwfsc_bio, pbs_bio)

# Define User Interface
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
  tabsetPanel(
    tabPanel("Biomass"), # can add plot outputs here!
    tabPanel("Age and length",
             plotOutput("agelengthPlot", height = "1000px")),
    tabPanel("Maps",
             plotOutput("modelPlot", height = "1200px")),
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Map plots
  output$modelPlot <- renderPlot({
    req(input$species != "None selected")
    p1 <- fishmap(predictions_afsc, input$species)
    p2 <- fishmap(predictions_pbs, input$species)
    p3 <- fishmap(predictions_nwfsc, input$species)
    p1 + p2 + p3 + plot_layout(ncol = 1)
  })
  
  # Length Frequency and Growth plots -- add length-weight here!
  output$agelengthPlot <- renderPlot({
    req(input$species != c("None selected", ""))
    # Growth plot
    p1 <- plot_growth(all_data, vb_predictions, c("AK BSAI", "AK GULF", "PBS", "NWFSC"), input$species) 
    
    # Length frequency
    p2 <- length_frequency(all_data, c("AK BSAI", "AK GULF", "PBS", "NWFSC"), input$species, time_series = TRUE)
    
    # Age frequency
    p3 <- age_frequency(all_data, c("AK BSAI", "AK GULF", "PBS", "NWFSC"), input$species, cutoff = 0.75)
    
    # Combine with patchwork
    p1 + p2 + p3 + plot_layout(ncol = 1)
  })
  
}

# Run Shiny app
shinyApp(ui = ui, server = server)
