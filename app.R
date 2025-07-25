# Load packages
library(shiny)
library(bslib)
library(surveyjoin)
library(sdmTMB)
library(fishyplots)
library(ggplot2)
library(dplyr)
library(patchwork)

# Load biological data
data(nwfsc_bio)
data(afsc_bio)
data(pbs_bio)
akbsai <- afsc_bio |> filter(survey == "AK BSAI")
akgulf <- afsc_bio |> filter(survey == "AK GULF")
nwfsc_bio <- nwfsc_bio |> select(-otosag_id)
all_data <- rbind(afsc_bio, nwfsc_bio, pbs_bio)

# Load prediction data
data(vb_predictions)
data(predictions_afsc)
data(predictions_nwfsc)
data(predictions_pbs)
predictions <- rbind(predictions_afsc, predictions_pbs, predictions_nwfsc)

# Define overlap species
overlap <- all_data |>
  distinct(common_name, region) |>
  count(common_name, name = "n") |>
  filter(n >= 2) |>
  pull(common_name)

# Create species list for each region
spp_list <- list(
  "Aleutians/Bering Sea" = sort(unique(akbsai$common_name)),
  "Gulf of Alaska" = sort(unique(akgulf$common_name)),
  "US West Coast" = sort(unique(nwfsc_bio$common_name)),
  "Canada" = sort(unique(pbs_bio$common_name)),
  "Overlap" = sort(overlap)
)

# Define User Interface
ui <- page_sidebar(
  title = "Coastwide fishery synopsis",
  sidebar_width = 2,
  sidebar = sidebar(
    helpText("Plots from NOAA and DFO survey data."),
    radioButtons(
      inputId = "region",
      label = "Choose a region",
      choices = list("Aleutians/Bering Sea", "Gulf of Alaska", "Canada", "US West Coast", "Overlap"),
      selected = character(0)
    ),
    selectInput(
      "species",
      label = "Choose a species",
      choices = NULL
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
  
  # Dynamic species selection based on region
  region_names <- reactive({
    switch(input$region,
           "US West Coast" = "NWFSC", "Canada" = "PBS", "Aleutians/Bering Sea" = "AK BSAI", "Gulf of Alaska" = "AK GULF", "Overlap" = c("AK BSAI", "AK GULF", "PBS", "NWFSC"))
  })
  observeEvent(input$region, {
    updateSelectInput(
      session,
      "species",
      choices = c("None selected", spp_list[[input$region]]),
      selected = "None selected"
    )
  })
  
  # Map plots
  output$modelPlot <- renderPlot({
    req(input$species != "None selected")
    fishmap(predictions, region_names(), input$species)
  })
  
  # Length, age, growth plots -- add length-weight here!
  output$agelengthPlot <- renderPlot({
    req(input$species != c("None selected", ""))
    # Growth plot
    p1 <- plot_growth(all_data, vb_predictions, region_names(), input$species) 
    
    # Length frequency
    p2 <- length_frequency(all_data, region_names(), input$species, time_series = TRUE)
    
    # Age frequency
    p3 <- age_frequency(all_data, region_names(), input$species, cutoff = 0.75)
    
    # Combine with patchwork
    p1 + p2 + p3 + plot_layout(ncol = 1)
  })
  
}

# Run Shiny app
shinyApp(ui = ui, server = server)
