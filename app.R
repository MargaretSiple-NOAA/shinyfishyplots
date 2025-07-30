# Load packages
library(shiny)
library(bslib)
library(surveyjoin)
library(sdmTMB)
library(fishyplots) #devtools::install_github("DFO-NOAA-Pacific/fishyplots")
library(ggplot2)
library(dplyr)
library(patchwork)

# Load biological data
data(nwfsc_bio)
data(afsc_bio)
data(pbs_bio)
akbsai <- afsc_bio |> filter(survey == "AK BSAI")
akgulf <- afsc_bio |> filter(survey == "AK GULF")
all_data <- bind_rows(afsc_bio, nwfsc_bio, pbs_bio)

# Load prediction data
data(vb_predictions)
data(predictions_afsc)
data(predictions_nwfsc)
data(predictions_pbs)
predictions <- rbind(predictions_afsc, predictions_pbs, predictions_nwfsc)

#load biomass data
data("all.dbi")

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
    tabPanel("Biomass",
             plotOutput("dbiPlot", height = "1000px")), 
    tabPanel("Age and length",
             plotOutput("agelengthPlot", height = "1000px")),
    tabPanel("Maps",
             plotOutput("modelPlot", height = "1200px")),
    tabPanel("Data",
             plotOutput("surveytable"),
             tableOutput("demotable"),
             downloadButton("downloadbio", "Download biological data"),
             tableOutput("vbtable"),
             downloadButton("downloadvb", "Download growth predictions"),
             tableOutput("maptable"),
             downloadButton("downloadmap", "Download density predictions"))
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
  region_species <- spp_list[[input$region]]

  # Check if currently selected species is also present in the newly selected region:
  current_spp <- input$species

  if (!is.null(current_spp) && current_spp %in% region_species) {
    selected_species <- current_spp
  } else {
    selected_species <- "None selected"
  }

  updateSelectInput(
    session,
    "species",
    choices = c("None selected", region_species),
    selected = selected_species
  )
  })
  
  # Dynamic subsetting for downloading data
  bio_subset <- reactive({
    subset(all_data, common_name == input$species)
  })
  vb_subset <- reactive({
    subset(vb_predictions, common_name == input$species)
  })
  map_subset <- reactive({
    subset(predictions, species == input$species)
  })
  
  # Map plots
  output$modelPlot <- renderPlot({
    req(input$species != "None selected")
    fishmap(predictions, region_names(), input$species)
  })
  
  # Length, age, growth plots 
  output$agelengthPlot <- renderPlot({
    req(input$species != c("None selected", ""))
    # Growth plot
    p1 <- plot_growth(all_data, vb_predictions, region_names(), input$species) 
    # Length frequency
    p2 <- length_weight(subset(all_data, survey == region_names()), input$species, subset = TRUE)
    # Age frequency
    p3 <- age_frequency(all_data, region_names(), input$species, cutoff = 0.75)
    # Length-weight
    p4 <- length_frequency(all_data, region_names(), input$species, time_series = TRUE)
    # Combine with patchwork
    p1 + p2 + p3 + p4 + plot_layout(ncol = 1)
  })
  
  # DBI Biomass plots
  output$dbiPlot <- renderPlot({
    req(input$species != c("None selected", ""))
    # Growth plot
    pdbi1 <- plot_dbi(input$species,region_names()) 
    
    # Length frequency
    pdbi2 <- plot_stan_dbi(input$species,region_names())
    
    # Combine with patchwork
    pdbi1 + pdbi2 + plot_layout(ncol = 1)
  })
  
  # Download data tab
  output$surveytable <- renderPlot({
    req(input$species != c("None selected", ""))
    survey_table(subset(all_data, survey == region_names()), input$species, form = 2)
  })
  
  output$demotable <- renderTable({
    head(bio_subset(), n = 2)
  })
  output$downloadbio <- downloadHandler(
    filename = function() {
      paste0("biodata_", input$species, ".csv")
    },
    content = function(file) {
      write.csv(bio_subset(), file)
    }
  )
  
  output$vbtable <- renderTable({
    head(vb_subset(), n = 2)
  })
  output$downloadvb <- downloadHandler(
    filename = function() {
      paste0("growth_predictions_", input$species, ".csv")
    },
    content = function(file) {
      write.csv(vb_subset(), file)
    }
  )
  
  
  output$maptable <- renderTable({
    head(map_subset(), n = 2)
  })
  output$downloadmap <- downloadHandler(
    filename = function() {
      paste0("density_predictions_", input$species, ".csv")
    },
    content = function(file) {
      write.csv(map_subset(), file)
    }
  )
}

# Run Shiny app
shinyApp(ui = ui, server = server)
