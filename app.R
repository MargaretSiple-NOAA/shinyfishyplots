# Load packages
library(shiny)
library(bslib)
library(surveyjoin)
library(sdmTMB)
library(fishyplots)
library(ggplot2)
library(dplyr)
library(patchwork)

#### Data ####

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
predictions <- bind_rows(predictions_afsc, predictions_pbs, predictions_nwfsc)
predictions <- predictions |>
  mutate(subregion = case_when(
    region == "NWFSC" ~ "NWFSC",
    region == "PBS" ~ "PBS",
    survey == "Gulf of Alaska Bottom Trawl Survey" ~ "AK GULF",
    TRUE ~ "AK BSAI"
  ))

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
  "All regions" = sort(overlap)
)

#### Define User Interface ####
ui <- page_sidebar(
  title = "Coastwide fishery synopsis",
  sidebar_width = 2,
  sidebar = sidebar(
    helpText("Plots from NOAA and DFO survey data."),
    radioButtons(
      inputId = "region",
      label = "Choose a region",
      choices = list("All regions", "Aleutians/Bering Sea", "Gulf of Alaska", "Canada", "US West Coast"),
      selected = "All regions"
    ),
    conditionalPanel( # show when All regions is selected for biomass plots
      condition = "input.region == 'All regions' && input.tabs == 'Biomass'",
      checkboxGroupInput(
        inputId = "surveys_selected",
        label = "Select surveys (Biomass only)",
        choices = c("U.S. West Coast", "SYN HS", "SYN QCS", "SYN WCHG", "SYN WCVI", "Gulf of Alaska" = "U.S. Gulf of Alaska", "Aleutian Islands" = "U.S. Aleutian Islands", "Eastern Bering Slope" = "U.S. Eastern Bering Sea Slope", "Eastern Bering and NW" = "U.S. Eastern Bering Sea Standard Plus NW Region", "Northern Bering" = "U.S. Northern Bering Sea")
      )
    ),
    
    selectInput(
      "species",
      label = "Choose a species",
      choices = NULL
    )
  ),
  tabsetPanel(
    id = "tabs",
    tabPanel("Biomass",
             uiOutput("dbiPlotUI")), #dynamic height
    tabPanel("Age and length",
             #plotOutput("agelengthPlot", height = "1000px")),
             uiOutput("dynamic_agelength")),
    tabPanel("Maps",
             plotOutput("modelPlot", height = "1200px")),
    tabPanel("Depth",
             plotOutput("depthPlot")),
    tabPanel("Data",
             div(style = "overflow-x: scroll; min-width: 1200px;",
                 plotOutput("surveytable")),
             tableOutput("demotable"),
             downloadButton("downloadbio", "Download biological data"),
             tableOutput("vbtable"),
             downloadButton("downloadvb", "Download growth predictions"),
             tableOutput("lwtable"),
             downloadButton("downloadlw", "Download length-weight predictions"),
             tableOutput("maptable"),
             downloadButton("downloadmap", "Download density predictions"),
             tableOutput("dbitable"),
             downloadButton("downloaddbi", "Download design-based biomass indicies"))
  )
)

#### Define Server ####
server <- function(input, output, session) {
  
  # Dynamic species selection based on region
  region_names <- reactive({
    switch(input$region,
           "US West Coast" = "NWFSC", "Canada" = "PBS", "Aleutians/Bering Sea" = "AK BSAI", "Gulf of Alaska" = "AK GULF", "All regions" = c("AK BSAI", "AK GULF", "PBS", "NWFSC"))
  })
  observeEvent(input$region, {
    updateSelectInput(
      session,
      "species",
      choices = c("None selected", spp_list[[input$region]])
      #selected = "Arrowtooth flounder"
    )
  })

  
  # Dynamic subsetting for downloading data
  bio_subset <- reactive({
    all_data <- all_data |> select(-otosag_id)
    subset(all_data, common_name == input$species & survey %in% region_names())
  })
  vb_subset <- reactive({
    subset(vb_predictions, common_name == input$species & survey %in% region_names())
  })
  map_subset <- reactive({
    predictions <- predictions |> select(-sanity) |> select(-survey)
    subset(predictions, species == input$species & subregion %in% region_names())
  })
  lw_subset <- reactive({
    subset(lw_predictions, common == input$species & survey %in% region_names())
  })
  dbi_subset <- reactive({
    subset(all.dbi, common_name == input$species & survey %in% region_names())
  })
  
  # Map plots
  output$modelPlot <- renderPlot({
    req(input$species != "None selected")
    fishmap(predictions, region_names(), input$species)
  })
  
  # Length, age, growth plots 
  output$dynamic_agelength <- renderUI({
    width <- if (region_names() == "All regions") "100%" else "80%"
    plotOutput("agelengthPlot", width = width, height = "1000px")
  })
  
  output$agelengthPlot <- renderPlot({
    req(input$species != c("None selected", ""))
    # Growth plot
    p1 <- plot_growth(all_data, vb_predictions, region_names(), input$species) 
    # Length - weight
    p2 <- length_weight(subset(all_data, survey %in% region_names()), input$species, subset = TRUE)
    # Age frequency
    p3 <- age_frequency(all_data, region_names(), input$species, cutoff = 0.75)
    # Llength frequency
    p4 <- length_frequency(all_data, region_names(), input$species, time_series = TRUE)
    # Combine with patchwork
    p1 + p2 + p3 + p4 + plot_layout(ncol = 1)
  })
  
  
  
  # Depth plots
  output$depthPlot <- renderPlot({
    req(input$species != "None selected")
    depth_plot(all_data, region_names(), input$species)
  })
  
  # DBI Biomass plots
  output$dbiPlotUI <- renderUI({
    if (input$region == "All regions") {
      plotOutput("dbiPlot", height = "500px")  # smaller for All regions, only one plot
    } else {
      plotOutput("dbiPlot", height = "900px")  # larger for stacked plots
    }
  })
  
  output$dbiPlot <- renderPlot({
    req(input$species != c("None selected", ""))
  if (input$region == "All regions") {
    req(input$surveys_selected)
  
    plot_stan_dbi(input$species, input$surveys_selected) # show only standardized plot if All regions selected
    
  } else {
    # show normal and standardized
    pdbi1 <- plot_dbi(input$species, region_names())
    pdbi2 <- plot_stan_dbi(input$species, region_names())
    pdbi1 + pdbi2 + plot_layout(ncol = 1)
  }
})
  
  # Download data tab
  # Survey table
  output$surveytable <- renderPlot({
    req(input$species != c("None selected", ""))
    survey_table(subset(all_data, survey == region_names()), input$species, form = 2)
  }, height = function() {
    200 * length(region_names()) #dynamically change plot size based on amount
  })
  # Download biological data
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
  # Download growth predictions
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
  
  # Download map predictions
  output$maptable <- renderTable({
    #map_subset() <- map_subset() |> select(-sanity)
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
  
  # Download LW predictions
  output$lwtable <- renderTable({
    head(lw_subset(), n = 2)
  })
  output$downloadlw <- downloadHandler(
    filename = function() {
      paste0("length_weight_predictions_", input$species, ".csv")
    },
    content = function(file) {
      write.csv(lw_subset(), file)
    }
  )
  
  # Download DBI 
  output$dbitable <- renderTable({
    head(dbi_subset(), n = 2)
  })
  output$downloaddbi <- downloadHandler(
    filename = function() {
      paste0("design_biomass_index_", input$species, ".csv")
    },
    content = function(file) {
      write.csv(dbi_subset(), file)
    }
  )
}

#### Run Shiny app ####
shinyApp(ui = ui, server = server)
