# app.R
# Brady Sokora, ST 558 Project 2
#
# Melbourne Housing Explorer. The sidebar subsets the data, and the three
# tabs in the main panel (About, Data Download, Data Exploration) all use
# that same subsetted data.

library(shiny)
library(bslib)
library(tidyverse)
library(DT)

# cleaning function, variable lookups, and summary helpers
source("helpers.R")

# read the data once when the app starts so every session shares it
housing <- read_melb_data()

# make_range_slider()
# Builds a two handle slider for a numeric column, spanning its full range.
# Both sliders use this, so the logic lives in one place.
make_range_slider <- function(input_id, var) {
  rng <- range(housing[[var]], na.rm = TRUE)
  
  sliderInput(
    inputId = input_id,
    label   = paste("Range for", var_label(var)),
    min     = floor(rng[1]),
    max     = ceiling(rng[2]),
    value   = c(floor(rng[1]), ceiling(rng[2]))
  )
}


# UI ---------------------------------------------------------------------

ui <- page_sidebar(
  title = "Melbourne Housing Explorer",
  
  # sidebar: only the filtering widgets go here so they work across all tabs
  sidebar = sidebar(
    title = "Subset the Data",
    width = 300,
    
    # categorical filter 1: property type, all boxes checked by default
    # so the user starts with every level selected
    checkboxGroupInput(
      inputId  = "type_filter",
      label    = "Property Type",
      choices  = levels(housing$type),
      selected = levels(housing$type)
    ),
    
    # categorical filter 2: region group, with an explicit "All" option
    # (I collapsed the 8 regions down to Metro vs Regional in helpers.R)
    radioButtons(
      inputId  = "region_filter",
      label    = "Region Group",
      choices  = c("All", levels(housing$region_group)),
      selected = "All"
    ),
    
    hr(),
    
    # numeric filter 1: pick a variable, then a slider shows up for it
    selectInput(
      inputId  = "num1_var",
      label    = "First Numeric Variable to Filter On",
      choices  = c("None" = "none", num_vars),
      selected = "price"
    ),
    uiOutput("num1_slider"),
    
    # numeric filter 2: same idea
    selectInput(
      inputId  = "num2_var",
      label    = "Second Numeric Variable to Filter On",
      choices  = c("None" = "none", num_vars),
      selected = "distance"
    ),
    uiOutput("num2_slider")
  ),
  
  # main panel tabs, About is first so it's the default tab
  navset_card_underline(
    id = "main_tabs",
    
    nav_panel(
      title = "About",
      p("About tab coming soon.")
    ),
    
    nav_panel(
      title = "Data Download",
      p("Data table and download button coming soon.")
    ),
    
    nav_panel(
      title = "Data Exploration",
      p("Summaries and plots coming soon.")
    )
  )
)


# Server -----------------------------------------------------------------

server <- function(input, output, session) {
  
  # Dynamic sliders ------------------------------------------------------
  # Each slider only shows up once a numeric variable is picked. The range
  # comes from the full data so the user can always slide back out.
  
  output$num1_slider <- renderUI({
    req(input$num1_var != "none")
    make_range_slider("num1_range", input$num1_var)
  })
  
  output$num2_slider <- renderUI({
    req(input$num2_var != "none")
    make_range_slider("num2_range", input$num2_var)
  })
  
  # the subsetting logic with the action button comes in the next commit
  
}


shinyApp(ui = ui, server = server)