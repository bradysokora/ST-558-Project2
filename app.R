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
    uiOutput("num2_slider"),
    
    hr(),
    
    # nothing in the main panel updates until this gets clicked
    actionButton(
      inputId = "apply_filters",
      label   = "Apply Filters",
      class   = "btn-primary",
      width   = "100%"
    )
  ),
  
  # main panel tabs, About is first so it's the default tab
  navset_card_underline(
    id = "main_tabs",
    
    nav_panel(
      title = "About",
      
      layout_columns(
        col_widths = c(8, 4),
        
        # left side: what the app is and how to use it
        div(
          h3("What this app is for"),
          p("This app lets you dig into property sales in Melbourne, Australia.",
            "You can narrow the data down to the kinds of sales you care about,",
            "look at the raw rows, download them, and make tables and plots to",
            "see how things like price, size, and location relate to each other."),
          
          h4("The data"),
          p("The data is the Melbourne Housing Snapshot from Kaggle. Each row is",
            "one property sale from 2016 and 2017, with the sale price, property",
            "type, how it was sold, the region, distance from the city center,",
            "and size details like rooms, land size, and building area."),
          p("More info and the original file: ",
            a("Melbourne Housing Snapshot on Kaggle",
              href   = "https://www.kaggle.com/datasets/dansbecker/melbourne-housing-snapshot",
              target = "_blank")),
          
          h4("How to use it"),
          tags$ul(
            tags$li(strong("Sidebar: "),
                    "pick which property types and region group to keep, choose",
                    "up to two numeric variables, and set a range for each with",
                    "the sliders. Nothing changes until you click",
                    strong("Apply Filters"), "."),
            tags$li(strong("About: "),
                    "this page."),
            tags$li(strong("Data Download: "),
                    "see the filtered data in a table and save it as a csv."),
            tags$li(strong("Data Exploration: "),
                    "make contingency tables, numeric summaries, and plots from",
                    "the filtered data, choosing which variables to use and how",
                    "to color or facet them.")
          )
        ),
        
        # right side: picture
        div(
          tags$img(src = "melbourne.jpg",
                   alt = "Melbourne city skyline",
                   style = "width: 100%; border-radius: 8px;"),
          p(em("Melbourne skyline. Photo by David Iliff, Wikimedia Commons (CC BY-SA)."),
            style = "font-size: 0.85em; margin-top: 6px;")
        )
      )
    ),
    
    nav_panel(
      title = "Data Download",
      # quick check that the filtering works, the table comes next commit
      textOutput("n_rows")
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
  
  
  # Subsetting -----------------------------------------------------------
  # eventReactive() means this only reruns when Apply Filters is clicked,
  # not every time a widget changes. ignoreNULL = FALSE lets it run once
  # when the app first opens so the tabs aren't empty before any click.
  # Every tab pulls from filtered_data() so they all stay in sync.
  
  filtered_data <- eventReactive(input$apply_filters, {
    
    df <- housing |>
      dplyr::filter(type %in% input$type_filter)
    
    if (input$region_filter != "All") {
      df <- df |>
        dplyr::filter(region_group == input$region_filter)
    }
    
    # numeric filters only apply if a variable is picked and its slider
    # has loaded. Note this drops rows that are missing that variable.
    if (input$num1_var != "none" && !is.null(input$num1_range)) {
      df <- df |>
        dplyr::filter(dplyr::between(.data[[input$num1_var]],
                                     input$num1_range[1],
                                     input$num1_range[2]))
    }
    
    if (input$num2_var != "none" && !is.null(input$num2_range)) {
      df <- df |>
        dplyr::filter(dplyr::between(.data[[input$num2_var]],
                                     input$num2_range[1],
                                     input$num2_range[2]))
    }
    
    df
    
  }, ignoreNULL = FALSE)
  
  
  # Data Download tab ----------------------------------------------------
  
  output$n_rows <- renderText({
    paste0(format(nrow(filtered_data()), big.mark = ","),
           " of ", format(nrow(housing), big.mark = ","),
           " sales match the current filters.")
  })
  
}


shinyApp(ui = ui, server = server)