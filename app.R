# app.R
# Brady Sokora, ST 558 Project 2
#
# Melbourne Housing Explorer. The sidebar subsets the data, and the three
# tabs in the main panel (About, Data Download, Data Exploration) all use
# that same subsetted data.
#
# Right now this is just the skeleton so I can make sure the layout works
# before adding the real pieces one at a time.

library(shiny)
library(bslib)
library(tidyverse)
library(DT)

# cleaning function, variable lookups, and summary helpers
source("helpers.R")

# read the data once when the app starts so every session shares it
housing <- read_melb_data()


# UI ---------------------------------------------------------------------

ui <- page_sidebar(
  title = "Melbourne Housing Explorer",

  # sidebar: only the filtering widgets go here so they work across all tabs
  sidebar = sidebar(
    title = "Subset the Data",
    width = 300,
    p("Filters coming next commit.")
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

  # nothing yet, the subsetting logic comes in the next commits

}


shinyApp(ui = ui, server = server)
