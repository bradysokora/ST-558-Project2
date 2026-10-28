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
library(shinycssloaders)

# cleaning function, variable lookups, and summary helpers
source("helpers.R")

# read the data once when the app starts so every session shares it
housing <- read_melb_data()


# App helper functions ---------------------------------------------------

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

# need_rows()
# If the filters leave nothing behind, this shows a friendly message
# instead of a red error. Only the plots and the data table use this, so
# the message shows up once per section instead of twice.
need_rows <- function(df) {
  validate(need(
    nrow(df) > 0,
    paste("No sales match the current filters. Make sure at least one",
          "Property Type is checked, loosen the sliders, and click Apply Filters again.")
  ))
}

# spinner()
# Wraps an output in a loading spinner. One spot to change the look.
spinner <- function(ui_element) {
  shinycssloaders::withSpinner(ui_element, type = 6, color = "#2c7fb8")
}

# make_num_plot()
# Builds whichever numeric plot the user picked. Keeping this out of the
# server so renderPlot() is short and each plot type is easy to find.
#   plot_type: "hist", "box", "scatter", or "heat"
#   num_var:   the numeric variable being plotted
#   group_var: categorical x axis for box plots and heatmaps
#   scatter_x: numeric x axis for scatter plots
#   heat_y:    second categorical variable for heatmap rows
#   color_var: categorical variable for color/fill, or "none"
#   facet_var: categorical variable to facet by, or "none"
#   log_scale: TRUE/FALSE, puts the numeric axis on a log scale
make_num_plot <- function(df, plot_type, num_var, group_var, scatter_x,
                          heat_y, color_var, facet_var, log_scale) {
  
  num_lab <- var_label(num_var)
  
  # heatmap is its own thing since it summarizes first, so handle it and
  # return early (color, facet, and log scale don't apply here)
  if (plot_type == "heat") {
    p <- df |>
      dplyr::group_by(.data[[group_var]], .data[[heat_y]]) |>
      dplyr::summarize(med = median(.data[[num_var]], na.rm = TRUE),
                       .groups = "drop") |>
      # cells with no values get a blank label instead of the text "NA"
      dplyr::mutate(med_label = dplyr::if_else(
        is.na(med), "", scales::comma(med, accuracy = 1)
      )) |>
      ggplot(aes(x = .data[[group_var]], y = .data[[heat_y]], fill = med)) +
      geom_tile(color = "white") +
      geom_text(aes(label = med_label), size = 3.5) +
      scale_fill_distiller(palette = "YlOrRd", direction = 1,
                           labels = scales::label_comma(),
                           na.value = "grey90") +
      labs(
        title = paste("Median", num_lab, "by", var_label(group_var),
                      "and", var_label(heat_y)),
        x     = var_label(group_var),
        y     = var_label(heat_y),
        fill  = paste("Median", num_lab)
      ) +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
    
    return(p)
  }
  
  # log scales can't handle 0 or negatives (land size has a bunch of 0s),
  # so drop those rows and tell the user how many got left out
  n_dropped <- 0
  if (log_scale) {
    bad_rows  <- !is.na(df[[num_var]]) & df[[num_var]] <= 0
    n_dropped <- sum(bad_rows)
    df        <- df[!bad_rows, ]
  }
  
  has_color <- color_var != "none"
  log_note  <- if (log_scale) " (log scale)" else ""
  
  # histogram: distribution of one numeric variable
  if (plot_type == "hist") {
    p <- ggplot(df, aes(x = .data[[num_var]]))
    
    p <- if (has_color) {
      p + geom_histogram(aes(fill = .data[[color_var]]),
                         bins = 40, color = "white")
    } else {
      p + geom_histogram(bins = 40, fill = "darkorange", color = "white")
    }
    
    p <- p +
      (if (log_scale) scale_x_log10(labels = scales::label_comma())
       else scale_x_continuous(labels = scales::label_comma())) +
      labs(title = paste("Distribution of", num_lab),
           x = paste0(num_lab, log_note),
           y = "Number of Sales")
  }
  
  # box plot: numeric variable across levels of a categorical variable
  if (plot_type == "box") {
    p <- ggplot(df, aes(x = .data[[group_var]], y = .data[[num_var]]))
    
    p <- if (has_color) {
      p + geom_boxplot(aes(fill = .data[[color_var]]), outlier.alpha = 0.2)
    } else {
      p + geom_boxplot(fill = "steelblue", alpha = 0.6, outlier.alpha = 0.2)
    }
    
    p <- p +
      (if (log_scale) scale_y_log10(labels = scales::label_comma())
       else scale_y_continuous(labels = scales::label_comma())) +
      labs(title = paste(num_lab, "by", var_label(group_var)),
           x = var_label(group_var),
           y = paste0(num_lab, log_note))
  }
  
  # scatter plot: two numeric variables against each other
  if (plot_type == "scatter") {
    p <- ggplot(df, aes(x = .data[[scatter_x]], y = .data[[num_var]]))
    
    p <- if (has_color) {
      p + geom_point(aes(color = .data[[color_var]]), alpha = 0.3, size = 1.2)
    } else {
      p + geom_point(color = "steelblue", alpha = 0.3, size = 1.2)
    }
    
    p <- p +
      scale_x_continuous(labels = scales::label_comma()) +
      (if (log_scale) scale_y_log10(labels = scales::label_comma())
       else scale_y_continuous(labels = scales::label_comma())) +
      labs(title = paste(num_lab, "vs", var_label(scatter_x)),
           x = var_label(scatter_x),
           y = paste0(num_lab, log_note))
  }
  
  # let the user know if the log scale left some sales out
  if (n_dropped > 0) {
    p <- p + labs(subtitle = paste(scales::comma(n_dropped),
                                   "sales with a value of 0 were left out so the log scale works"))
  }
  
  # legend title, only matters if something is colored
  if (has_color) {
    p <- p + labs(fill = var_label(color_var), color = var_label(color_var))
  }
  
  if (facet_var != "none") {
    p <- p + facet_wrap(vars(.data[[facet_var]]))
  }
  
  p +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
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
    # quick way to get every level back after unchecking some
    actionLink(
      inputId = "select_all_types",
      label   = "Select all property types"
    ),
    br(), br(),
    
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
      
      p("This table shows the data after your sidebar filters are applied.",
        "Change the filters and click Apply Filters to update it, then use",
        "the button to save exactly what you see as a csv."),
      
      # row count so the user knows how much data survived the filters
      textOutput("n_rows"),
      br(),
      
      downloadButton(
        outputId = "download_data",
        label    = "Download Filtered Data (.csv)",
        class    = "btn-success"
      ),
      br(), br(),
      
      spinner(DT::dataTableOutput("data_table"))
    ),
    
    nav_panel(
      title = "Data Exploration",
      
      # short description so this tab explains itself like the others do
      p("Explore the filtered data with tables and plots. Pick",
        strong("Categorical Summaries"), "for counts, contingency tables, and",
        "bar charts, or", strong("Numeric Summaries"), "for summary stats",
        "across groups plus histograms, box plots, scatter plots, and a",
        "heatmap. Use the controls on the left of each section to choose the",
        "variables and how to color or facet the plots. Everything here",
        "updates when you click Apply Filters in the sidebar."),
      
      # the big switch: categorical stuff or numeric stuff
      radioButtons(
        inputId  = "explore_type",
        label    = "What do you want to explore?",
        choices  = c("Categorical Summaries" = "cat",
                     "Numeric Summaries"     = "num"),
        selected = "cat",
        inline   = TRUE
      ),
      hr(),
      
      # Categorical section ----------------------------------------------
      conditionalPanel(
        condition = "input.explore_type == 'cat'",
        
        layout_columns(
          col_widths = c(3, 9),
          
          # left: controls for this section only (not in the sidebar on
          # purpose, since these pick what to plot, not how to filter)
          div(
            selectInput(
              inputId  = "cat_x",
              label    = "Main Variable",
              choices  = cat_vars,
              selected = "type"
            ),
            selectInput(
              inputId  = "cat_fill",
              label    = "Second Variable (two-way table and bar color)",
              choices  = c("None" = "none", cat_vars),
              selected = "region_group"
            ),
            radioButtons(
              inputId  = "bar_position",
              label    = "Bar Style (when a second variable is picked)",
              choices  = c("Side by Side" = "dodge",
                           "Stacked"      = "stack",
                           "Proportions"  = "fill"),
              selected = "dodge"
            ),
            selectInput(
              inputId  = "cat_facet",
              label    = "Facet By",
              choices  = c("None" = "none", cat_vars),
              selected = "none"
            )
          ),
          
          # right: the table and the bar chart
          div(
            h5(textOutput("cat_table_title")),
            div(style = "overflow-x: auto;", tableOutput("cat_table")),
            spinner(plotOutput("cat_bar", height = "450px"))
          )
        )
      ),
      
      # Numeric section --------------------------------------------------
      conditionalPanel(
        condition = "input.explore_type == 'num'",
        
        layout_columns(
          col_widths = c(3, 9),
          
          # left: controls for the numeric summaries and plots
          div(
            selectInput(
              inputId  = "num_var",
              label    = "Numeric Variable",
              choices  = num_vars,
              selected = "price"
            ),
            selectInput(
              inputId  = "num_group",
              label    = "Summarize Across",
              choices  = cat_vars,
              selected = "type"
            ),
            helpText("Summarize Across sets the groups in the table, and the",
                     "x axis for box plots and heatmaps."),
            
            radioButtons(
              inputId  = "plot_type",
              label    = "Plot Type",
              choices  = c("Histogram"          = "hist",
                           "Box Plot"           = "box",
                           "Scatter Plot"       = "scatter",
                           "Heatmap of Medians" = "heat"),
              selected = "box"
            ),
            
            # only shows up for scatter plots
            conditionalPanel(
              condition = "input.plot_type == 'scatter'",
              selectInput(
                inputId  = "scatter_x",
                label    = "X Axis Variable",
                choices  = num_vars,
                selected = "distance"
              )
            ),
            
            # only shows up for heatmaps
            conditionalPanel(
              condition = "input.plot_type == 'heat'",
              selectInput(
                inputId  = "heat_y",
                label    = "Second Categorical Variable (heatmap rows)",
                choices  = cat_vars,
                selected = "region"
              )
            ),
            
            # color, facet, and log scale don't apply to the heatmap
            conditionalPanel(
              condition = "input.plot_type != 'heat'",
              selectInput(
                inputId  = "num_color",
                label    = "Color By",
                choices  = c("None" = "none", cat_vars),
                selected = "none"
              ),
              selectInput(
                inputId  = "num_facet",
                label    = "Facet By",
                choices  = c("None" = "none", cat_vars),
                selected = "none"
              ),
              checkboxInput(
                inputId = "log_scale",
                label   = "Log scale for the numeric axis (helps with price)",
                value   = TRUE
              )
            )
          ),
          
          # right: the summary table and the plot
          div(
            h5(textOutput("num_table_title")),
            div(style = "overflow-x: auto;", tableOutput("num_table")),
            spinner(plotOutput("num_plot", height = "500px"))
          )
        )
      )
    )
  )
)


# Server -----------------------------------------------------------------

server <- function(input, output, session) {
  
  # Select all link ------------------------------------------------------
  # Re-checks every property type box. Like everything else in the sidebar,
  # it doesn't change the data until Apply Filters is clicked.
  
  observeEvent(input$select_all_types, {
    updateCheckboxGroupInput(
      session  = session,
      inputId  = "type_filter",
      selected = levels(housing$type)
    )
  })
  
  
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
  # If no property types are checked this just returns 0 rows, and the
  # outputs handle that with need_rows().
  
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
  
  # the table, scrollX keeps it from blowing out the page width
  output$data_table <- DT::renderDataTable({
    df <- filtered_data()
    need_rows(df)
    
    DT::datatable(
      df,
      rownames = FALSE,
      options  = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  # saves whatever is currently filtered, with today's date in the name
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("melbourne_housing_filtered_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(filtered_data(), file)
    }
  )
  
  
  # Data Exploration: categorical ----------------------------------------
  
  # title above the table changes depending on one-way or two-way
  output$cat_table_title <- renderText({
    if (input$cat_fill == "none") {
      paste("One-Way Table:", var_label(input$cat_x))
    } else {
      paste("Two-Way Table:", var_label(input$cat_x), "(rows) by",
            var_label(input$cat_fill), "(columns)")
    }
  })
  
  # one-way table if no second variable, two-way table if there is one.
  # req() hides the table quietly when something's off, and the bar chart
  # below shows the actual message, so it only appears once
  output$cat_table <- renderTable({
    df <- filtered_data()
    req(nrow(df) > 0, input$cat_fill != input$cat_x)
    
    if (input$cat_fill == "none") {
      tab <- as.data.frame(table(df[[input$cat_x]]))
      names(tab) <- c(var_label(input$cat_x), "Count")
    } else {
      tab <- table(df[[input$cat_x]], df[[input$cat_fill]]) |>
        as.data.frame.matrix() |>
        tibble::rownames_to_column(var = var_label(input$cat_x))
    }
    
    tab
  }, striped = TRUE, digits = 0)
  
  # bar chart that adds color, bar style, and facets based on the controls
  output$cat_bar <- renderPlot({
    df <- filtered_data()
    need_rows(df)
    validate(
      need(input$cat_fill != input$cat_x,
           "Pick a Second Variable that's different from the Main Variable (or set it to None)."),
      need(input$cat_facet != input$cat_x,
           "Pick a Facet By variable that's different from the Main Variable (or set it to None).")
    )
    
    p <- ggplot(df, aes(x = .data[[input$cat_x]]))
    
    if (input$cat_fill == "none") {
      p <- p + geom_bar(fill = "steelblue")
    } else {
      p <- p +
        geom_bar(aes(fill = .data[[input$cat_fill]]),
                 position = input$bar_position) +
        labs(fill = var_label(input$cat_fill))
    }
    
    if (input$cat_facet != "none") {
      p <- p + facet_wrap(vars(.data[[input$cat_facet]]))
    }
    
    # proportions get a different y label than counts
    y_lab <- if (input$cat_fill != "none" && input$bar_position == "fill") {
      "Proportion of Sales"
    } else {
      "Number of Sales"
    }
    
    p +
      labs(
        title = paste("Sales by", var_label(input$cat_x)),
        x     = var_label(input$cat_x),
        y     = y_lab
      ) +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })
  
  
  # Data Exploration: numeric --------------------------------------------
  
  output$num_table_title <- renderText({
    paste("Summary of", var_label(input$num_var), "by",
          var_label(input$num_group))
  })
  
  # uses summarize_num_by() from helpers.R, then formats the numbers.
  # big variables (like price) get whole numbers, small ones (like
  # distance) keep one decimal so the table stays narrow but still useful.
  # req() hides it quietly when there's nothing to show, the plot below
  # handles the message
  output$num_table <- renderTable({
    df <- filtered_data()
    req(nrow(df) > 0, any(!is.na(df[[input$num_var]])))
    
    big_var  <- max(abs(df[[input$num_var]]), na.rm = TRUE) >= 1000
    accuracy <- if (big_var) 1 else 0.1
    
    summarize_num_by(df, input$num_var, input$num_group) |>
      dplyr::mutate(
        n = scales::comma(n),
        dplyr::across(mean:max, ~ scales::comma(.x, accuracy = accuracy))
      ) |>
      setNames(c(var_label(input$num_group),
                 "N", "Mean", "Median", "SD", "Min", "Max"))
  }, striped = TRUE, na = "none")
  
  # all the plot logic lives in make_num_plot() up top, this just checks
  # for problem combos first so the user gets a message, not an error
  output$num_plot <- renderPlot({
    df <- filtered_data()
    need_rows(df)
    validate(need(
      any(!is.na(df[[input$num_var]])),
      paste("There are no", var_label(input$num_var),
            "values left after filtering. Try a different variable or loosen the filters.")
    ))
    
    if (input$plot_type == "scatter") {
      validate(need(
        input$scatter_x != input$num_var,
        "Pick an X Axis Variable that's different from the Numeric Variable."
      ))
    }
    
    if (input$plot_type == "heat") {
      validate(need(
        input$heat_y != input$num_group,
        "Pick a heatmap row variable that's different from Summarize Across."
      ))
    }
    
    make_num_plot(
      df        = df,
      plot_type = input$plot_type,
      num_var   = input$num_var,
      group_var = input$num_group,
      scatter_x = input$scatter_x,
      heat_y    = input$heat_y,
      color_var = input$num_color,
      facet_var = input$num_facet,
      log_scale = input$log_scale
    )
  })
  
}


shinyApp(ui = ui, server = server)