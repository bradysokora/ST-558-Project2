# Melbourne Housing Explorer (ST 558 Project 2)

**Live app:** https://bradysokora.shinyapps.io/melbourne-housing/

This is a Shiny app for exploring property sales in Melbourne, Australia. You can narrow the data down from the sidebar, look at and download the filtered rows, and build your own contingency tables, numeric summaries, and plots to see how price, size, and location relate to each other.

## What the app does

- **Sidebar:** filter by property type and region group, then pick up to two numeric variables and set a range for each with sliders. Nothing updates until you click Apply Filters.
- **About tab:** explains the app, the data, and how to use each part.
- **Data Download tab:** shows the filtered data in a searchable table and lets you save it as a csv.
- **Data Exploration tab:** switch between categorical and numeric summaries. Make one and two way contingency tables, bar charts, summary stats across groups, histograms, box plots, scatter plots, and a heatmap, with options to color and facet by other variables.

## Data

Melbourne Housing Snapshot from Kaggle: https://www.kaggle.com/datasets/dansbecker/melbourne-housing-snapshot

Each row is one property sale from 2016 and 2017.

## Files

- `app.R` is the Shiny app
- `helpers.R` has the shared cleaning function, variable lookups, and summary helpers used by both the app and the EDA file
- `eda.qmd` is the static exploration I did before building the app (tables, numeric summaries, and plots)
- `data/` holds the csv
- `www/` holds the image used on the About tab

## Running it locally

Install the packages once:

    install.packages(c("shiny", "bslib", "tidyverse", "DT", "shinycssloaders"))

Then open `app.R` in RStudio and click Run App.