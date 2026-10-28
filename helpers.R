# helpers.R
# Brady Sokora, ST 558 Project 2
#
# Shared functions and lookups used by both the static EDA file (eda.qmd)
# and the shiny app (app.R). I'm keeping all of this in one spot so I'm not
# copying the same cleaning code twice, and so both files always agree on
# what the data looks like.

library(tidyverse)


# Reading and cleaning ---------------------------------------------------

# read_melb_data()
# Reads the raw Kaggle csv and hands back a cleaned tibble with snake_case
# names, labeled factors, a real date column, and a couple of obviously bad
# values set to NA.
read_melb_data <- function(path = "data/melb_data.csv") {
  
  raw <- readr::read_csv(path, show_col_types = FALSE)
  
  raw |>
    # only keeping the columns I plan to use, and renaming them while I'm at it
    dplyr::select(
      suburb        = Suburb,
      rooms         = Rooms,
      type          = Type,
      price         = Price,
      method        = Method,
      sale_date     = Date,
      distance      = Distance,
      bathrooms     = Bathroom,
      car_spots     = Car,
      land_size     = Landsize,
      building_area = BuildingArea,
      year_built    = YearBuilt,
      council       = CouncilArea,
      region        = Regionname,
      latitude      = Lattitude,   # yes, it's spelled wrong in the raw file
      longitude     = Longtitude
    ) |>
    dplyr::mutate(
      # property type comes in as single letters, so I'm spelling them out
      type = factor(type,
                    levels = c("h", "t", "u"),
                    labels = c("House", "Townhouse", "Unit")),
      
      # same deal for how the property was sold
      method = factor(method,
                      levels = c("S", "SP", "PI", "VB", "SA"),
                      labels = c("Sold", "Sold Prior", "Passed In",
                                 "Vendor Bid", "Sold After Auction")),
      
      region = factor(region),
      
      # 8 regions is a lot for a sidebar, so I'm also making a 2 level version
      # (anything "Metropolitan" vs the outer Victoria regions)
      region_group = dplyr::if_else(
        stringr::str_detect(region, "Metropolitan"), "Metro", "Regional"
      ) |>
        factor(levels = c("Metro", "Regional")),
      
      # dates are stored day/month/year in this file
      sale_date = lubridate::dmy(sale_date),
      sale_year = factor(lubridate::year(sale_date)),
      
      # rooms as a grouped category too, so it can be used for coloring,
      # faceting, and contingency tables (5+ rooms is rare so I lumped those)
      rooms_group = dplyr::case_when(
        rooms <= 1 ~ "1",
        rooms == 2 ~ "2",
        rooms == 3 ~ "3",
        rooms == 4 ~ "4",
        rooms >= 5 ~ "5+"
      ) |>
        factor(levels = c("1", "2", "3", "4", "5+")),
      
      # a building area of 0 or a house built before 1800 are clearly entry
      # errors, so I'm setting those to NA instead of tossing the whole row
      building_area = dplyr::if_else(building_area <= 0, NA_real_, building_area),
      year_built    = dplyr::if_else(year_built < 1800, NA_real_, year_built)
    ) |>
    # can't do much with a sale that doesn't have a price
    dplyr::filter(!is.na(price))
}


# Variable lookups -------------------------------------------------------
# Named vectors so the app dropdowns can show nice labels while the code
# still uses the real column names behind the scenes.

cat_vars <- c(
  "Property Type"   = "type",
  "Sale Method"     = "method",
  "Region"          = "region",
  "Region Group"    = "region_group",
  "Rooms (grouped)" = "rooms_group",
  "Sale Year"       = "sale_year"
)

num_vars <- c(
  "Price (AUD)"            = "price",
  "Rooms"                  = "rooms",
  "Bathrooms"              = "bathrooms",
  "Car Spots"              = "car_spots",
  "Distance from CBD (km)" = "distance",
  "Land Size (sq m)"       = "land_size",
  "Building Area (sq m)"   = "building_area",
  "Year Built"             = "year_built"
)

# var_label()
# Turns a column name back into its nice label, e.g. "price" -> "Price (AUD)".
# Handy for slider labels, axis titles, and table headers in the app.
var_label <- function(var) {
  all_vars <- c(cat_vars, num_vars)
  names(all_vars)[match(var, all_vars)]
}


# Summary helpers --------------------------------------------------------

# summarize_num_by()
# Count, mean, median, sd, min, and max for one numeric variable at each
# level of a categorical variable. Both arguments are column names as
# strings, which is exactly what shiny inputs give back, so the app can call
# this directly without any extra tidy eval tricks.
summarize_num_by <- function(df, num_var, group_var) {
  df |>
    dplyr::group_by(.data[[group_var]]) |>
    dplyr::summarize(
      n      = sum(!is.na(.data[[num_var]])),
      mean   = mean(.data[[num_var]], na.rm = TRUE),
      median = median(.data[[num_var]], na.rm = TRUE),
      sd     = sd(.data[[num_var]], na.rm = TRUE),
      min    = min(.data[[num_var]], na.rm = TRUE),
      max    = max(.data[[num_var]], na.rm = TRUE),
      .groups = "drop"
    )
}