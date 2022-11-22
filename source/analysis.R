library(tidyverse)
library(plotly)
library(rjson)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Basic values regarding gender in incarcerated populations
#----------------------------------------------------------------------------#
incarceration_df <- get_data()

# Average percentage of female population that were in jail in 2018
avg_female_2018 <- incarceration_df %>%
  filter(
    year == 2018,
    !is.na(total_pop_15to64),
    !is.na(male_jail_pop),
    !is.na(female_jail_pop)
  ) %>%
  mutate(percent_female = female_jail_pop / total_pop_15to64) %>%
  summarize(avg_percent_female = mean(percent_female)) %>%
  pull(avg_percent_female)

# Average percentage of male population that were in jail in 2018
avg_male_2018 <- incarceration_df %>%
  filter(
    year == 2018,
    !is.na(total_pop_15to64),
    !is.na(male_jail_pop),
    !is.na(female_jail_pop)
    ) %>%
  mutate(percent_male = male_jail_pop / total_pop_15to64) %>%
  summarize(avg_percent_male = mean(percent_male)) %>%
  pull(avg_percent_male)

# Average percentage of female population that were in jail in 2000
avg_female_2000 <- incarceration_df %>%
  filter(
    year == 2000,
    !is.na(total_pop_15to64),
    !is.na(male_jail_pop),
    !is.na(female_jail_pop)
  ) %>%
  mutate(percent_female = female_jail_pop / total_pop_15to64) %>%
  summarize(avg_percent_female = mean(percent_female)) %>%
  pull(avg_percent_female)

# Average percentage of male population that were in jail in 2000
avg_male_2000 <- incarceration_df %>%
  filter(
    year == 2000,
    !is.na(total_pop_15to64),
    !is.na(male_jail_pop),
    !is.na(female_jail_pop)
  ) %>%
  mutate(percent_male = male_jail_pop / total_pop_15to64) %>%
  summarize(avg_percent_male = mean(percent_male)) %>%
  pull(avg_percent_male)

# Ratio (male : female) of prisoners in 2018
ratio_2018 <- incarceration_df %>%
  filter(
    year == 2018,
    !is.na(male_jail_pop),
    !is.na(female_jail_pop)
  ) %>%
  summarize(ratio = sum(male_jail_pop) / sum(female_jail_pop)) %>%
  pull(ratio)

# Ratio (male : female) of prisoners in 2000
ratio_2000 <- incarceration_df %>%
  filter(
    year == 2000,
    !is.na(male_jail_pop),
    !is.na(female_jail_pop)
  ) %>%
  summarize(ratio = sum(male_jail_pop) / sum(female_jail_pop)) %>%
  pull(ratio)

# Ratio (male : female) of prisoners in 1980
ratio_1980 <- incarceration_df %>%
  filter(
    year == 1980,
    !is.na(male_jail_pop),
    !is.na(female_jail_pop)
  ) %>%
  summarize(ratio = sum(male_jail_pop) / sum(female_jail_pop)) %>%
  pull(ratio)

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#
# This function gets the total number of incarcerated people every year.
get_year_jail_pop <- function() {
  res <- incarceration_df %>%
    group_by(year) %>%
    summarize(sum_jail_pop = sum(total_jail_pop, na.rm = TRUE))
  return(res)
}

# This function plots the total number of incarcerated people every year as
# a bar chart.
plot_jail_pop_for_us <- function() {
  chart <- ggplot(data = get_year_jail_pop()) +
    
    # bar chart that maps year to sum_jail_pop
    geom_col(mapping = aes(x = year, y = sum_jail_pop)) +
    
    # add labels
    labs(x = "Year", y = "Total Jail Population",
         title = "Increase of Jail Population in U.S. (1970-2018)",
         caption = "The trends in the graph show the increase of U.S.
         jail populations from 1970-2018.") +
    
    # remove scientific notation
    scale_y_continuous(labels = scales::label_comma())
  return(chart)
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population in Specified States
#----------------------------------------------------------------------------#
# This function gets the total number of incarcerated people every year in
# the specified states.
# states - a vector containing state abbreviations (i.e. c("WA","OR","CA"))
get_jail_pop_by_states <- function(states) {
  res <- incarceration_df %>%
    filter(state %in% states) %>%
    group_by(state, year) %>%
    summarize(sum_jail_pop = sum(total_jail_pop, na.rm = TRUE), .groups = "keep")
  return(res)
}

# This function plots the total number of incarcerated people every year in
# the specified states.
# states - a vector containing state abbreviations (i.e. c("WA","OR","CA"))
plot_jail_pop_by_states <- function(states) {
  chart <- ggplot(data = get_jail_pop_by_states(states)) +
    
    # line chart that maps year to sum_jail_pop, color encoding states
    geom_line(mapping = aes(x = year, y = sum_jail_pop, color = state)) +
    
    # add labels
    labs(x = "Year", y = "Total Jail Population",
         title = "Change in Jail Population in U.S. States (1970-2018)",
         caption = "The trends in the graph show the increase of the jail populations
         in the Pacific Northwest (WA, ID, OR) from 1970-2018.") +
    
    # remove scientific notation
    scale_y_continuous(labels = scales::label_comma())
  return(chart)
}

## Section 5  ---- 
#----------------------------------------------------------------------------#
# Incarcerated population by gender over the years.
#----------------------------------------------------------------------------#
# This function returns the total sum of incarcerated people based on gender
# for every year in the dataset. It negates the values for female totals for
# plotting it in the negative direction later.
get_year_gender_jail_pop <- function() {
  res <- incarceration_df %>%
    group_by(year) %>%
    summarize(
      sum_female_jail_pop = -sum(female_jail_pop, na.rm = TRUE),
      sum_male_jail_pop = sum(male_jail_pop, na.rm = TRUE)
    )
  return(res)
}

# This function returns a figure that graphs total incarcerated population
# split by gender as a color encoding and with year as the x-axis. It uses
# the relative barmode for mapping female numbers in the negative direction.
plot_year_gender_jail_pop <- function() {
  data <- get_year_gender_jail_pop()
  fig <- plot_ly(
    data = data,
    x = ~year,
    y = ~sum_female_jail_pop,
    type = "bar",
    name = "Female",
    marker = list(color = "rgb(98, 3, 252)")
  ) %>% add_trace(
    y = ~sum_male_jail_pop,
    name = "Male",
    marker = list(color = "rgb(2, 201, 198)")
  ) %>% layout(
    title = "Total Incarcerated Populations by Gender (1970-2018)",
    xaxis = list(title = "Year", range = list(1969, 2019)),
    yaxis = list(title = "Incarcerated Population"),
    barmode = "relative",
    margin = list(l = 0, r = 0, t = 50, b = 100),
    annotations = list(
      text = "The trends in the graph show the total incarcerated
      populations split by gender in the U.S. from 1970-2018.",
      xref = "paper", yref = "paper",
      xanchor = "right", yanchor = "bottom",
      x = 1, y = -0.2, showarrow = FALSE
    )
  )
  return(fig)
}

## Section 6  ---- 
#----------------------------------------------------------------------------#
# Incarcerated gender ratio based on year and state.
#----------------------------------------------------------------------------#
# This function returns county fips and percentage of female incarcerated
# people in jails from 1970 to 2018.
get_year_gender_jail_ratio <- function() {
  res <- incarceration_df %>%
    group_by(fips, year) %>%
    summarize(
      sum_jail_pop = sum(female_jail_pop, na.rm = TRUE) + sum(male_jail_pop, na.rm = TRUE),
      percent_female_jail = 100 * sum(female_jail_pop, na.rm = TRUE) / sum_jail_pop
    ) %>%
    ungroup() %>%
    select(-sum_jail_pop) %>%
    pivot_wider(names_from = year, values_from = percent_female_jail, names_prefix = "year_")
  return(res)
}

# This function returns a by-county choropleth of the percentage of female
# incarcerated people in jails in a selected year. Year can be selected
# using the dropdown menu.
map_year_gender_jail_ratio <- function() {
  # Get county fips data
  counties <- rjson::fromJSON(
    file="https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json"
  )
  
  data <- get_year_gender_jail_ratio()
  
  fig <- plot_ly() %>%
    add_trace(
      type = "choropleth",
      geojson = counties,
      locations = data$fips,
      z = data$year_1970, # Default to the 1970 data
      colorscale = "RdBu",
      zmin = 0, zmax = 100,
      marker = list(line = list(width = 0))
    ) %>%
    colorbar(title = "Female Jail Population (%)") %>%
    layout(
      title = "Percentage of Jail Population who are Female",
      geo = list(
        scope = "usa",
        projection = list(type = "albers usa"),
        bgcolor = "#bbb"
      ),
      
      # Add caption
      margin = list(l = 0, r = 0, t = 25, b=50),
      annotations = list(
        text = "The choropleth shows the gender ratio in incarcerated
          populations in counties in the U.S. from 1970 to 2018.",
        xref = "paper", yref = "paper",
        xanchor = "right", yanchor = "bottom",
        x = 1, y = 0, showarrow = FALSE
      ),
      
      # create drop down menu with every year as an option
      updatemenus = list(
        list(
          y = 0.7,
          buttons = lapply(
            X = paste0("year_", 1970:2018),
            FUN = function(x) {
              button <- list(
                method = "restyle",
                args = list('z', list(data[[x]])),
                label = x
                )
            }
          )
        )
      )
    )
  return(fig)
}
