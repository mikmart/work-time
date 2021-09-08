library(dplyr)
library(tidyr)
library(stringr)


zip_contents <- function(zip_file) {
  unzip(zip_file, list = TRUE)$Name
}

index_build <- function(zip_file) {
  paths <- zip_contents(zip_file)
  semantic_paths <- str_subset(paths, "/Semantic Location History/")

  semantic_df <- tibble(path = semantic_paths) %>%
    extract(path, into = c("year", "month"), "(\\d*)_(.*).json", remove = FALSE) %>%
    mutate(year = as.integer(year), month = match(month, toupper(month.name))) %>%
    arrange(desc(year), desc(month))

  semantic_df
}

index_find <- function(index, year, month) {
  index[["path"]][index$year == year & index$month == month]
}


find_places <- function(timeline_data) {
  timeline_data$timelineObjects$placeVisit %>%
    unpack(cols = c(location, duration)) %>%
    filter(!is.na(name) | !is.na(semanticType)) %>%
    transmute(
      place_name = name,
      place_type = semanticType,
      came = parse_timestamp(startTimestampMs),
      left = parse_timestamp(endTimestampMs),
      hours = diff_hours(came, left)
    )
}

parse_timestamp <- function(x) {
  vctrs::new_datetime(as.double(x) / 1000)
}

diff_hours <- function(start, end) {
  as.double(difftime(end, start, units = "hours"))
}


summarise_work_days <- function(places) {
  work_visits <- places %>%
    filter(place_type == "TYPE_WORK")

  work_visits %>%
    group_by(day = as.Date(came), .add = TRUE) %>%
    summarise(
      came = first(came),
      left = last(left),
      hours = diff_hours(came, left)
    )
}

format_work_days <- function(data) {
  data %>%
    transmute(
      # week = strftime(day, "%V"),
      day = strftime(day, "%A, %d %B"),
      across(c(came, left), strftime, "%R"),
      hours = round(hours, 2)
    )
}

summarise_work_time <- function(places) {
  summarise_work_days(places) %>%
    summarise(
      total_days = n(),
      total_hours = sum(hours),
      average_hours = total_hours / total_days
    )
}
