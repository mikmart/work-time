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
      lat = latitudeE7 / 1e7,
      lon = longitudeE7 / 1e7,
      tz = find_tz(lat, lon),
      came = parse_timestamp(startTimestampMs),
      left = parse_timestamp(endTimestampMs),
      hours = hours_stayed(came, left)
    )
}

find_tz <- function(lat, lon) {
  lutz::tz_lookup_coords(lat, lon, method = "fast", warn = FALSE)
}

parse_timestamp <- function(x) {
  vctrs::new_datetime(as.double(x) / 1000)
}

hours_stayed <- function(start, end) {
  as.double(difftime(end, start, units = "hours"))
}


summarise_daily_hours <- function(places) {
  places %>%
    group_by(day = as.Date(came), .add = TRUE) %>%
    summarise(
      tz = first(tz),
      came = first(came),
      left = last(left),
      hours = hours_stayed(came, left)
    )
}

format_daily_hours <- function(data) {
  data %>%
    rowwise() %>%
    transmute(
      day = strftime(day, "%A, %d %B", tz = tz),
      came = strftime(came, "%R", tz = tz),
      left = strftime(left, "%R", tz = tz),
      hours = round(hours, 2)
    )
}

summarise_total_hours <- function(places) {
  summarise_daily_hours(places) %>%
    summarise(
      total_days = n(),
      total_hours = sum(hours),
      average_hours = total_hours / total_days
    )
}


tz_choices <- function() {
  zones <- clock::tzdb_names()
  offset <- tz_offset(zones)
  names(zones) <- sprintf("UTC%s (%s)", format_offset(offset), zones)
  zones[order(offset)]
}

tz_offset <- function(tzone, time = clock::sys_time_now()) {
  as.double(clock::sys_time_info(clock::as_sys_time(time), tzone)$offset)
}

format_offset <- function(x) {
  sprintf("%+03d:%02d", x %/% 3600, x %% 3600 %/% 60)
}
