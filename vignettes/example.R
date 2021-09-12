source(here::here("R/functions.R"))


zip_file <- "data/takeout-20210906T135921Z-001.zip"
file_index <- index_build(zip_file)

data_file <- index_find(file_index, 2021, 9)
timeline_data <- jsonlite::fromJSON(unz(zip_file, data_file))

places <- find_places(timeline_data)

work_visits <- places %>%
  filter(place_type == "TYPE_WORK")

summarise_daily_hours(work_visits) |> format_daily_hours()
summarise_total_hours(work_visits)
