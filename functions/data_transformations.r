
# Problem 2 ----
# Creating a function that transorms 
# Defining the function as acquired in the task
transform_metadata_to_df <-
# Beginning the functions and includes the data set that we use
  function(stations_metadata) {
# Reading the data set from the first entry
  stations_metadata[[1]] |>
# The data set is structured in a list. Using the map-function the transform it
# to tables
  map(as_tibble) |>
# Binding the tables together
  list_rbind() |>
# Editing latestData column to show the date time
  mutate(latestData = map_chr(latestData, 1, .default = "")) |>
# Editing to the time to UTC-format and choosing changing the timezone to 
# Europe and Berlin
  mutate(latestData = as_datetime(latestData, tz = "UTC")) |>
# Unlisting the location column to make it simplier
  mutate(location = map(location, unlist)) |>
# Adding a columns for latitude and longitude
  mutate(
    lat = map_dbl(location, "latLon.lat"),
    lon = map_dbl(location, "latLon.lon")
  ) |>
# Removing the location column from the data frame
  select(-location)
  }

# Problem 4a ----
# Creating a function that transform the date format into iso8601 format 
to_iso8601 <-
# Adding two arguments to the function
  function(input_data_time, offset_days) {
# Defining date_time as input date added with the number of offset days
    date_time = input_data_time + days(offset_days)
# Defining the iso8601 format
    iso8601_date <- format(date_time, format = "%Y-%m-%dT%H:%M:%SZ")
# Returning the date in the right format
    return(iso8601_date)
  }

# Modifying the dates in stations_metadata_df by using the formula to_iso8601
stations_metadata_df <-
# Finding the old data frame
  stations_metadata_df |>
# Modifying the latestdata column
  mutate(
    latestData = to_iso8601(latestData, 0)
  )

# Alternative method 
dateformat <- function(stations_metadata_df) {
  stations_metadata_df$latestData <- to_iso8601(stations_metadata_df$latestData, 0)
  return(stations_metadata_df)
}

# Adding the date format changes to stations_metadata_df
stations_metadata_df <- dateformat(stations_metadata_df)


# I couldnt manage to solve the prblems regarding volume. Will look closely to
# solution when it gets published.


# Query
query <- function(id, from, to){
  id = stations_metadata_df["97411V72313"],
  from = stations_metadata_df["2022-05-01T06:55:47Z"]
  to = stations_metadata_df["2022-05-08T06:55:47Z"]
  }
  


# Problem 5 ----

transform_volumes <- function(traffic_data) {
  volume_data <- traffic_data$trafficData$volume$byHour$edges
  
  df <- volume_data %>% 
    map(function(x) {
      tibble(
        from = force_tz(as_datetime(x$node$from), tzone = "UTC"),
        to = force_tz(as_datetime(x$node$to), tzone = "UTC"),
        volume = x$node$total$volumeNumbers$volume
      )
    }) %>% 
    list_rbind()
  
  return(df)
}














