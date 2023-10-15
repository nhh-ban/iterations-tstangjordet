# This file contains tests to be applied to 
# the Vegvesen stations-data *after* being transformed
# to a data frame. 
# 
# All tests are packed in a function test_stations_metadata that apples
# all the aforementioned tests

test_stations_metadata_colnames <-
  function(df) {
# Defining a vector with all the expected column names
    expected_colnames <- c("id", "name", "latestData", "lat", "lon")
# Using a if-function to check if the column names is equal to the expected 
# column names. If something wrong, it prints a fail-message  
    if (all(colnames(df) == expected_colnames) == TRUE) {
      print("PASS: Data has the correct columns")
    } else{
      print("FAIL: Columns do not match the correct specification")
    }
  }

test_stations_metadata_nrows <-
  function(df) {
# Defining two variables. The expected minimum and maximum values in the data
# data frame that we are testing
    min_expected_rows <- 5000
    max_expected_rows <- 10000
# Using a if-function to check if the number of ros are within the expected 
# minimum and maximum values. If it's not, it prints a message to say if it's 
# too many or too few rows.
    if (nrow(df) > min_expected_rows & nrow(df) < max_expected_rows) {
      print("PASS: Data has a reasonable number of rows")
    } else if (nrow(df) <= min_expected_rows) {
      print("FAIL: Data has suspiciously few rows")
    } else {
      print("FAIL: Data has suspiciously many rows")
    }
  }

test_stations_metadata_coltypes <-
  function(df) {
    expected_coltypes <-
      c("character", "character", "double", "double", "double")
# Using a if-function to check if the data types in the data frame is as the
# expected data types. If it fails it prints a fail-message
    if (all(df %>%
            map_chr( ~ typeof(.)) == expected_coltypes) == TRUE) {
      print("PASS: All cols have the correct specifications")
    } else{
      print("FAIL: Columns do not have the correct specification")
    }
  }
  
test_stations_metadata_nmissing <-
  function(df) {
    max_miss_vals <- 200
# Using if-function to check whether the data frame has too many missing values
    if (df %>% map_int( ~ sum(is.na((.)))) %>% sum(.) < max_miss_vals) {
      print("PASS: Amount of missing values is reasonable")
    } else {
      print("FAIL: Too many missing values in data set")
    }
  }

test_stations_metadata_latestdata_timezone <-
  function(df) {
# Checking if the timezone oin the column latestData are modified to UTC    
    if (attr(df$latestData,"tzone")=="UTC") {
      print("PASS: latestData has UTC-time zone")
    } else {
      print("FAIL: latestData does not have expected UTC-time zone")
    }
  }

# Gathering the test results into one data frame
test_stations_metadata <- 
  function(df){
    test_stations_metadata_colnames(df)
    test_stations_metadata_coltypes(df)
    test_stations_metadata_nmissing(df)
    test_stations_metadata_nrows(df)
    test_stations_metadata_latestdata_timezone(df)
  }





