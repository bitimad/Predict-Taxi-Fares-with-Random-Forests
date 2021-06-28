
# Loading the tidyverse
library(tidyverse)

# Reading in the taxi data
taxi <- read_csv('datasets/taxi.csv')

# Taking a look at the first few rows in taxi
head(taxi)

library(testthat) 
library(IRkernel.testthat)

run_tests({
    test_that("Test that tidyverse is loaded", {
        expect_true( "package:tidyverse" %in% search(), 
            info = "The tidyverse package should be loaded using library().")
    })
    
    test_that("Read in data correctly.", {
        expect_is(taxi, "tbl_df", 
            info = 'You should use read_csv (with an underscore) to read "datasets/taxi.csv" into taxi.')
    })
    
    test_that("Read in data correctly.", {
        taxi_temp <- read_csv('datasets/taxi.csv')
        expect_equivalent(taxi, taxi_temp, 
            info = 'taxi should contain the data in "datasets/taxi.csv".')
    })
})

# Renaming the location variables,
# dropping any journeys with zero fares and zero tips,
# and creating the total variable as the log sum of fare and tip
taxi <- taxi %>%
   rename(long = pickup_longitude, lat = pickup_latitude)  %>% 
   filter(fare_amount > 0 | tip_amount > 0) %>%
   mutate(total = log(fare_amount + tip_amount) )

run_tests({
    test_that("rename lat", {
        expect_true(!is.null(taxi$lat), 
            info = "The taxi data frame does not contain a variable called lat. You need to rename pickup_latitude.")
    })
    test_that("rename long", {
        expect_true(!is.null(taxi$long), 
            info = "The taxi data frame does not contain a variable called long. You need to rename pickup_longitude.")
    })
    test_that("total exists", {
        expect_true(!is.null(taxi$total), 
            info = "The taxi data frame does not contain a variable called total. You need to create this as the logarithm (use the log() function) of the sum of fare_amount and tip_amount.")
    })
    test_that("Modified data correctly.", {
        taxi_temp <- read_csv('datasets/taxi.csv') %>%
            rename(long = pickup_longitude, lat = pickup_latitude)  %>% 
            filter(fare_amount > 0 | tip_amount > 0) %>%
            mutate(total = log(fare_amount + tip_amount) )
        expect_equivalent(taxi, taxi_temp, 
            info = 'The taxi dataframe has not been modified correctly. See if you can find something is wrong with your code.')
    })
})


# Reducing the data to taxi trips starting in Manhattan
# Manhattan is bounded by the rectangle with 
# latitude from 40.70 to 40.83 and 
# longitude from -74.025 to -73.93
taxi <- taxi  %>% 
    filter(between(lat,  40.70, 40.83) & 
           between(long, -74.025, -73.93))

run_tests({
  test_that("The correct number of rows have been filtered away", {
      expect_equal(45766, nrow(taxi), 
      info = "It seems you haven't filter away the taxi trips outside of Manhattan correctly.")
  })
})

# Loading in ggmap and viridis for nice colors
library(ggmap)
library(viridis)

# Retrieving a stored map object which originally was created by
# nycmap <- get_map("manhattan", zoom = 12, color = "bw")
manhattan <- readRDS("datasets/manhattan.rds")

# Drawing a density map with the number of journey start locations
ggmap(manhattan, darken=0.5)+
    scale_fill_viridis(option='plasma') +
    geom_bin2d(data = taxi, aes(x = long, y = lat), bins = 60, alpha = 0.6) +
    labs(x = 'Longitude', y = 'Latitude', fill = 'Journeys')

run_tests({
    
    test_that("Test that ggmap is loaded", {
        expect_true( "package:ggmap" %in% search(), 
            info = "The ggmap package should be loaded using library().")
    })
    test_that("Test that viridis is loaded", {
        expect_true( "package:viridis" %in% search(), 
            info = "The viridis package should be loaded using library().")
    })
    
    test_that("Check that geom_bin2d was used", {
        p <- last_plot()
        stat_classes <- as.character(sapply(p$layers, function(layer) {
            class(layer$stat)
        }))

        expect_true("StatBin2d" %in% stat_classes, 
            info = "You need to use geom_bin2d correctly to draw the map.")
    })
})


# Loading in the tree package
library(tree)

# Fitting a tree to lat and long
fitted_tree <- tree(total ~ lat + long, data = taxi)

# draw a diagram of the tree structure
plot(fitted_tree)
text(fitted_tree)

run_tests({
    test_that("Test that tree is loaded", {
        expect_true( "package:tree" %in% search(), 
            info = "The tree package should be loaded using library().")
    })
  test_that("The tree has been fitted correctly", {
      correctly_fitted_tree <- tree(total ~ lat + long, data = taxi)
      expect_equivalent(fitted_tree, correctly_fitted_tree, 
      info = "It seem you didn't fit the tree correctly. Check the hint, it might help!")
  })
})


# Loading in the lubridate package
library(lubridate)

# Generate the three new time variables
taxi <- taxi %>% 
    mutate(hour = hour(pickup_datetime),
           wday = wday(pickup_datetime, label = TRUE),
           month = month(pickup_datetime, label = TRUE))

run_tests({
    test_that("Test that lubridate is loaded", {
        expect_true( "package:lubridate" %in% search(), 
            info = "The lubridate package should be loaded using library().")
    })
    test_that("hour is correct", {
        expect_equivalent(taxi$hour[1], 10L, 
            info = "The `hour` column doesn't seem to be correct. Check the hint for more help.")
    })
    test_that("wday is correct", {
        expect_true(taxi$wday[1] == "Sun", 
            info = "The `wday` column doesn't seem to be correct. Check the hint for more help.")
    })
    test_that("month is correct", {
        expect_true(taxi$month[1] == "Jan", 
            info = "The `month` column doesn't seem to be correct. Check the hint for more help.")
    })
})

# Fitting a tree to lat and long
fitted_tree <- tree(total ~ lat + long + hour + wday + month, data = taxi)

# draw a diagram of the tree structure
plot(fitted_tree)
text(fitted_tree)

# Summarizing the performance of the tree
summary(fitted_tree)

run_tests({
  test_that("The tree has been fitted correctly", {
      correctly_fitted_tree <- tree(total ~ lat + long + hour + wday + month, data = taxi)
      expect_equivalent(fitted_tree, correctly_fitted_tree, 
      info = "It seem you didn't fit the tree correctly. Check the hint, it might help!")
  })
})

# Loading in the randomForest package
library(randomForest)

# Fitting a random forest
fitted_forest <- randomForest(total ~ lat + long + hour + wday + month,
    data=taxi, ntree=80, sampsize=10000)

# Printing the fitted_forest object
fitted_forest

run_tests({
    test_that("Test that randomForest is loaded", {
        expect_true( "package:randomForest" %in% search(), 
            info = "The randomForest package should be loaded using library().")
    })
    test_that("ntree is correct.", {
        expect_true(fitted_forest$ntree == 80, 
            info = "The ntree argument to randomForest should be ntree = 80 .")
    })
    test_that("Check randomForest call was ok", {
        call_string <- paste(deparse(fitted_forest$call), collapse = " ")
        keywords <- c("total", "lat", "long", "hour", "wday", "month",
                      "ntree", "sampsize", "100")
        expect_true(all(str_detect(call_string, keywords)), 
            info = "You have not called randomForest correctly. Did you include all the predictors and the right output variable?.")
    })
})

# Extracting the prediction from forest_fit
taxi$pred_total <- fitted_forest$predicted

# Plotting the predicted mean trip prices from according to the random forest
ggmap(manhattan, darken=0.5) +
    scale_fill_viridis(option = 'plasma') +
    stat_summary_2d(data=taxi, aes(x = long, y = lat, z = pred_total),
                    fun = mean, alpha = 0.6, bins = 60) +
    labs(x = 'Longitude', y = 'Latitude', fill = 'Log fare+tip')

run_tests({
    test_that("taxi$pred_total == fitted_forest$predicted", {
        expect_true(all(taxi$pred_total == fitted_forest$predicted), 
            info = "You should assign fitted_forest$predicted to taxi$pred_total .")
    })
    test_that("Check that stat_summary_2d was used", {
        p <- last_plot()
        stat_classes <- as.character(sapply(p$layers, function(layer) {
            class(layer$stat)
        }))

        expect_true("StatSummary2d" %in% stat_classes, 
            info = "You need to use geom_bin2d correctly to draw the map.")
    })
    test_that("Check that pred_total was used", {
        p <- last_plot()
        p_variables <- unlist(sapply(p$layers, function(layer) {
            as.character(layer$mapping)
        }))
        expect_true(any(str_detect(p_variables, "pred_total")), 
            info = "You need to connect pred_total to z in the aes() call correctly.")
    })
})


# Function that returns the mean *if* there are 15 or more datapoints
mean_if_enough_data <- function(x) { 
    ifelse( length(x) >= 15, mean(x), NA) 
}

# Plotting the mean trip prices from the data
ggmap(manhattan, darken=0.5) +
    stat_summary_2d(data=taxi, aes(x = long, y = lat, z = total),
                    fun = mean_if_enough_data,
                    alpha = 0.6, bins = 60) +
  scale_fill_viridis(option = 'plasma') +
  labs(x = 'Longitude', y = 'Latitude', fill = 'Log fare+tip')

run_tests({
    test_that("Check that total was used but not pred_total", {
        p <- last_plot()
        p_variables <- unlist(sapply(p$layers, function(layer) {
            as.character(layer$mapping)
        }))
        expect_true(any(str_detect(p_variables, "total")) & 
                   !any(str_detect(p_variables, "pred_total")), 
            info = "You need to connect total to z in the aes() call correctly. Make sure you are not still using pred_total.")
    })
})

# Where are people spending the most on their taxi trips?
spends_most_on_trips <- "downtown" # "uptown" or "downtown"

run_tests({
  test_that("...", {
      expect_true(str_detect(tolower(spends_most_on_trips), "downtown"), 
      info = "Well, looking at the plot it looks like people pay more downtown.")
  })
})
