library(data.table)
library(dplyr)
library(ggplot2)

#Function

## Question: Edit the function mentioned in class so that it will accept any of the offence_descriptions found in Offence Level 3 and will accept a 2-element vector of suburbs.

#' Correlation Finder
#' Finds the correlation between the offence count in two suburbs
#' \code{adelaide_Offence_level_3} The functions visualizes the correlation in offence count between the two input suburbs for a level 3 offence.
#' @param crime_data A data.table object with the following columns:
#'     "date" (POSIXct), "suburb" (chr), "postcode" (chr), "offence_level_1" (chr),
#'     "offence_level_2" (chr), "offence_level_3" (chr), "offence_count" (num).
#' @param offence_description A character string of type offence_level_3.
#' @param suburbs A two-element character vector. Each element is the name (UPPERCASE)
#'     of an SA suburb.
#' @export
#' @return  A ggplot object showing the correlation in offence count between the two input suburbs.
#' @examples
#' adelaide_offence_level_3(crime_2012,"OFFENCES AGAINST PROPERTY", vec)
adelaide_offence_level_3 <- function(crime_data, offence_description, suburbs) {
  require(data.table)
  require(ggplot2)

  # Error catching

  if (length(suburbs)!=2) {
    stop("Please enter two suburbs")
  }

  expected_colnames <- c("date", "suburb", "postcode", "offence_level_1", "offence_level_2",
                         "offence_level_3", "offence_count")

  input_colnames <- names(crime_data)

  if (!all.equal(expected_colnames, input_colnames)) {
    print("not matching")
    stop(paste("Input table columns need to match: ",
               paste(expected_colnames, collapse = ", ")))
  }


  # Check that the input suburbs and offence description exist in crime_data
  if (any(!suburbs %in% crime_data$suburb) |
      !offence_description %in% crime_data$offence_level_3) {
    stop("The input data does not exist in the data table")
  }

  # Make a data table for plotting using data.table transformations
  # You will need to filter, summarise and group by
  # Expect cols: "date", "suburb", "total_offence_count"
  plot_data <- crime_data[suburb %in% c(suburbs[1], suburbs[2]) &  offence_level_3 == offence_description,
                          list(total_offence_count = sum(offence_count),suburb),
                          by = date]

  # These lines will transform the plot_data structure to allow us to plot
  # correlations. Try them out
  plot_data[, suburb := plyr::mapvalues(suburb, suburbs, c("x", "y"))]

  plot_data <- dcast(plot_data, date ~ suburb, fun = sum,
                     fill = 0, value.var = "total_offence_count")

  # Generate the plot

  ggplot(plot_data, aes(x,y, group = month(date))) +
    geom_count() +
    labs(x = suburbs[1],
         y = suburbs[2])
}
