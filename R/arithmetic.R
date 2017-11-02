#' Create patient record
#'
#' \code{create_patient_record} takes a person’s full name (FirstName LastName), weight
#'   in lb and height in inches and converts it into a list with the person’s first
#'   name, person’s last name, weight in kg and height in m. There are 2.2 lb in a kg.
#'   There are 39.4 inches in a metre.
#' @param full_name A character string containing two names seperated by a space
#' @param height_in A number. The patients height in inches.
#' @param weight_lb A number. The patient's weight in pounds
#' @return A list of first_name, last_name, weight_kg and height_m
create_patient_record <- function(full_name, weight_lb, height_in) {
  name_list <- strsplit(full_name, split = " ")[[1]]
  first_name <- name_list[1]
  last_name <- name_list[2]
  weight_kg <- weight_lb / 2.2
  height_m <- height_in / 39.4

  list(first_name = first_name, last_name = last_name,
       weight_kg = weight_kg, height_m = height_m)
}
