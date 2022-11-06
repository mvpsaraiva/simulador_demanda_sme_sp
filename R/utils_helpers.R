#' helpers
#'
#' @description Reads a table from the SQLite database and converts it to SF.
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

read_sf_from_db <- function(con, table) {
  # read table from db
  data_df <- DBI::dbReadTable(con, table)

  # convert geometry column from WKB format
  wkb <- structure(data_df$geometry, class = "WKB")
  data_df$geometry <- sf::st_as_sfc(wkb, EWKB = TRUE, crs = 4326)

  # put geometry back into table
  data_df <- sf::st_as_sf(data_df)

  return(data_df)
}
