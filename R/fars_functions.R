#' Data Input
#' @description After checking if a file exist in a given path, the function read the
#'  file and creat a data frame table.
#'
#' @param filename .csv file from which the data will be read
#'
#' @return check if the file exists, gives error if it does not. if the file exists,
#'  it read the file in a data frame and returns it
#' @export
#'
#' @importFrom dplyr tbl_df %>%
#' @importFrom readr read_csv
#'
#' @examples
#' \dontrun{
#' fars_read(xyz.csv)
#' }
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Creat a file name from the data source for a given year
#'
#' @param year Only logical, integer, real and character vectors are supported. The
#'  year for which the file will be created
#'
#' @return read pram as integer. and return file name for a given year
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  make_filename(1999)
#'  make_filename("1999")
#'}
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Loads FARS data files for given year/s
#'
#' @param years a vector of years for which FARS data file will be loaded
#'
#' @return return NULL for an invalid year. For a valid input, returns list of
#' FARS data
#'
#' @export
#'
#' @importFrom dplyr mutate select %>%
#'
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#' fars_read_years(c(2001, 2005, 2007))
#' fars_read_years(c("2001", "2005", "2007"))
#'}
#'
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}



#' Monthly number of accident for given years in FARS data
#'
#' @param years a vector of years for which FARS data file will be loaded and number
#'  of accident will be summurized
#'
#' @return return NULL for an invalid year. For a valid input, returns list of
#' FARS data in data frame in years and number of accidents
#'
#' @export
#'
#' @importFrom dplyr bind_rows group_by summarize %>%
#'
#' @importFrom dplyr group_by
#'
#' @importFrom dplyr summarize
#'
#' @importFrom tidyr spread
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(c(2001, 2005, 2007))
#' fars_summarize_years(c("2001", "2005", "2007"))
#' }
#'
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}



#'  For a given state and given year, it draws the location of accidents
#'
#' @param year a vector of years for which FARS data file will be loaded
#' @param state.num an integer representing state number for which the location will be
#'        drawn
#'
#' @return if there is no accident a massage is produced. for invalid state number error
#'  is thrown. for valid input, plots a map with accident as dots on it.
#'
#' @export
#'
#' @importFrom dplyr filter %>%
#'
#' @importFrom maps map
#'
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{
#' fars_map_state (3, 2013)
#' fars_map_state (10, 2015)
#' }
#'
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
