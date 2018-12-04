#' Fars package code
#'
#' This supporting function reads a csv file.
#' if the input file doesn't exist execution will stop and
#' error will be thrown.
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @param filename A character string with the name of the file to read, see
#'   notes.
#'
#' @return A data frame with data readed from the csv file, or an error if the
#'   file does not exists.
#'
#' @author Federico Sensi and Coursera
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(readr)
#' yr <- 2015
#' data <- yr %>%
#'   make_filename %>%
#'   fars_read
#' View(data)
#' }
#' @note To generate file name use: \code{\link{make_filename}}
#' @seealso \link{make_filename}
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}


#' Make data file name
#'
#' Make .csv data file name related to the given \code{year}
#' The function does not check if the file is available.
#'
#' @param year A string or an integer with the input \code{year}
#'
#' @return This function returns a string with the data file name for a given
#'   year
#'
#' @author Federico Sensi and Coursera
#'
#' @examples
#'  \dontrun{
#' make_filename(2013)
#' # "accident_2013.csv.bz2"}
#' @seealso \link{fars_read}
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}


#' Read FARS years
#'
#' This  function loads the raw data for fars
#' @param years A vector with a list of years
#'
#' @importFrom dplyr mutate_
#' @importFrom dplyr select_
#' @importFrom magrittr "%>%"
#
#' @return A data.frame including entries in data by month, or NULL if the
#'  \code{year} is not valid
#'
#' @author Federico Sensi and Coursera
#'
#' @seealso \link{fars_read}
#' @seealso \link{make_filename}
#' @seealso \link{fars_summarize_years}
#' @examples
#' \dontrun{
#' fars_read_years(2013)
#' }
#' @export
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

#' Summarize FARS data by years
#'
#' This function summarizes yearly accidents data by month
#' @param years A vector with a list of years to summarize by.
#'
#' @return A data.frame with number of accidents by years summarized by month
#'
#' @author Federico Sensi and Coursera
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by_
#' @importFrom dplyr summarize_
#' @importFrom tidyr spread_
#' @importFrom magrittr "%>%"
#' @seealso \link{fars_read_years}
#' @examples
#' \dontrun{
#' plot(fars_summarize_years(2015))
#' fars_summarize_years(c(2015, 2014))
#' }
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Display accidents map by state and year
#'
#' Creates a plot with a state map including the accidents location by year
#' If the \code{state.num} is invalid the function shows an error
#' @param state.num An Integer with the State Code
#' @param year A string, or an integer, with the input \code{year}
#'
#' @importFrom maps map
#' @importFrom dplyr filter_
#' @importFrom graphics points
#' @return None
#'
#' @author Federico Sensi and Coursera
#'
#' @seealso \link{fars_read}
#' @seealso \link{make_filename}
#' @export
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
