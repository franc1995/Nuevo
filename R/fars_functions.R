#' Reads a FARS file
#'
#' Reads data from the US National Highway Traffic Safety Administration's
#' \href{http://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}{Fatality Analysis Reporting System} and stores it into a table.
#'
#' It returns and error in case the file does not exist
#'
#' @param filename Name of the FARS file to be read. It may be a plain or a compressed file.
#'
#' @return A tbl_df with the information in the file
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#'
#' # Read data from 2013, in plain csv format
#' a <- fars_read('accident_2013.csv')
#'
#' # Read data from 2014, with bz2-compressed csv format
#' b <- fars_read('accident_2014.csv.bz2)
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Builds the name of a FARS file
#'
#' Builds the file name of a FARS file. The file name is constructed based on the year
#'
#' @param year The year to build the file name
#'
#' @return The name of the FARS file containg the data
#'
#' @examples
#'
#' # Make the filename for year 2001
#' make_filename(2001)
#'
#' # Make the filename for year 2016
#' make_filename(2016)
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read FARS info for a list of years
#'
#' Reads the FARS info for the list of years passed as parameter, returning them as a list of tables.
#'
#' In case of error reading the data for one year, it will give a warning and continue with the rest.
#'
#' @param years List or numeric vector of years to read data from
#'
#' @return A list of tbl_df with the information per year
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr "%>%"
#' @examples
#'
#' # Reads the data from 2013 to 2015
#' fars_read_years(2013:2015)
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

#' Gets the number of accident per month and year
#'
#' Reads data FARS data and calculate the number of accidents per month
#'
#' @param years List of vector of years to read data from
#'
#' @return A table with one row per month and one column per year. The value of the MONTH column is the month.
#' The value of the rest of the columns is the number of accident in the year given by the column name.
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @importFrom magrittr "%>%"
#'
#' @examples
#'
#' # Reads and summarize data from 2013 to 2015
#' fars_summarize_years(2013:2015)
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Plots the accidentes within a state
#'
#' Plots the aciddent locations for a selected state and year
#'
#' In case an invalid state is selected, the function throws and error. In case there
#' are no accidents to plot, it shows a warning and no plot is created.
#'
#' @param state.num The number of state to plot
#' @param year The year to plot
#'
#'
#' @importFrom dplyr filter group_by summarize
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#'
#' fars_map_state(1, 2014)
#'
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
