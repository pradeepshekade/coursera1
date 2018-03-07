#' @title Function to read a CSV file and create a data frame.
#' @description This function reads a csv file and creates a data frame.
#' This function requires packages - deplyr and readr.
#' If the file name is not present it errors out with a message
#' @param filename Complete path to a CSV file.
#' @examples
#' dat <- fars_read("data/accident_2013.csv")
#' @return Data Frame
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' @title Function to return a filename for a given year
#' @description  This function returns an appropriate filename with ".csv.bz2" extension.
#' @param year The 4 digit year for the filename to be created.
#' @examples
#' make_filename(2013) outputs the filename as [1] "accident_2013.csv.bz2"
#' @return File name
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' @title Read CSV file for a given year  and build a data frame
#' @description Reads the appropriate .csv.bz2 file for a given year and builds a data frame with two columns - Month and Year selected from the CSV.
#' If the year is invalid, it outpus a warning saying "invalid year" and does not produce any output
#' @param years A vector of years.
#' @examples
#' fars_read_years(c(2013,2014,2015)) will create data frames for each of the years
#' @return A data frame (with columns MONTH and Year) for each of the years.
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

#' @title Summarize the number of accidents by year
#' @description This function returns a summary data frame of number of accidents per month for a given vector of years.
#'
#' @param years A vector of years.
#' @examples
#' fars_summarize_years(c(2013,2014,2015)) creates a data frame with the columns - MONTH, `2013`, `2014`, and `2015`
#' @return summary data frame of number of accidents per month for a given vector of years

fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' @title Plot data on a map of a state for a given year
#' @description This function plots a map indicating the points where the accidents have been reported.
#' If the state number provided is not in the data, it outputs an error. Similarly if the year is not present
#' or there is no data to plot, the function terminates with a message "no accidents to plot"
#' @param state.num State number.
#' @param year Year.
#' @return A map of the state with the data plotted
#' @examples
#' fars_map_state(6,2013)
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
