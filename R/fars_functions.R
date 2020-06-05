#' Read the FARS file
#'
#' @description This function will search for the input filename in the current directory, if the file exist,
#'              it will read in the file (.csv format) and store the file to current directory;
#'              or return error if the file does not exist.
#'
#' @usage fars_read(filename)
#'
#' @param filename a string indicates the name of the file. Can use \code{make_filename} to generate a filename.
#'
#' @return This function returns a dataframe if the selected file exist;
#'         return an error message "file ... does not exist" if the file not exist.
#'
#' @source Data source: US National Highway Traffic Safety Administration's Fatality Analysis Reporting System (FARS),
#'         which is a nationwide census providing the American public yearly data regarding fatal injuries suffered in motor vehicle traffic crashes.
#'         For more information, please see \url{https://crashstats.nhtsa.dot.gov/#/DocumentTypeList/23}.
#'
#' @examples \dontrun{fars_read("accident_2013.csv.bz2")}
#'
#' @importFrom readr read_csv
#' @import dplyr
#'
#' @note To generate a file name, use \code{make_filename}.
#'
#' @seealso \code{\link{make_filename}}
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data) # create a dataframe
}

#' Create a filename
#'
#' @description This function will create a .csv filename with input year.
#'
#' @usage make_filename(year)
#'
#' @param year an integer or string indicating the input year.
#'
#' @return This function will return the filname as a string format using the input year: accident_(year).csv.bz2"
#'
#' @examples make_filename(2013) ## output "accident_2013.csv.bz2"
#'
#' @note This function will only create a filename, to read in the file, use \code{fars_read}.
#'
#' @seealso \code{\link{fars_read}}
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read the FARS data with given year
#'
#' @description This function will search and read the FARS files in the current directory with the given years, then
#'              return dataframe with the given years and month of the fatal injuries incidents (long format, each year is a seperate dataframe),
#'              or NULL if the file not exist.
#'
#' @usage fars_read_years(years)
#'
#' @param years a vector with list of years.
#'
#' @return If the given year exist, return a dataframe with the given year and month of the fatal injuries incidents (long format);
#'         if the file does not exist, return NULL with warning message "invalid year: ..."
#'
#' @source Data source: US National Highway Traffic Safety Administration's Fatality Analysis Reporting System (FARS),
#'         which is a nationwide census providing the American public yearly data regarding fatal injuries suffered in motor vehicle traffic crashes.
#'
#' @examples \dontrun{fars_read_years(2013)}
#' @examples \dontrun{fars_read_years(c(2013, 2014))}
#'
#' @import dplyr
#'
#' @seealso \code{\link{fars_read}}
#' @seealso \code{\link{make_filename}}
#'
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(.data$MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}



#' Summarize FARS data by year
#'
#' @description This function will summarize the number of injuries counts per month for the given years,
#'              or return warning if the data with given year does not exist.
#'
#' @usage fars_summarize_years(years)
#'
#' @param years a vector with list of years.
#'
#' @return A summarize table with the given year and number of injuries counts per month (in wide format);
#'         if the data with given year does not exist, return NULL with warning message "invalid year: (given_year)".
#'
#' @source Data source: US National Highway Traffic Safety Administration's Fatality Analysis Reporting System (FARS),
#'         which is a nationwide census providing the American public yearly data regarding fatal injuries suffered in motor vehicle traffic crashes.
#'
#' @examples \dontrun{fars_summarize_years(2013)},
#' @examples \dontrun{fars_summarize_years(c(2013, 2014))}
#'
#' @import dplyr
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(.data$year, .data$MONTH) %>%
                dplyr::summarize(n = n()) %>% # number of injuries per month
                tidyr::spread(.data$year, n)
}

#' Map displaying location of fatal injuries for selected state and year
#'
#' @description This function will display a map of fatal injuries location for selected state and year.
#'
#' @usage fars_map_state(state.num, year)
#'
#' @param state.num a interger or string indicating state code, please see notes.
#' @param year a interger or string indicating year.
#'
#' @return A map showing location of fatal injuries for selected state and year.
#'         If the state code (\code{state.num}) does not exist, return error message "invalid STATE number".
#'         If the file for given year does not exsit, return error message "file ... does not exist".
#'
#' @source Data source: US National Highway Traffic Safety Administration's Fatality Analysis Reporting System (FARS),
#'         which is a nationwide census providing the American public yearly data regarding fatal injuries suffered in motor vehicle traffic crashes.
#'
#' @note State code:
#'   01: Alabama,
#'   02: Alaska,
#'   04: Arizona,
#'   05: Arkansas,
#'   06: California,
#'   08: Colorado,
#'   09: Connecticut,
#'   10: Delaware,
#'   11: District of Columbia,
#'   12: Florida,
#'   13: Georgia,
#'   15: Hawaii,
#'   16: Idaho,
#'   17: Illinois,
#'   18: Indiana,
#'   19: Iowa,
#'   20: Kansas,
#'   21: Kentucky,
#'   22: Louisiana,
#'   23: Maine,
#'   24: Maryland,
#'   25: Massachusetts,
#'   26: Michigan,
#'   27: Minnesota,
#'   28: Mississippi,
#'   29: Missouri,
#'   30: Montana,
#'   31: Nebraska,
#'   32: Nevada,
#'   33: New Hampshire,
#'   34: New Jersey,
#'   35: New Mexico,
#'   36: New York,
#'   37: North Carolina,
#'   38: North Dakota,
#'   39: Ohio,
#'   40: Oklahoma,
#'   41: Oregon,
#'   42: Pennsylvania,
#'   43: Puerto Rico,
#'   44: Rhode Island,
#'   45: South Carolina,
#'   46: South Dakota,
#'   47: Tennessee,
#'   48: Texas,
#'   49: Utah,
#'   50: Vermont,
#'   51: Virginia,
#'   52: Virgin Islands,
#'   53: Washington,
#'   54: West Virginia,
#'   55: Wisconsin,
#'   56: Wyoming.
#'
#' @examples \dontrun{fars_map_state(40, 2013)}
#'
#' @importFrom maps map
#' @importFrom graphics points
#' @import dplyr
#'
#' @seealso \code{\link{fars_summarize_years}}
#' @seealso  \code{\link{make_filename}}
#' @seealso  \code{\link{fars_read}}
#'
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, .data$STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE), # map: draw lines and polygons as specified by a map database.
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46) #points is a generic function to draw a sequence of points at the specified coordinates.
        })
}
