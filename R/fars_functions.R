#' Read and print fars_data for a particular year.
#'
#' This is a function, that reads the input data frame. Firstly
#' it checks whether the data exists in the specified directory or NOT. If the data
#' exists, then it is printed out in Tibble format along with the variable names.
#'
#' @param Input class 'data.frame' in .csv format, .csv within ZIP format as argument.
#'
#' @return This function returns the input data file in Tibble format
#' along with the variable names in the data file.
#'
#' @importFrom dplyr tbl_df
#' @importFrom readr read_csv
#'
#' @examples
#' \dontrun{
#' fars_read("accident_2013.csv.bz2")
#' fars_read("accident_2015.csv")
#' }
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Make individual names of each data file for a particular year.
#'
#' This function creates individual file names for each data frame
#' with .csv format corresponding to a year.
#'
#' @param Input class 'integer' value of a 'year' as the one argument.
#'
#' @return This function simply returns the names of the data files
#' year wise.
#'
#' @examples
#' \dontrun{
#' make_filename(2013)
#' make_filename(2014)
#' }
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Make a list of occurances year and month wise.
#'
#' This function provides a list of occurances (accidents) in a year. For every occurance 
#' it adds a row in this list and records the corresponding MONTH and year in that
#' list for that particular occurance.
#'
#' @source This function uses the function make_filename() inside to get individual
#' output year wise.
#'
#' @param Input class 'integer' value(s) of 'year(s)' as the one argument.
#'
#' @return This function returns an output that contains one-to-one row for
#' every single occurance in a MONTH within a year. Each row contains only MONTH and year
#' number for every occurance. This function prints a Tibble format of the output.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @examples
#' \dontrun{
#' fars_read_years(2013)
#' fars_read_years(c(2013,2015))
#' }
#'
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

#' Make a list of cumulative number of occurances (accidents) in a MONTH of the year.
#'
#' This function calculates the total number of accidents happened in a MONTH within a
#' year. It yields a data frame and prints a Tibble format of the output.
#'
#' @param Input class 'integer' value(s) of year(s) as the one argument.
#'
#' @return This function returns a data frame that contains total number of occurances
#' (accidents) in a MONTH for the input year.
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr bind_rows
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2015)
#' fars_summarize_years(c(2013,2015))
#' }
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Make a plot for a particular state for a year.
#'
#' This function shows a geographical plot of 'dots' according to number of accidents
#' in a state within a year.
#'
#' @source This function uses the function make_filename() inside.
#'
#' @param Input class 'integer' value for the 'STATE number'
#' @param Input class 'integer' value for the 'year'
#'
#' @return This function returns a plot displaying density of accidents for a
#' certain state number in a particular year.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' 
#' @note The two packages 'stats' and 'maps' must be preloaded before compiling
#' this function. Otherwise there would be a situation to get an error as: "Error
#' in get(dbname) : object 'stateMapEnv' not found"
#'
#' @examples
#' \dontrun{
#' fars_map_state(1,2015)
#' fars_map_state(56,2013)
#' }
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
