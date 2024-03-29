#' @rdname formatTwDate
#' @export
#'
#' @title
#' Converts from Twitter date format to R date format
#'
#' @param datestring Date string in Twitter format
#'
#' @param format Either "date", for Date format; or "datetime" for POSIX.
#'

formatTwDate <- function(datestring, format="datetime"){
    Sys.setlocale("LC_TIME", "English") # change locale settings
    if (format=="datetime"){
        date <- as.POSIXct(datestring, format="%a %b %d %H:%M:%S %z %Y")
    }
    if (format=="date"){
        date <- as.Date(datestring, format="%a %b %d %H:%M:%S %z %Y")
    }
    Sys.setlocale('LC_TIME',Sys.getlocale('LC_COLLATE')) # return to normal
    return(date)
}
