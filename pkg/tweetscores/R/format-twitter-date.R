#' @rdname formatTwDate
#' @export
#'
#' @title
#' Converts from Twitter date format to R date format
#'
#' @author
#' Pablo Barbera \email{P.Barbera@@lse.ac.uk}
#'
#' @param datestring Date string in Twitter format
#'
#' @param format Either "date", for Date format; or "datetime" for POSIX.
#'

formatTwDate <- function(datestring, format="datetime"){
    if (format=="datetime"){
        date <- as.POSIXct(datestring, format="%a %b %d %H:%M:%S %z %Y")
    }
    if (format=="date"){
        date <- as.Date(datestring, format="%a %b %d %H:%M:%S %z %Y")
    }
    return(date)
}
