#' @rdname format.facebook.date
#' @export
#'
#' @title
#' Converts from Facebook date format to R date format
#'
#' @param datestring Date string in Facebook format
#'
#' @param format Either "date", for Date format; or "datetime" for POSIX.
#'
#' @examples \dontrun{
#' ## example of Facebook data
#'   example <- "2014-02-21T16:22:03+0000"
#'   dt <- format.facebook.date(example)
#' }
#'

formatFbDate <- function(datestring, format="datetime") {
    if (format=="datetime"){
        date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
    }
    if (format=="date"){
        date <- as.Date(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
    }
    return(date)
}
