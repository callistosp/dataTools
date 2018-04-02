#' Convert Clock Time to Numeric Time
#'
#' Clock times stored as character are converted into numeric hours for ease of
#' calculating time differences.
#'
#' This function was created to convert clock times to numeric times with
#' minutes converted to fractions of hours.
#'
#' @param vec Vector containing time values of format "HH:MM" stored as
#'   \code{character}.
#' @return Vector containing time values converted to fractional hour +
#'   minute/60 as \code{numeric}.
#' @examples
#' clockTimes <- c("10:06", "8:30", "12:15")
#' numericTime(clockTimes)
#'
#' @author Samuel Callisto \email{calli055@@umn.edu}
#' numericTime()

numericTime <- function(vec){
  sapply(strsplit(vec,":"),
         function(x) {
           x <- as.numeric(x)
           round(x[1]+x[2]/60,2)
         }
  )
}
