#' Convert Clock Time to Time After Dose
#'
#' Short description.
#'
#' Long description.
#'
#' @param dataset data.frame containing columns listed
#' @param ID the column number contianing ID in ds
#' @param TIME the column number containing TIME in ds
#' @param DATE the column number containing DATE in ds
#' @return vector containing TAD values
#' @examples
#'
#'
#'
#' @author Samuel Callisto \email{calli055@@umn.edu} timeAfterDose()
#' @importFrom lubridate is.Date
#' @export

timeAfterDose <- function(dataset, ID, TIME, DATE){
  ## create local copy to work with
  ds <- dataset[,c(ID,TIME,DATE)]
  names(ds) <- c("SID", "TIME", "DATE")

  ## check if column in clock time or decimal time and convert if necessary
  if(length(grep(":",ds$TIME)) > 0){
    ## change clock time to decimal time
    ds$TIME <- numericTime(ds$TIME)
  }

  ## check if DATA column is in correct format
  if(!(is.Date(ds$DATE))){
    warning("Setting date using default format")
    ds$DATE <- as.Date(ds$DATE)
  }

  pxList <- unique(ds$SID)

  ## change decimal time to time after dose
  for(i in 1:length(pxList)){
    index <- ds$SID == pxList[i]
    ## pull minimum date for given patient
    minDate <- ds$DATE[index][which.min(ds$DATE[index])]
    ## pull minimum time for given patient
    minTime <- ds$TIME[index & ds$DATE == minDate][which.min(ds$TIME[index & ds$DATE == minDate])]
    ## add 24 hours for each day difference
    ds$TIME[index] <- 24*(ds$DATE[index]-minDate) + ds$TIME[index]
    ## subtract minTime from all instances of patient i
    ds$TIME[index] <- ds$TIME[index] - minTime
  }

  return(ds$TIME)
}


