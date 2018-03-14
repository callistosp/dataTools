#' Merge by Date
#'
#' mergeByDate function adds new column to ds that contains VAR from nearest
#' past DATE for SID to each row and returns ds.
#'
#' Long description.
#'
#' @param ds Data.frame to be added to and returned.
#' @param ds2add Data to pull covariates from and merge into \code{ds}. The
#'   function assumes this data.frame has 2 columns named \code{SID} and
#'   \code{DATE} for merging, with a third column with the variable to be
#'   included.
#' @return Data.frame \code{ds} with an added column from \code{ds2add}.
#' @examples
#'
#'
#'
#' @author Samuel Callisto \email{calli055@@umn.edu}
#' @import lubridate  mergeByDate()

mergeByDate <- function(ds, ds2add){
  ## Assumption: ds2add has 3 named columns (SID, DATE, VAR)
  ## mergeByDate function adds new column to ds that contains
  ## VAR from nearest past DATE for SID to each row
  ## and returns ds

  ## check that DATE columns are Date datatype
  if(!(lubridate::is.Date(ds$DATE) && lubridate::is.Date(ds2add$DATE))){
    warning("Setting date using default format")
    ds$DATE <- as.Date(ds$DATE)
    ds2add$DATE <- as.Date(ds2add$DATE)
  }

  ## store column names for later
  tmpnames <- names(ds)
  ds$VAR <- NA
  for(i in 1:nrow(ds)){
    ## select only rows with matching SID
    tmpsubset <- subset(ds2add, SID==ds$SID[i])
    ## subtract date values
    dif <- ds$DATE[i] - tmpsubset$DATE
    ## only include positive values to pull VAR
    ## values measured on nearest PAST date (LOCF)
    onlyPos <- subset(dif, dif>=0)
    loc <- which.min(onlyPos)
    ## if no prior date exists in ds2add set
    ## just pull most recent value (forward in time)
    if(length(loc) == 0){loc <- which.min(abs(dif))}
    ## if no previous value recorded, put as NA
    if(length(loc) == 0){
      ds$VAR[i] <- NA
    }else{
      ds$VAR[i] <- tmpsubset[loc,3]
    }
  }
  ## change name of column to match ds2add
  names(ds) <- append(tmpnames, names(ds2add[3]))
  return(ds)
}

