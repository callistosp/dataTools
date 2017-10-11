#' makeMDVEVID()
#'
#' This function adds MDV and EVID columns to NONMEM
#' formatted data.frames
#' @param ds NONMEM formatted data.frame to add MDV/EVID column to
#' @param DV column number containing DV record
#' @param AMT column number containing AMT record
#' @keywords dataTools
#' @export
#' @examples
#' @author Samuel Callisto \email{calli055@umn.edu}
#' makeMDVEVID()

makeMDVEVID <- function(ds, DV, AMT){
  ## initially assume all rows obs event
  ds$MDV <- 0
  ds$EVID <- 0

  ## loop through all rows
  for(i in 1:nrow(ds)){
    ## check dosing event
    if(!(ds[i,AMT] == "." || is.na(ds[i,AMT]))){
      ds$MDV[i] <- 1
      ds$EVID[i] <- 1
    }
    ## check missing DV
    if((ds[i,AMT] == "." || is.na(ds[i,AMT])) && (ds[i,DV] == "." || is.na(ds[i, DV]))){
      ds$MDV[i] <- 1
      ds$EVID[i] <- 2
    }
  }

  return(ds)
}
