#' Auto-populate MDV and EVID Columns.
#'
#' This function adds MDV and EVID columns to NONMEM formatted data.frames.
#'
#' Long description.
#'
#' @param ds NONMEM formatted data.frame to add MDV/EVID column to. Assumes
#'   contains column with patient identifiers named "SID".
#' @param DV Column number containing DV record.
#' @param AMT Column number containing AMT record.
#' @param clearSystem Boolean variable; if \code{TRUE} will clear system
#'   (EVID=4) for multiple dosing events in a single patient.
#' @param C Indicates that the first column contains comments rather than IDs. Second
#'   column used for IDs instead
#' @return Data.frame \code{ds} with two columns (MDV and EVID) appended to the end.
#' @examples
#'
#' @author Samuel Callisto \email{calli055@@umn.edu} makeMDVEVID()
#' @export

makeMDVEVID <- function(ds, DV, AMT, clearSystem=F, C = F){
  if(C){
    tmpName <- names(ds)[2]
    names(ds)[2] <- c("SID")
  }else{
    tmpName <- names(ds)[1]
    names(ds)[1] <- c("SID")
  }

  ## initially assume all rows obs event
  ds$MDV <- 0
  ds$EVID <- 0

  currentPatient <- ds$SID[1]
  prevDose <- F

  ## loop through all rows
  for(i in 1:nrow(ds)){
    ## check if new patient
    if(ds$SID[i] != currentPatient){
      currentPatient <- ds$SID[i]
      prevDose <- F
    }

    ## check dosing event
    if(!(ds[i,AMT] == "." || ds[i,AMT] == 0 || is.na(ds[i,AMT]))){
      ds$MDV[i] <- 1
      ds$EVID[i] <- ifelse(clearSystem && prevDose, 4, 1)
      prevDose <- T
    }

    ## check missing DV
    if((ds[i,AMT] == "." || ds[i,AMT] == 0 || is.na(ds[i,AMT])) && (ds[i,DV] == "." || ds[i,DV] == 0 || is.na(ds[i, DV]))){
      ds$MDV[i] <- 1
      ds$EVID[i] <- 2
    }
  }

  if(C){
    names(ds)[2] <- tmpName
  }else{
    names(ds)[1] <- tmpName
  }

  return(ds)
}
