#' Extract Patients
#'
#' Data.frames are subsetted using a vector of patients.
#'
#' This function extracts a subset of patient data from a larger dataset. It
#' assumes the first column of the data.frame \code{ds} contains alphanumeric
#' subject identifications (\code{SID}) to be filtered; this column will be
#' renamed "SID" as a result of this function if it is not already.
#'
#' @param ds Parent data.frame to extract patient subset from.
#' @param px Vector of patient identifiers to extract from the dataset.
#' @return A subset of the data.frame ds will be returned with only the patients
#'   listed in the px vector. The first column will be renamed "SID" as a result
#'   of this function.
#' @examples
#' dataset <- data.frame(c(1:20), LETTERS[1:20], letters[1:20])
#' names(dataset) <- c("SID", "cov1", "cov2")
#' output <- extractPatients(dataset, 4:8)
#' output
#'
#' @author Samuel Callisto \email{calli055@@umn.edu} extractPatients()
#' @importFrom stringr str_detect
#' @export

## use of a function to filter and merge
## datasets to include only listed patients
extractPatients <- function(ds, px){
  if(!is.data.frame(px)){
    px <- as.data.frame(px)
    names(px) <- c("SID")
  }
  names(ds)[1] <- c("SID")
  ## change to upper if letters in SID
  if(all(str_detect(c(ds$SID, px$SID), "[[:alpha:]]"))){
    ds$SID <- toupper(ds$SID)
    px$SID <- toupper(px$SID)
  }
  finalds <- merge(px, ds, by="SID")
  return(finalds)
}
