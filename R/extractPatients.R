#' Extract Patients
#'
#' This function extracts a subset of patient data from a larger dataset
#' @param ds parent data.frame to extract data from
#' @param px list of patient identifiers to extract from the dataset
#' @keywords dataTools
#' @export
#' @examples
#' @author Samuel Callisto \email{calli055@umn.edu}
#' @import stringr
#' extractPatients()

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
