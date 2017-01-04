# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

## use of a function to filter and merge entire RedCap
## datasets to include only the PPEP patients
extractPatients <- function(ds, px){
  names(ds)[1] <- c("SID")
  ds$SID <- toupper(ds$SID)
  finalds <- merge(px, ds, by="SID")
  return(finalds)
}
