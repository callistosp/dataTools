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

mergeByDate <- function(ds, ds2add){
  ## Assumption: ds2add has 3 columns (SID, DATE, VAR)
  ## mergeByDate function adds new column to ds that contains
  ## VAR from nearest past DATE for SID to each row
  ## and returns ds
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
