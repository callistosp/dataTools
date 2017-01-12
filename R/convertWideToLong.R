#' Convert Wide To Long
#'
#' This function converts patient data in wide form to long form
#' @param data this is the data.frame to be transformed
#' @param length the number of repeated blocks
#' @keywords
#' @export
#' @examples
#' convertWideToLong()

## takes data.frame "data", preserves the first column as a label column,
## and condenses the data to longform with ("length" + 1) columns per row
convertWideToLong <- function(data, length){
  output <- data.frame()
  ## calculate number of blocks of given length,
  ## floor() to ignore any extra columns on the end
  nblock <- floor(ncol(data)/length)-1
  for(i in 0:nblock){
    cols <- i*length + (2:(length+1))
    appendage <- data.frame(data[,1])
    if(! all(is.na(data[,cols]))){
      ## values present in this block
      appendage <- cbind(appendage, data[,cols])
      names(appendage) <- names(data[1:(length+1)])
      output <- rbind(output, appendage)
    }
  }
  ## remove blank rows introduced by rbind
  keep <- apply(output[,2:ncol(output)],1,function(x)any(!(is.na(x) | x=="")))
  return(output[keep,])
}

## use of a function to filter and merge entire RedCap
## datasets to include only the PPEP patients
extractPatients <- function(ds, px){
  names(ds)[1] <- c("SID")
  ds$SID <- toupper(ds$SID)
  finalds <- merge(px, ds, by="SID")
  return(finalds)
}
