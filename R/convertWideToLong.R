#' Convert Wide Data To Long
#'
#' This function converts patient data in wide form to long form.
#'
#' Long description.
#'
#' @param data Data.frame to be transformed from wide to long.
#' @param length The number of columns in each repeated block.
#' @return Data.frame in long form.
#' @examples
#'
#'
#'
#' @author Samuel Callisto \email{calli055@@umn.edu} convertWideToLong()
#' @export

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
