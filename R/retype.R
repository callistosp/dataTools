## TYPECAST function
# @param df: data.frame with values to re-type
# @param ...: unquoted list alternating variables and datatypes to convert to
retype <- function(df, ...) {
  ## save input arguments as a vector
  vec <- as.vector(substitute(...()))
  ## loop through vector
  for(i in 1:length(vec)){
    ## skip even indices
    if(i %% 2 == 0) next
    ## retype df column "i" to type "i+1"
    cmd <- paste0("as.",vec[i+1],"(df$",vec[i],")")
    df[[vec[[i]]]] <- eval(parse(text=cmd))
  }
  return(df)
}

