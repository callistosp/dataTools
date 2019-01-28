#' Retype Variable
#'
#' retype is a typecasting function which can be used
#' in the tidy workflow to change the type of multiple
#' functions within a data.frame in-line.
#'
#' @param df: data.frame with values to typecast
#' @param ...: unquoted list alternating variables and datatypes to convert to
#' @return Data.frame \code{ds} with variables listed typecast to
#' @examples
#' iris2 <- iris %>% select(Species, Sepal.Length) %>% retype(Species, character)
#' str(iris2)
#'
#' @author Samuel Callisto \email{calli055@@umn.edu} retype()
#' @export


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

