#' Date(s) of death in decimal format
#' 
#' Retrieves date(s) of death from the database
#' 
#' 
#' @param idego vector of IDs of egos
#' @param d Name of database. If d is missing, dLH is used.
#' @return Date of death (decimal format)
#' @examples
#' # Load the data
#' data(dLH,package = "Families")
#' 
#' # Date of death of first individual in database
#' Dd(idego=1,d=dLH) 
#' 
#' @export Dd
#' 
Dd <- function (idego,d=NULL)
{ # Tests
  test <- Tests(idego=idego,d=d)
  idego <- test$idego
  d <- test$d
  
  # Date death
  dd <- rep(NA,length(idego))
  dd[!is.na(idego)]  <- d$ddated[idego[!is.na(idego)]]

  return (dd)
}
