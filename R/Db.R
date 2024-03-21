#' Date(s) of birth in decimal format
#' 
#' Retrieves date(s) of birth from the database
#' 
#' 
#' @param idego vector of IDs of egos
#' @param d Name of database. If d is missing, dLH is used.
#' @return Dates of birth (decimal format)
#' @examples
#' # Load the data
#' data(dLH,package = "Families")
#' 
#' # Date of birth of first individual in database
#' d <- dLH
#' Db(idego=1,d=d) 
#' 
#' 
#' @export Db
Db <- function (idego,d=NULL)
{
  # Tests
  test <- Tests(idego=idego,d=d)
  idego <- test$idego
  d <- test$d
  
  # Date of birth
  dbd <- rep(NA,length(idego))
  dbd[!is.na(idego)]  <- d$bdated[idego[!is.na(idego)]]
  return (dbd)
}
