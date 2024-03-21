#' ID of mother of ego
#' 
#' Retrieves the ID of mother of ego or mothers of vector of egos
#' 
#' 
#' @param idego ID
#' @param d Name of database. If d is missing, the dataset dLH in the global environment (R workspace) is used. 
#' If no dLH in the global environment, the database dLH distributed with the Families package is used.  
#' @return ID of mother. Returns NA if ID of mother is not included in the database
#' @examples
#' 
#' # load the data
#' data(dLH,package = "Families")
#' IDmother (sample(dLH$ID[dLH$gen==2],1),d=dLH)
#' 
#' 
#' @export IDmother
IDmother <- function (idego,d=NULL)
{ 
  if (all(is.na(idego))) idm <- NA else
  { # Perform few tests
    test <- Tests(idego=idego,d=d)
    idego <- test$idego
    d <- test$d

    # ID mother
    idm <- d$IDmother[idego] 
  }
  return(idm)
}
