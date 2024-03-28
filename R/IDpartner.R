#' ID of partner of ego
#' 
#' Retrieves ID of partners of vector of egos #' 
#' 
#' @param idego vector of IDs of egos.
#' @param d Name of database. If d is missing, the dataset dLH in the global environment (R workspace) is used. 
#' If no dLH in the global environment, the database dLH distributed with the Families package is used.  
#' @return Vector of IDs of partners
#' 
#' @examples
#' # Load the data
#' data(dLH,package = "Families")
#' # Get ID of partner
#' IDpartner(idego=1,d=dLH)
#' IDpartner(idego=c(4,9,NA,30),d=dLH)  
#' 
#' @export IDpartner
#' 
IDpartner <- function(idego,d=NULL)
{ 
test <- Tests(idego=idego,d=d)
idego <- test$idego
d <- test$d

# ===============  Get ID of partner(s) of ego  ===============
if (all(is.na(idego))) return(NA)
idp <- d$IDpartner[idego]
return (idp)
}
