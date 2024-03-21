#' Number of kin of given type
#' 
#' Computes, for each ego, number of kin of given type (parents, grandparents, aunts, uncles, siblings, etc.)
#' 
#' 
#' @param idkin dataframe of IDs of ego and IDs of kin of given type
#' @return Number of kin of given type
#' @examples
#' # Load the data
#' data(dLH,package="Families")
#' idego <- dLH$ID[dLH$gen==3]
#' ## Number of siblings
#' idsib <- IDsib(idego)
#' names(idsib$id) <- idego
#' nsib <- Nkin(idkin=idsib$d)
#' 
#' @export Nkin
#' 
Nkin <- function (idkin)
  { # Living kin
    nkin <- aggregate(idkin ~ idego,data=idkin,
                      function(x) n <- length(x[!is.na((x))]),na.action=na.pass)
      # sapply(idkin,function(x) n <- length(x[!is.na(x)]))
    return(nkin)
  }
