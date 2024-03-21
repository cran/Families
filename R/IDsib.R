#' IDs of siblings of ego
#' 
#' Retrieves IDs of siblings of ego
#' 
#' 
#' @param idego ID of ego
#' @param d Name of database. If d is missing, the dataset dLH in the global environment (R workspace) is used. 
#' If no dLH in the global environment, the database dLH distributed with the Families package is used.  
#' @return A list vector of two elements. 
#' \itemize{
#'  \item dfsib: dataframe of IDs of ego-sibling dyads. 
#'  \item id: vector of IDs of siblings. 
#' }
#' 
#' Returns NA if ID of sibling cannot be computed because the ID of mother is not included in the database.
#' 
#' @examples
#' # Load data
#' data(dLH,package="Families")
#' # IDs of siblings of single member of generation 3
#' set.seed(34)
#' idego <- sample(dLH$ID[dLH$gen==3],1)
#' idsib <- IDsib (idego)
#' 
#' # For each member of generation 2, IDs of siblings
#' idego <- dLH$ID[dLH$gen==2]
#' idsib <- IDsib(idego)
#' 
#' @export IDsib
#'
IDsib <- function(idego,d=NULL) {
  # Tests
  test <- Tests(idego=idego,d=d)
  idego <- test$idego
  d <- test$d
  
  # Check: ego should not be member of first generation
  if (d$gen[idego[1]] == 1)
        { warning(paste0("Sibling cannot be determined",
        " for members of first generation"))
         return()
        }
  # IDs of siblings: list vector
  idsib <- sapply(idego, function(x) {
       idch <- IDch(IDmother(x))
       sib <- idch[idch!=x]
       #names(sib) <- d$sex[sib] # OR Sex(sib)
       sib
  })
  names(idsib) <- idego
  
  # IDs of siblings: dataframe
  dfsib <- Kin_long(idsib)
  # To sort data frame: dfsib[order(dfsib$idego),]
  
  aa <- list(dfsib=dfsib,
             id=idsib)
  
  return(aa)
}