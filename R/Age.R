#' Age of ego or kin at given calendar date
#' 
#' Ages are computed from calendar dates. 
#' 
#' 
#' @param idref vector of IDs of reference population, usually idego
#' @param refT Calendar date (decimal date) provided or computed

#' @return 
#' \item{Age}{Age of kin (or ego). If ego is not yet born or dead at the reference date, the missing data symbol NA is returned.}
#' 
#' @examples
#' 
#' # Load data
#' data(dLH,package="Families")
#' 
#' # Age of ego on January 1, 2100. Ego is individual with ID equal to 1 in dLH
#' idego <- 1 
#' age <- Age(idref=idego,refT=2100)
#' 
#' # Age of ego at death of mother
#' idego <- dLH$ID[dLH$gen==2]
#' age <- Age(idref=idego,refT=Dd(IDmother(idego)))
#' 
#' # Age of siblings at ego's 20th birthday
#' idego <- dLH$ID[dLH$gen==2]
#' # Get IDs of siblings and convert the list object into a dataframe
#' idsib <- IDsib(idego)
#' names(idsib$id) <- idego
#' dfsib <- Kin_long(idsib$id)
#' colnames(dfsib) <- c("idego","idkin")
#' # Get ages of siblings at 20th birthday of egos
#' dfsib$age <- Age(idref=dfsib$idkin,refT=Db(dfsib$idego)+20)
#' 
#' @export Age
#' 
Age <- function(idref,refT)
 { # Is individual (idref) alive at point in time (refT)?
   alive <- Alive (idref=idref,refA=refT)
  
   # Age of id (ego or kin) in given calendar year
   aa <- sample(refT[!is.na(refT)],1)
   ageIndiv <- refT - Db(idref)
   ageIndiv[!alive] <- NA
   return (ageIndiv)
}