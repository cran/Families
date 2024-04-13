#' Living status of individual at given age or calendar date
#' 
#' Determines living status of individual at given age or calendar date. The living status is requested for ego or kin. 
#' If the number provided is less than 200, the number is interpreted as age. 
#' 
#' 
#' @param idref  vector of IDs. 
#' @param refA calendar date (decimal date) or age at which survival status of individuals should be determined. 
#' Dates can differ between individuals, but number of dates must be same as number of individuals.
#' @return 
#' \item{alive_kin}{Living status of ego (if id is a vector) or kin (if id is a dataframe)}
#' @examples
#' # Load the data
#' data(dLH,package="Families")
#' # Select members of generation 1
#' idego <- dLH$ID[dLH$gen==1]
#' # Are egos alive at age 18?
#' alive <- Alive(idref=idego,refA=18)
#' # Number alive and deceased
#' tab <-  table (alive)
#' 
#' ## Is ego alive at 18th birthday of oldest child?
#' date <- sapply(idego,function(x) min(Db(IDch(x))+18))
#' alive <- Alive(idego,refA=date)
#' 
#' @export Alive
#' 
Alive <- function (idref,refA) 
 { # Is ego alive at given age or at given date of event (just before event)?
      # (hence: ego is alive at date of death)
 
    reftest <- refA[!is.na(refA)][1]
    if (reftest < 200) ref2 <- Db(idref) + refA  else ref2 <- refA
    # CHECK several idref are NA while ref2 has no NA
    born <- Db(idref) <= ref2 # ref2 can be age 0
    alive <- Dd(idref) >= ref2   
    alive_kin <- born & alive

    return(alive_kin)
 }

