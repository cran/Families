#' IDs of aunts and uncles of ego
#' 
#' Retrieves the IDs of ego's aunts and uncles by blood. Partners (in-laws) are not included. 
#' They are obtained using the IDPartner() function.
#' 
#' 
#' @param idego Identification number(s) of ego(s).
#' @param d Name of database. If missing, dLH is used, if it exists in the global environment (i.e. R workspace). 
#' Otherwise the dataset dLH distributed with the Families package are used. 
#' @return IDs of aunts and uncles by blood (without their partner, i.e. in-laws). 
#' @examples
#' # Load the data
#' data(dLH,package="Families")
#' set.seed <- 45
#' idego <- sample(dLH$ID[dLH$gen==3],1)
#' idau <- IDau (idego)
#' 
#' idego <- dLH$ID[dLH$gen==3]
#' idau <- IDau (idego,d=dLH)
#' 
#' @export IDau

IDau <- function(idego,d=NULL)
{  # Perform a few tests
   test <- Tests(idego=idego,d=d)
   idego <- test$idego
   d <- test$d
    
   # Check: ego should not be member of first or second generation
   if (d$gen[idego][1]<3)
   { warning(paste0("Aunts and uncles cannot be determined",
                    " for members of first and second generation"))
     return()
   }
   # Siblings of parents 
  id0 <- data.frame(idego=idego,idm=IDmother(idego),idf=IDpartner(IDmother(idego)))
  idauM <- IDsib(id0$idm)
  names(idauM$id) <- idego
  colnames(idauM$dfsib)[1] <- "parent"
  idauP <- IDsib(id0$idf)  
  names(idauP$id) <- idego  
  colnames(idauP$dfsib)[1] <- "parent"

  # Create dataframe in long format: ID of aunts and uncles of ego
  # idkin is aunt or uncle: Maternal and Paternal
  dfauM <- idauM$dfsib  #   Kin_long(idauM)
  dfauP <- idauP$dfsib  # Kin_long(idauP)
  dfauM$family <- "Maternal"
  dfauP$family <- "Paternal"
  # Add idego
  nn <- sapply(idauM$id,function(x) length(x))
  nn[nn==0] <- 1
  dfauM$idego <- rep(idego,nn)
  nn <- sapply(idauP$id,function(x) length(x))
  nn[nn==0] <- 1
  dfauP$idego <- rep(idego,nn)
  # Merge
  dfau <- rbind(dfauM,dfauP)
  colnames(dfau)[1] <- "parent"
  dfau <- dfau[,c("idego","parent","idkin","family")]


  # Sort data frame 
  dfau <- dfau[order(dfau$idego),]
  # Add gender: aunt/uncle
  dfau$gender <- d$sex[dfau$idkin]
  dfau$gender <- factor(d$sex[dfau$idkin],levels=c("Female","Male"),labels=c("Aunt","Uncle"),ordered=TRUE)

  # Add number of children
  dfau$nch <- d$nch[dfau$idkin]
  kk <- !is.na(dfau$idkin) & d$sex[dfau$idkin]=="Male"
  dfau$nch[kk] <- d$nch[IDpartner(dfau$idkin[kk])] 

return(dfau)
}