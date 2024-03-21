#' IDs of children of ego
#' 
#' Retrieves IDs of children of ego(s).
#' 
#' 
#' @param idego ID of ego(s)
#' @param d Name of database. If d is missing, the dataset dLH in the global environment (R workspace) is used. 
#' If no dLH in the global environment, the database dLH distributed with the Families package is used.  
#' @param keep_ego Logical variable. If TRUE, a dataframe of parent-child and father-child dyads is produced. 
#' It includes, for each ego (parent), ego's ID and the IDs of ego's children.
#' @return Two cases:
#' \itemize{
#'  \item keep_ego=FALSE: IDch() returns the IDs of children. If ego has no children or IDs of children are not
#' included in database (e.g. in case of last generation considered), the missing data symbol NA is returned. 
#'  \item keep_ego=TRUE: IDch() returns a data frame of parent-child dyads. It has the following columns: 
#'  \itemize{
#'    \item ID of parent of child (ego)
#'    \item ID of child
#'    \item sex of ego 
#'    \item sex of child
#'    }
#' }
#'    
#' @examples
#' # Load the data
#' data(dLH,package=("Families"))
#' 
#' IDch(idego=1)
#' set.seed(43)
#' id <- sample (dLH$ID[dLH$gen==1],10)
#' id2 <- IDch(idego=sort(id),keep_ego=TRUE)
#' id3 <- IDch(id2$idch,keep_ego=TRUE)
#' 
#' 
#' @export IDch
IDch <-
function (idego,d=NULL,keep_ego=FALSE)
{   if (all(is.na(idego))) idch3 <- NA else
  { # Perform few tests
    test <- Tests(idego=idego,d=d)
    idego <- test$idego
    d <- test$d
   
    #  =============    IDs of children of females (idchFem)  =========== 
    # IDs of females (idF) in idego
    idFem <- idego[d$sex[idego]=="Female"]
    # Find individuals with ID of mother in idF
    idchAFem <-  NA
    if (length(idFem) > 0) idchAFem <- 
              d$ID[d$IDmother%in%idFem[!is.na(idFem)]] else
              idchAFem <- NA
    
    #  =============   IDs of children of males (idchMal)  ==============
    # IDs of males
    idMal <- idego[d$sex[idego]=="Male"]
    # Find individuals with ID of father in idMal
    idchAMal <- NA
    # Find individuals with ID of father in idF
    if (length(idMal) > 0) idchAMal <- 
      d$ID[d$IDmother%in%IDpartner(idMal)[!is.na(IDpartner(idMal))]]
    
    # ==========  IDs of children of all members of idego combined  =====
    # Correct for double counting
    idch3 <- unique(c(idchAMal[!is.na(idchAMal)],idchAFem[!is.na(idchAFem)]))
    if (length(idch3)==0) idch3 <- NA
    
    # ======= IDs of dyads ego-child (dataframe)  =========
    #          long format (females and males)
    #   if ego has n children => n rows added
    #   if ego has no children => one row added (for ego)
    if (keep_ego==TRUE & !all(is.na(idch3)))
     { # -----------  Children of females  --------------
          idegoFemale <- idego[d$sex[idego]=="Female"]
          # Create dataframe with ID of ego (idegoFemale) and ego's children
         idch_mother <- 
           d$ID[IDmother(d$ID)%in%idegoFemale[!is.na(idegoFemale)]]
         idM <- NULL
         if (length(idch_mother) > 0) 
           idM <- data.frame(idego=IDmother(idch_mother),idch=idch_mother)
         #  Add childless women included in idego
         id_women0 <- idego[d$sex[idego]=="Female" & d$nch[idego]==0 | is.na(d$nch[idego])]
         idw0 <- data.frame(idego=id_women0,idch=rep(NA,length(id_women0)))
         idwomen <- rbind(idM,idw0)
         
         # -----------  Children of Males  --------------
         idegoMale <- idego[d$sex[idego]=="Male"]
         # Partner of idegoMale (if idegoMale has partner)
         if (length(idegoMale)>0) idp <- IDpartner(idegoMale) else idp <- NA
         # Children of idegoMale
         idch_father <- d$ID[IDmother(d$ID)%in%idp[!is.na(idp)]]
         idF <- NULL
         if (length(idch_father) > 0) idF <- data.frame(idego=
                      IDpartner(IDmother(idch_father)),idch=idch_father)         
         #  Add childless men included in idego
         id_men0 <- idegoMale[!idegoMale%in%idF$idego]
         idmen0 <- data.frame(idego=id_men0,idch=rep(NA,length(id_men0)))
         idmen <- rbind(idF,idmen0)        
         
         # -------  Merge males and females  ---------
         idch2 <- rbind(idwomen,idmen)
          # Number of individuals in generation 1 without a partner
                # length(d$ID[idego][is.na(d$IDpartner[idego])])
         # Sort data frame
         idch2 <- idch2[order(idch2$idego,decreasing=FALSE),]
         idch3 <- idch2
     }
  }
   return (idch3)
}

