#' IDs of children of ego
#' 
#' Retrieves IDs of children of ego(s).
#' 
#' 
#' @param idego ID of ego(s). If the vector idego includes missing elements (NA), they are removed.
#' @param d Name of database. If d is missing, the dataset dLH in the global environment (R workspace) is used. 
#' If no dLH in the global environment, the database dLH distributed with the Families package is used.  
#' @param keep_ego Logical variable. If TRUE, a dataframe of parent-child and father-child dyads is produced. 
#' It includes, for each ego (parent), ego's ID and the IDs of ego's children.
#' @return Two cases:
#' \itemize{
#'  \item keep_ego=FALSE: IDch() returns the IDs of children. If ego has no children or IDs of children are not
#' included in database, the missing data symbol NA is returned. The
#' vector idego may include the IDs of egos who form a couple. In that case, the IDs of their children are included 
#' only once to prevent double-counting.
#'  \item keep_ego=TRUE: IDch() returns a dataframe of parent-child dyads. If idego includes the IDs of egos 
#'  who form a couple, then the IDs of their children are included twice, in the mother-child dyads and
#'  in the father-child dyads. To select the father-child dyads, select the male egos. The dataframe of parent-child
#'  dyads include childless females and males. The dyad has the ID of the female(male) and NA instead of the 
#'  ID of the child. The object returned has the following columns: 
#'  \itemize{
#'    \item ID of ego (parent of child)
#'    \item ID of child
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
{   if (all(is.na(idego))) return(dfch=NA) else
  { # ==============   Perform few tests   ===============
    # idego <- idego[!is.na(idego)]
    test <- Tests(idego=idego,d=d)
    idego <- test$idego
    d <- test$d
    if (all(is.na(idego))) return(dfch=NA)
    # ============   IDs of females and males   =============
    idFem <- idego[!is.na(idego) & d$sex[idego]=="Female"]
    idMal <- idego[!is.na(idego) & d$sex[idego]=="Male"]
    # ===========   IDs of children of women (IDchMOM) =============
    idchFem <- d$ID[!is.na(d$IDmother) & d$IDmother%in%idFem]
    if (length(idchFem)==0) idchFem <- NA
    # ==========  IDs of children of males (IDchDAD) ============
    if (length(idMal>0)) 
      {idchMal <-  d$ID[!is.na(d$IDmother) & d$IDmother%in%IDpartner(idMal)]} else
        idchMal <- numeric(0)
      
    dfch <- c(idchFem,idchMal)
    dfch <- unique(dfch[!is.na(dfch)])
    # If idego is male without a partner
    if (length(dfch) == 0) return(NA)

    if (keep_ego)
    { # If idego includes both females and their partners, ID of their
      # child is included twice
  # ================  dyads mother - child  =========
    dfMOM <- data.frame(idego=IDmother(idchFem),idch=idchFem)
    # ================  dyads father - child  =========
    if (length(idMal>0)) 
      { dfDAD <- data.frame(idego=IDfather(idchMal),idch=idchMal)} else
        dfDAD <- numeric(0)
    
  # =========  Childless females and males included in idego  =========
    idFem0 <- subset(idFem,d$nch[idFem]==0)
    # If idego has no partner, idego has no children
    idMal0 <- subset(idMal,d$nch[IDpartner(idMal)]==0 | 
                              is.na(IDpartner(idMal)))
                               # No partner d$nch is NA (and not 0)
    dfP0 <- data.frame(idego=c(idFem0,idMal0))
    if (length(dfP0$idego)>0) dfP0$idch <- NA
    
  # ==================  Merge dataframes  =====================
    dfch <- rbind(dfMOM,dfDAD,dfP0)
    dfch <- dfch[!is.na(dfch$idego),]
  # =================  Sort dataframe by idego  =================
    dfch <- dfch[order(dfch$idego,decreasing=FALSE),]
    # dfc has IDs of children TWICE + IDs of childless females and males
    #     length(dfch$idego[is.na(dfch$idch)])   7874*2+2602=18350
    }
  }
   return (dfch)
}

