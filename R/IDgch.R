#' IDs of grandchildren of ego
#' 
#' Retrieves ID of grandchildren of ego or vector of egos
#' 
#' 
#' @param idego ID of ego(s)
#' @param d Name of database. If d is missing, the dataset dLH in the global environment (R workspace) is used. 
#' If no dLH in the global environment, the database dLH distributed with the Families package is used.  
#' @param keep_ego If keep_ego=TRUE, parent-child-grandchild triads are shown.
#' @return Two cases:
#' \itemize{
#'  \item keep_ego=FALSE: The function IDgch() returns the IDs of grandchildren. If ego has no grandchildren or IDs of grandchildren are not
#' included in database, the missing data symbol NA is returned. 
#'  \item keep_ego=TRUE: IDgch() returns a data frame of 
#' child-parent-grandparent triads. A triad consists of: 
#' \itemize{
#'    \item{idego}  ID of grandparent (ego)
#'    \item {idch}  ID of child
#'    \item {idgch} ID of grandchild
#'    \item{gp}     lineage: maternal grandfather, paternal grandmother, etc.
#'    \item{idMOM}  ID of mother of grandchild
#'    \item{idDAD}  ID of father of grandchild
#'    }
#' }
#' 
#' @examples
#' 
#' # Load data
#' data(dLH,package="Families")
#' IDch(IDch(dLH$ID[dLH$sex=="Female" & dLH$gen==1]))[1]
#' 
#' set.seed(51)
#' id <- sample (dLH$ID[dLH$gen==1],10)
#' id2 <- IDgch(idego=sort(id),keep_ego=TRUE)
#' 
#' @export IDgch
#' 
IDgch <- function(idego,d=NULL,keep_ego=FALSE)
{  if (all(is.na(idego))) stop("All elements of idego vector are missing")
  # Perform few tests
  test <- Tests(idego=idego,d=d)
  idego <- test$idego
  d <- test$d
  
  # IDs of children
  dfch <- IDch(idego,keep_ego=TRUE)
  
  # =========  IDs of grandchildren (all combined)  ==========
  dfgch <- IDch(IDch(idego))
  
  # =====   IDs of triads ego-child-grandchild (dataframe)   ======
  if (keep_ego==TRUE)
  { # IDs of dyads of all indiv in idego combined
    #             (female and male; no double counting)
    # ============ Triads ego - child -grandchild   ========
    dfch <- IDch(idego,keep_ego=TRUE)
    
    if (all(is.na(dfch))) return() else 
      { # ========  IDs of grandchildren (children of children) of idego ===
        # each child has MOM=DAD in idego => idch twice in dfch$idch
        #     7874*2 = 15748
        
      #  library(Families)
      #  load("/users/frans/VirtualPop_data/dLH_USA2021_5_10000.RData")
      #  idego <- dLH$ID[dLH$gen==1]
        # IDs of grandchildren
        idgch <- IDch(IDch(idego))  # 6102
        # Mothers and fathers of grandchildren
        idMOM <- IDmother(idgch)
        idDAD <- IDfather(idgch)
        # Maternal and paternal grandmothers and grandfathers
        id_MOMdad <- IDmother(idDAD)
        id_MOMmom <- IDmother(idMOM)
        id_DADdad <- IDfather(idDAD)
        id_DADmom <- IDfather(idMOM)
        # Create dataframe (one line for each triad grandchild-child-ego)
        dfgch1 <- data.frame(idgch=idgch,idch=idDAD,idMOM=idMOM,idDAD=idDAD,
                             idgp=id_MOMdad,gp="MOMdad")
        dfgch2 <- data.frame(idgch=idgch,idch=idMOM,idMOM=idMOM,idDAD=idDAD,
                             idgp=id_MOMmom,gp="MOMmom")        
        dfgch3 <- data.frame(idgch=idgch,idch=idDAD,idMOM=idMOM,idDAD=idDAD,
                             idgp=id_DADdad,gp="DADdad")
        dfgch4 <- data.frame(idgch=idgch,idch=idMOM,idMOM=idMOM,idDAD=idDAD,
                             idgp=id_DADmom,gp="DADmom")
        dfgch <- rbind (dfgch1,dfgch2,dfgch3,dfgch4)
        dfgch <- dfgch[order(dfgch$idgch,decreasing=FALSE),]
        dfgch <- dfgch[order(dfgch$idgp,decreasing=FALSE),]
        dfgch$idego <- dfgch$idgp[dfgch$idgp%in%idego]
        dfgch$sexego <- d$sex[dfgch$idego]
        dfgch <- dfgch[,c("idego","idch","idgch","sexego","gp","idMOM","idDAD")] # idgp omitted

        # Egos without children 
        idego_ch0 <- subset(idego,(is.na(d$nch[idego]) | d$nch[idego]==0) & 
            (is.na(d$nch[IDpartner(idego)]) | d$nch[IDpartner(idego)]==0))
        # Egos without grandchildren (irrespective of presence of children)
        idego_gch0 <- subset(idego,!idego%in%dfgch$idego)
        # Egos with children but no grandchildren
        idego_ch_gch0 <- subset(idego_gch0,!idego_gch0%in%idego_ch0)
        
        # Some childless egos have no partner (d$IDpartner=NA & d$nch=NA)
        a <- length(unique(dfgch$idego))
        b <- length(idego_ch0)
        c <- length(idego_ch_gch0)
        a+b+c
        B <- data.frame(idego=idego_ch0,idch=rep(NA,b),idgch=rep(NA,b),
                        sexego=rep(NA,b),
                        gp=rep(NA,b),idMOM=rep(NA,b),idDAD=rep(NA,b))
        B$sexego <- d$sex[B$idego]
        C <- subset(dfch[,c(1,2)],dfch$idego%in%idego_ch_gch0)  
        if (nrow(C)>0) 
          { C$sexego <- d$sex[C$idego]
            C$idgch <- NA
            C$gp <- NA
            C$idMOM <- NA
            C$idDAD <- NA
          }
        dfgch <- rbind(dfgch,B,C)
        dfgch <- dfgch[order(dfgch$idego,decreasing=FALSE),]
        
     }
  }
   return(dfgch)
}
