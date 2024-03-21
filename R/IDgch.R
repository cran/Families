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
#'    \item ID of grandparent (ego), 
#'    \item ID of child,
#'    \item ID of grandchild.
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
  
  # =====   IDs of triads ego-child-grandchild (dataframe)   ======
  if (keep_ego==FALSE)
  { # IDs of all grandchildren combined (vector)  
    dfgch <- IDch(IDch(idego))
  } else
  { # IDs of dyads of all indiv in idego combined
    #             (female and male; no double counting)
    dfch <- IDch(idego,keep_ego=TRUE)
    
    if (all(is.na(dfch))) idgch <- NA else 
      { # IDs of grandchildren (children of children) of idego
        dfgch <- IDch(idego=dfch$idch,keep_ego=TRUE) 
        colnames(dfgch) <- c("idch","idgch")
        # Add column with ID of ego
        #dfgch$idego <- IDmother(dfgch$idch)
        # dfgch <- dfgch[,c(3,1,2)]

        # =============   Add idego to obtain triad  ===============
        #          idego is mother or father of a child 
        dfgch$idgrandmother <- IDmother(dfgch$idch) 
        dfgch$idgrandfather <- IDfather(dfgch$idch)
        kk <- match(dfgch$idch,dfch$idch)
        dfgch$idego <- dfch$idego[kk]
        dfgch <- dfgch[!is.na(dfgch$idch),] # 15653
        # Add idego with 0 children
        ka <- dfch$idego[is.na(dfch$idch)]  # 2862
        kb <- data.frame(idch=rep(NA,length(ka)),idgch=rep(NA,length(ka)),
                         idgrandmother=rep(NA,length(ka)),
                         idgrandfather=rep(NA,length(ka)),
                         idego=ka)
        dfgch <- rbind(dfgch,kb)
        
        dfgch <- dfgch[,c("idego","idch","idgch","idgrandmother","idgrandfather")]
        dfgch <- dfgch[order(dfgch$idego),]
        
        dfgch$idgch[is.na(dfgch$idgch)] <- 0
          
        # IDs of females without children
        idch0 <- d$ID[idego][d$sex[idego]=="Female" & d$nch[idego]==0]
        # Add females without children
         kb <- data.frame(idego=idch0,
                         idch=rep(NA,length(idch0)),
                         idgch=rep(NA,length(idch0)),
                         idgrandmother=rep(NA,length(idch0)),
                         idgrandfather=rep(NA,length(idch0)))
        dfgch <- rbind(dfgch,kb)
        
        # Rearrange rows
        dfgch <- dfgch[order(dfgch$idego),]
    }
  }
   return(dfgch)
}
