#' Converts a list vector of kin into a dataframe of kin
#' 
#' Converts a list vector of kin into a dataframe of kin. The dataframe has a "long format", 
#' i.e. it has one row for each ego-kin dyad.
#' 
#' 
#' @param idkin List vector of kin
#' @return Dataframe of kin (long format)
#' 
#' @examples
#' # Load data
#' data(dLH,package="Families")
#' # IDs of ego and their grandchildren
#' idego <- dLH$ID[dLH$gen==1]
#' # IDs of grandchildren of ego 
#' idgch <- lapply(idego,function(x) IDch(IDch(x)))
#' names(idgch) <- idego
#' # Dataframe with ID of grandmother and grandchild
#' dfgch <- Kin_long(idkin=idgch)[,1:2]
#' 
#' @export Kin_long
#' 
Kin_long <- function(idkin)
   # https://stackoverflow.com/questions/35360917/get-long-format-data-frame-from-list
   { # For each ego, create vector with ego and siblings (zz is list)
   zz <- mapply(function(x,z) 
         { mm <- length(x)
           if (length(x)==0) { mm <- 1 ; x <- NA }
           e <- rep(z,mm)
           # if (!is.null(names(x))) label <- names(x) else label <- " "
           ee <- data.frame(idego=e,idkin=x) # ,label=label)
         },x=idkin,z=names(idkin),SIMPLIFY=TRUE)
   uu <- data.frame(t(zz))
   long <- data.frame(idego=unname(unlist(uu$idego)),
                    idkin=unname(unlist(uu$idkin)))
                    # label=unname(unlist(uu$label))) 
   # long <- long[!is.na(long$idkin),]
   if (is.numeric (type.convert(names(idkin)[1],as.is=TRUE)))
       { long$idego <- type.convert(long$idego,as.is=TRUE)
       }
   
    return(long)
}