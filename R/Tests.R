#' Tests existence of data file. 
#' 
#' Tests existence of data file. If the data file d is null, the function checks whether dLH exists in the global environment. 
#' If it does, d=dLH. If not, the message "Object dLH is not provided and does not exist" is displayed and
#' the processing stops. 
#' If some values in idego vector are missing (are NA) a message is displayed and the missing elements are removed.
#' 
#' @param idego ID
#' @param d Name of database. If d is missing, the dataset dLH in the global environment (R workspace) is used. 
#' If no dLH in the global environment, the database dLH distributed with the Families package is used.  
#' @return idego and d
#' 
#' @export Tests
#' 
Tests <- function(idego,d=NULL)
 { # =============  Check idego  ==============
   if(!exists("idego")) stop("Tests: idego does not exist")
   # Remove NA
 #  idego <- idego[!is.na(idego)]
   if (length(idego)==0) return (list(idego=NA,d=d))  #  (numeric(0))
   if (length(idego)==1 & is.na(idego[1]))  return(list(idego=NA,d=d))
   if (any(idego[!is.na(idego)]<0)) return("idego has negative values")

   # ===========  Check the availability of d (or dLH)  =============
   if (is.null(d))
      {  # check if d exists in the .GlobalEnv. If not, return error message
         test <- exists("dLH",envir=.GlobalEnv)
         if (test)
            { d <- get("dLH",envir=.GlobalEnv)
              if (is.null(d)) stop (paste0("Error message from ",
                  "Families::Tests: Object dLH does not exist",
                  "in the global environment (workspace). Please provide dLH."))
            } else
                stop (paste0("Error message from Families::Tests: ",
                "Object dLH does not exist in the global environment ",
                "(workspace). Please provide dLH."))
      }
   # ==========   Do partners have an ID? ========
   tt <- all(is.na(d$IDpartner))
   if (tt) warning("Partners do not have an ID")
      
   aa <- list(idego=idego,d=d)
  return(aa)
}