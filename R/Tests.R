#' Checks the input data
#' 
#' Tests existence of data file and identifies anomalies. If the data file d is null, 
#' the function checks whether dLH exists in the global environment. 
#' If it does, d=dLH. If not, the message "Object dLH is not provided and does not exist" 
#' is displayed and the processing stops. If some values in idego vector are 
#' missing (are NA) a message is displayed 
#' and the missing elements are removed.
#'  
#' @details 
#' The function Tests() is called in functions starting with ID, e.g. IDmother(). If
#' the idego vector has values outside the acceptable range or missing values (NA), 
#' these outvalues are omitted and Tests gives a warning. The warning is not 
#' exported to the function calling Tests(). It is recommended to call Tests() before
#' any other function call. 
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
   # ==========   Check that idego is included in dLH  ====
   war <- NA
   options(scipen=999)
   if(any(is.na(idego)) | any(!idego%in%d$ID)) warning(paste(c(
           paste0("From Tests: The following elements of the idego ",
           "vector are NA or outside of the acceptable range from ",
           min(d$ID)," to ",max(d$ID)," and are removed: "),
           which(is.na(idego) | !idego%in%d$ID | idego < 0)),collapse=" "))
   idego[!idego%in%d$ID] <- NA
   # ==========   Do partners have an ID? ========
   tt <- all(is.na(d$IDpartner))
   if (tt) warning("Partners IDs are not provided")
      
   aa <- list(idego=idego[!is.na(idego)],d=d)
  return(aa)
}