#' ID of father of ego
#' 
#' Retrieves the ID of the father of ego or fathers of vector of egos. ID of the father is the
#' ID of the partner of the mother. If the ID of the father listed in the dataset d differs from 
#' the ID of the mother's partner, a warning is given. 
#' 
#' 
#' @param idego ID of ego(s)
#' @param d Name of database. If missing the dataset dLH distributed with the Families package is used.
#' @return ID of father or (if keep_ego=TRUE, dataframe with ego-father dyals: ID of ego and ID of
#' father). Returns NA if ID of father is not included in the database
#'
#' @examples
#' # Load the data
#' data(dLH,package = "Families")
#' 
#' set.seed(31)
#' idf <- IDfather (idego=sample (dLH$ID[dLH$gen>=2],10))
#' 
#' 
#' @export IDfather
IDfather <- function(idego,d=NULL)
{ # Tests
   test <- Tests(idego=idego,d=d)
  idego <- test$idego
  d <- test$d

  # ID of partner of mother
  idf1 <- IDpartner(IDmother(idego))
  if (is.na(idego[1])) idf2 <- NA else idf2 <- d$IDfather[idego]

  if (any(!is.na(idf2)) | !is.na(any(idf2!=idf1)))
      { warning("Some d$IDfather differs from IDpartner(IDmother(idego))")
      }
   idf1
  return(idf1)
}
