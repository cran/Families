
#' @title Individual fertility histories of members of a virtual population. Sample dataset.
#'
#' @description Fertility histories based on period data and in the presence of mortality. 
#' The histories are simulated from age-specific death rates and conditional fertility rates of USA 2021.
#' The virtual population consists of 5 generations (with selected info on members of the sixth generation). 
#' The VirtualPop package is used to generate the population. 
#' 
#' @name dLH
#' @aliases dLH
#' @docType data
#' @usage data(dLH,package="Families")
#' @format A data frame with data of about 7,000 individuals (2000 in initial cohort). \describe{
#' \item{ID}{Identification number} 	
#' \item{gen}{Generation}
#' \item{cohort}{Birth cohort}
#' \item{sex}{Sex. A factor with levels Males and Females}
#' \item{bdated}{Date of birth (decimal date)} 
#' \item{ddated}{Date of death (decimal date)} 
#' \item{x_D}{Age at death (decimal number)} 	
#' \item{IDmother}{ID of mother} 	
#' \item{IDfather}{ID of father}
#' \item{jch}{Child's line number in the household}
#' \item{IDpartner}{ID of partner}
#' \item{udated}{Date of union formation}
#' \item{nch}{Number of children ever born to woman}}	
#'
#' @source The data for the simulation are period mortality rates by age and sex and 
#' period fertility rates by age and birth
#' order for the United States 2021. The data are downloaded from the Human
#' Mortality Database (HMD) and the Human Fertility Database (HFD). 
NULL


#' Kinship Ties in Virtual Populations
#' 
#' Tools to study kinship networks, grandparenthood, loss of close relatives and double burden
#' (presence of children and oldest old parents) in virtual population produced
#' by ’VirtualPop’.
#' 
#' 
#' @name Families-package
#' @aliases Families-package FamiliesPop
#' @docType package
#' @author Frans Willekens <Willekens@nidi.nl>
#' @keywords demography family kinship
NULL



