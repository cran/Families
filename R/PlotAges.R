#' Plots age distribution(s) of kin
#' 
#' Plots one or more age distributions of kin (e.g. males, females; mothers, grandmothers)
#' 
#' 
#' @param d Dataframe with at least the following columns: 
#' \itemize{
#'  \item ID
#'  \item Age
#'  \item Case. Case is the population category for which age distribution should be drawn (e.g. male, female).
#' } 
#' @param xmin Minimum age to be displayed on x-axis
#' @param xmax Maximum age to be displayed on x-axis
#' @param ymax Maximum value on y-axis (minimum is 0)
#' @param legendPos Position of legend (position is indicated according to ggplot2 rule)
#' @return 
#' \item{p}{syntax to plot age distribution(s)}
#' 
#' @examples
#' # Load data
#' data(dLH,package="Families")
#' # Age of mother at birth of a child
#' idego=dLH$ID[dLH$gen==1 & dLH$sex=="Female"]
#' idch <- IDch(idego,d=dLH)
#' agem <- dLH$bdated[idch]  - dLH$bdated[IDmother(idch,d=dLH)]
#' dm <- data.frame(idego=IDmother(idch),Age=agem)
#' dm$Case <- "Motherhood"
#' 
#' # Age at grandmotherhood
#' idgch <- IDch(IDch(idego,d=dLH),d=dLH)
#' agegm <- dLH$bdated[idgch] - dLH$bdated[IDmother(IDmother(idgch,d=dLH),d=dLH)]
#' dgm <- data.frame(idego=IDmother(IDmother(idgch,d=dLH),d=dLH),Age=agegm)
#' dgm$Case <- "Grandmotherhood"
#' d <- rbind (dm,dgm)
#' d <- d[!is.na(d$Age),]
#' binwidth <- (max(d$Age,na.rm=TRUE)-min(d$Age,na.rm=TRUE))/60
#' cas <- unique(d$Case)
#' d$Case <- factor(d$Case,levels=cas,labels=cas,ordered=TRUE)
#' library(ggplot2)
#' p <- PlotAges(d)
#' 
#' @export PlotAges

PlotAges <- function(d,xmin=NULL,xmax=NULL,ymax=NULL,legendPos=NULL)
{
if (is.null(xmin)) xmin <- 10
if (is.null(xmax)) xmax <- 110
if (is.null(ymax)) ymax <- 0.07
d$Age[d$Age>xmax]  <- NA
d <- subset (d,!is.na(d$Age))
binwidth <- (max(d$Age,na.rm=TRUE)-min(d$Age,na.rm=TRUE))/60
p <- ggplot2::ggplot(data=d, aes(x=Age,color = .data$Case,fill = .data$Case)) +
  ggplot2::geom_histogram(aes(y=after_stat(density)), alpha=0.5, 
                 position="identity",binwidth=binwidth) +
  ggplot2::scale_y_continuous (breaks=seq(0,ymax,by=0.01)) +
  ggplot2::geom_density(alpha=0.2,linewidth=0.5,colour="blue") 
#   # .data[[case]] see 
#   https://cran.r-project.org/web/packages/ggplot2/vignettes/ggplot2-in-packages.html
p <- p + ggplot2::scale_fill_hue(c=45, l=70)
p <- p +  ggplot2::scale_x_continuous(breaks=seq(xmin,xmax,by=5)) 
p <- p + ggplot2::xlab("Age")
if (is.null(legendPos)) 
                 {if (mean(d$Age)>50) xx <- 0.1 else xx <- 0.9 
                  yy <- 0.85
                 } else
                 {xx <- legendPos[1]
                  yy <- legendPos[2]
                 }
p <- p + ggplot2::theme(legend.position = c(xx,yy))
return(p)
}