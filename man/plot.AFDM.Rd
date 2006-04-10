\name{plot.AFDM}

\alias{plot.AFDM}

\title{Draw the Multiple Factor Analysis for Mixt Data graphs}

\description{
Draw the Multiple Factor Analysis for Mixt Data graphs.
}

\usage{
\method{plot}{AFDM}(x, choix = "group", axes = c(1, 2), lab.grpe = TRUE,
    lab.var = TRUE, lab.ind = TRUE, habillage = "none", col.lab = FALSE,
    col.hab = NULL, invisible = NULL, lim.cos2.var = 0.1, xlim = NULL,
    ylim = NULL, \dots)
}

\arguments{
  \item{x}{an object of class AFDM} 
  \item{choix}{a string corresponding to the graph that you want to do ("ind" for the individual or qualitative
  variables graph, "var" for the quantitative variables graph, "axes" for the graph of the partial axes,
  "group" for the groups representation)}
  \item{axes}{a length 2 vector specifying the components to plot}
  \item{lab.grpe}{boolean, if TRUE, the label of the groups are drwan}
  \item{lab.var}{boolean indicating if the labelled of the variables should be drawn on the map}
  \item{lab.ind}{boolean indicating if the labelled of the individuals should be drawn on the map}
  \item{habillage}{string equal to "row" to labelled the row elements or "col" to labelled the columns elements}
  \item{col.lab}{boolean indicating if the labelled should be colored}
  \item{col.hab}{vector indicating the colors to use to labelled the rows or columns elements chosen in habillage}
  \item{invisible}{string indicating if some points should be unlabelled ("row" or "col")}
  \item{lim.cos2.var}{value of the square cosinus under the variables are not drawn}
  \item{xlim}{range for the plotted 'x' values, defaulting to the range of the finite values of 'x'}
  \item{ylim}{range for the plotted 'y' values, defaulting to the range of the finite values of 'y'}
  \item{\dots}{further arguments passed to or from other methods}
}

\value{
Returns the individuals factor map and the variables factor map.
}

\author{Jérémy Mazet, François Husson \email{François.Husson@agrocampus-rennes.fr}}

\seealso{ \code{\link{AFDM}}}

\keyword{dplot}
