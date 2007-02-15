\name{plot.DMFA}

\alias{plot.DMFA}

\title{Make the Duale Multiple Factor Analysis (DMFA) graphs}

\description{
Plot the graphs for a Principal Component Analysis (DMFA) with supplementary individuals, supplementary quantitative 
variables and supplementary qualitative variables.
}

\usage{
plot.DMFA(x, axes = c(1, 2), choix = "ind", label="all",
    invisible = NULL, \dots)
}

\arguments{
  \item{x}{an object of class DMFA}
  \item{axes}{a length 2 vector specifying the components to plot}
  \item{choix}{the graph to plot ("ind" for the individuals, "var" for the variables)}
  \item{label}{a list of character for the elements which are labelled (by default, all the elements are labelled ("ind", ind.sup", "quali", "var", "quanti.sup"}
  \item{invisible}{string indicating if some points should not be drawn ("ind" or "quali")}
  \item{\dots}{further arguments passed to or from other methods}
}

\value{
Returns the individuals factor map and the variables factor map, the partial variables representation and the groups factor map.
}

\author{Sébastien Lê, François Husson \email{François.Husson@agrocampus-rennes.fr}}

\seealso{ \code{\link{DMFA}}}

\examples{
}

\keyword{dplot}
