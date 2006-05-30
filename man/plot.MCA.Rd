\name{plot.MCA}

\alias{plot.MCA}

\title{Draw the Multiple Correspondance Analysis (CA) graphs}

\description{
Draw the Multiple Correspondance Analysis (CA) graphs.
}

\usage{
\method{plot}{MCA}(x, axes = c(1, 2), 
    xlim = NULL, ylim = NULL, invisible = NULL,
    col.ind = "blue", col.var = "red", col.quali.sup = "darkred",
    col.ind.sup = "darkblue", label = "all", cex = 1, title = NULL, \dots)
}

\arguments{
  \item{x}{an object of class CA}
  \item{axes}{a length 2 vector specifying the components to plot}
  \item{xlim}{range for the plotted 'x' values, defaulting to the range of the finite values of 'x'}
  \item{ylim}{range for the plotted 'y' values, defaulting to the range of the finite values of 'y'}
  \item{invisible}{string indicating if some points should not be drawn ("ind" or "var")}
  \item{col.ind}{a color for the individuals, if color ="none" the label is not written}
  \item{col.var}{a color for the categories of qualitative variables, if color ="none" the label is not written}
  \item{col.quali.sup}{a color for the qualitative supplementary variables, if color ="none" the label is not written}
  \item{col.ind.sup}{a color for the supplementary individuals only if there is not habillage, if color ="none" the label is not written}
  \item{label}{print the labels of the points}
  \item{cex}{cf. function \code{\link{par}} in the \pkg{graphics} package}
  \item{title}{string corresponding to the title of the graph you draw (by default NULL and a title is chosen)}
  \item{\dots}{further arguments passed to or from other methods}
}

\value{
Returns the individuals factor map and the variables factor map.
}

\author{Jérémy Mazet, François Husson \email{François.Husson@agrocampus-rennes.fr}}

\seealso{ \code{\link{MCA}}}

\keyword{dplot}
