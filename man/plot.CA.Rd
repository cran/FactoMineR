\name{plot.CA}

\alias{plot.CA}

\title{Draw the Correspondance Analysis (CA) graphs}

\description{
Draw the Correspondance Analysis (CA) graphs.
}

\usage{
plot.CA(x, axes = c(1, 2),
    xlim = NULL, ylim = NULL, invisible = NULL, col.row = "blue",
    col.col = "red", col.row.sup = "darkblue", 
    col.col.sup = "darkred", label = "all", cex = 1, 
    title = NULL, \dots)
}

\arguments{
  \item{x}{an object of class CA}
  \item{axes}{a length 2 vector specifying the components to plot}
  \item{xlim}{range for the plotted 'x' values, defaulting to the range of the finite values of 'x'}
  \item{ylim}{range for the plotted 'y' values, defaulting to the range of the finite values of 'y'}
  \item{invisible}{string indicating if some points should be unlabelled ("row", "col", "row.sup", "col.sup")}
  \item{col.row}{a color for the rows points}
  \item{col.col}{a color for columns points}
  \item{col.row.sup}{a color for the supplementary rows points}
  \item{col.col.sup}{a color for supplementary columns points}
  \item{label}{a list of character for the elements which are labelled (by default, all the elements are labelled ("row", "row.sup", "col", "col.sup"}
  \item{cex}{cf. function \code{\link{par}} in the \pkg{graphics} package}
  \item{title}{string corresponding to the title of the graph you draw (by default NULL and a title is chosen)}
  \item{\dots}{further arguments passed to or from other methods}
}

\value{
Returns the individuals factor map and the variables factor map.
}

\author{Jérémy Mazet, François Husson \email{François.Husson@agrocampus-rennes.fr}}

\seealso{ \code{\link{CA}}}

\examples{
data(children)
res.ca <- CA (children, col.sup = 6:8, row.sup = 15:18)
}

\keyword{dplot}
