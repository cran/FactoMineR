\name{plot.catdes}

\alias{plot.catdes}

\title{Plots for description of clusters (catdes)}

\description{
  Plots a graph from a catdes output.}

\usage{
\method{plot}{catdes}(x,col="deepskyblue",show="all",numchar=10,\dots)
}

\arguments{
  \item{x}{A catdes object, see \code{\link{catdes}} for details.}
  \item{col}{The color of the bars.}
  \item{show}{a strig. If "quali", only the categorical variables are
  used. If "quanti", only the the quantitative variables are used. If
  "all", both quali and quanti are used.}
  \item{numchar}{number of characters for the labels}
  \item{\dots}{further arguments passed to or from other methods}
  }
\value{
  Returns choosen plot.
}

\author{Guillaume Le Ray, Francois Husson \email{husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{catdes}}}

\examples{
\dontrun{
data(wine)
res.c=catdes(wine, num.var=2)
plot(res.c)
}
}

\keyword{dplot}
