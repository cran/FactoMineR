\name{spMCA}

\alias{spMCA}

\title{Specific Multiple Correspondence Analysis (spMCA)}

\description{
Performs Specific Multiple Correspondence Analysis (spMCA) with supplementary categories and supplementary categorical variables.
}

\usage{
spMCA(X,excl=NULL,ncp=5,quali.sup=NULL,graph=TRUE)}

\arguments{
  \item{X}{a data frame with \emph{n} rows (individuals) and \emph{p} columns (categorical variables)}
  \item{excl}{a vector with the indices of the categories to exclude}
  \item{ncp}{number of dimensions kept in the results (by default 5)}
  \item{quali.sup}{a vector indicating the indexes of the categorical supplementary variables}
  \item{graph}{boolean, if TRUE a graph is displayed}
}

\value{
Returns a list including:
  \item{eig}{a matrix containing all the eigenvalues, the percentage of variance and the cumulative percentage of variance}
  \item{ind}{a list of matrices containing all the results for the active individuals (coordinates, contributions)}
  \item{var}{a list of matrices containing all the results for the active categories (coordinates, contribution, square cosine, v.test, eta2, contribution of the variables)}
  \item{quali.sup}{a list of matrices containing all the results for the supplementary categories (coordinates, square cosine, v.test)}
  \item{call}{a list with some statistics}

Returns the graphs of the individuals and categories and the graph with the variables.
}

\references{
Le Roux, B. and H. Rouanet (2010). \emph{Multiple Correspondence Analysis}, vol. 163: Sage
}

\author{Francois Husson \email{husson@agrocampus-ouest.fr}, }

\seealso{\code{\link{print.spMCA}}, \code{\link{plot.spMCA}}}

\examples{
\dontrun{
data (poison)
res <- spMCA (poison[,3:8],excl=c(1,3))
}
}
\keyword{multivariate}
