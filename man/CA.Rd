\name{CA}

\alias{CA}

\title{Correspondence Analysis (CA)}

\description{
Performs Correspondence Analysis (CA) including supplementary row and/or column points.
}

\usage{
CA(X, ncp = 5, row.sup = NULL, col.sup = NULL, graph = TRUE)}

\arguments{
  \item{X}{a data frame with \emph{n} rows and \emph{p} columns, i.e. a contingency table}
  \item{ncp}{number of dimensions kept in the results (by default 5)}
  \item{row.sup}{a vector indicating the indexes of the supplementary rows}
  \item{col.sup}{a vector indicating the indexes of the supplementary columns}
  \item{graph}{boolean, if TRUE a graph is displayed}
}

\value{
Returns a list including:
  \item{eig}{a numeric vector containing all the eigenvalues}
  \item{col}{a list of matrices with all the results for the column variable (coordinates, square cosine, contributions)}
  \item{row}{a list of matrices with all the results for the row variable (coordinates, square cosine, contributions)}
  \item{col.sup}{a list of matrices containing all the results for the supplementary column points (coordinates, square cosine)}
  \item{row.sup}{a list of matrices containing all the results for the supplementary row points (coordinates, square cosine)}
  \item{call}{a list with some statistics}

Returns the row and column points factor map.
}

\references{
BENZECRI, J.-P. (1992) \emph{Correspondence Analysis Handbook}, New-York : Dekker \cr
BENZECRI, J.-P. (1980) \emph{L'analyse des données tome 2 : l'analyse des correspondances}, Paris : Bordas \cr
GREENACRE, M.J. (1993) \emph{Correspondence Analysis in Practice}, London : Academic Press

}

\author{Jérémy Mazet, François Husson \email{François.Husson@agrocampus-rennes.fr}}

\seealso{ \code{\link{print.CA}}, \code{\link{plot.CA}}}

\examples{
data(children)
res.ca <- CA (children, col.sup = 6:8, row.sup = 15:18)
}

\keyword{multivariate}
