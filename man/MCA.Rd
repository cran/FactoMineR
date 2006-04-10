\name{MCA}

\alias{MCA}

\title{Multiple Correspondence Analysis (MCA)}

\description{
Performs Multiple Correspondence Analysis (MCA) with supplementary individuals, supplementary quantitative 
variables and supplementary qualitative variables.
}

\usage{
MCA(X, ncp = 5, ind.sup = NULL, quanti.sup = NULL, 
    quali.sup = NULL, graph = TRUE)}

\arguments{
  \item{X}{a data frame with \emph{n} rows (individuals) and \emph{p} columns (categorical variables)}
  \item{ncp}{number of dimensions kept in the results (by default 5)}
  \item{ind.sup}{a vector indicating the indexes of the supplementary individuals}
  \item{quanti.sup}{a vector indicating the indexes of the quantitative supplementary variables}
  \item{quali.sup}{a vector indicating the indexes of the qualitative supplementary variables}
  \item{graph}{boolean, if TRUE a graph is displayed}
}

\value{
Returns a list including:
  \item{eig}{a numeric vector containing all the eigenvalues}
  \item{var}{a list of matrices containing all the results for the active variables (coordinates, square cosine, contributions, v.test)}
  \item{ind}{a list of matrices containing all the results for the active individuals (coordinates, square cosine, contributions)}
  \item{ind.sup}{a list of matrices containing all the results for the supplementary individuals (coordinates, square cosine)}
  \item{quanti.sup}{a matrix containing the coordinates of the supplementary quantitative variables (the correlation between a variable and an axis is equal to the variable coordinate on the axis)}
  \item{quali.sup}{a list of matrices with all the results for the supplementary qualitative variables (coordinates of each categories of each variables, square cosine and v.test which is a criterion with a Normal distribution)}
  \item{call}{a list with some statistics}

Returns the individuals factor map and the variables factor map.
}

\author{Jérémy Mazet, François Husson \email{François.Husson@agrocampus-rennes.fr}}

\seealso{ \code{\link{print.MCA}}, \code{\link{plot.MCA}}}

\examples{
data (poison)
MCA (poison, quali.sup = 3:4, quanti.sup = 1:2)
}

\keyword{multivariate}
