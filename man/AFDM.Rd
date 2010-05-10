\name{AFDM}

\alias{AFDM}

\title{Multiple Factor Analysis for Mixed Data}

\description{
Performs Multiple Factor Analysis with both quantitative and categorical data.
}

\usage{
AFDM (base, ncp = 5, graph = TRUE, sup.var = NULL, 
    ind.sup = NULL, axes = c(1,2), row.w = NULL)
}

\arguments{
  \item{base}{a data frame with \emph{n} rows (individuals) and \emph{p} columns}
  \item{ncp}{number of dimensions kept in the results (by default 5)}
  \item{graph}{boolean, if TRUE a graph is displayed}
  \item{ind.sup}{a vector indicating the indexes of the supplementary individuals}
  \item{sup.var}{a vector indicating the indexes of the supplementary variables}
  \item{axes}{a length 2 vector specifying the components to plot}
  \item{row.w}{an optional row weights (by default, uniform row weights)}
}

\value{
Returns a list including:
  \item{eig}{a matrix containing all the eigenvalues, the percentage of variance and the cumulative percentage of variance}
  \item{group}{a matrix with the variables (continuous and categorical) considered as a group}
  \item{ind}{a list of matrices with all the results for the individuals (coordinates, square cosine, contributions)}
  \item{quali.var}{a list of matrices with all the results for the categorical variables (coordinates, square cosine, contributions, v.test)}
  \item{quanti.var}{a list of matrices with all the results for the quantitative variables (coordinates, correlation, square cosine, contributions)}
  \item{call}{a list with some statistics}

Returns the individuals factor map.
}


\references{
Pages J. (2004). Analyse factorielle de donnees mixtes. Revue Statistique Appliquee.  LII (4). pp. 93-111.
}

\author{Francois Husson \email{Francois.Husson@agrocampus-ouest.fr}, Jeremy Mazet}

\seealso{ \code{\link{print.AFDM}}, \code{\link{plot.AFDM}}}

\examples{
\dontrun{
data(wine)
res.afdm = AFDM(wine[,c(1,2,30,31)])
}
}

\keyword{multivariate}
