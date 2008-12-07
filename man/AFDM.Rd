\name{AFDM}

\alias{AFDM}

\title{Multiple Factor Analysis for Mixed Data}

\description{
Performs Multiple Factor Analysis with both quantitative and qualitative data.
}

\usage{
AFDM (base, type, ncp = 5, graph = TRUE, sup.var = NULL, 
    ind.sup = NULL, axes = c(1,2))
}

\arguments{
  \item{base}{a data frame with \emph{n} rows (individuals) and \emph{p} columns}
  \item{type}{a vector indicating the type of each variable (the length of \code{type} must be equal 
              to the number of columns of X}
  \item{ncp}{number of dimensions kept in the results (by default 5)}
  \item{graph}{boolean, if TRUE a graph is displayed}
  \item{ind.sup}{a vector indicating the indexes of the supplementary individuals}
  \item{sup.var}{a vector indicating the indexes of the supplementary variables}
  \item{axes}{a length 2 vector specifying the components to plot}
}

\value{
Returns a list including:
  \item{eig}{a matrix containing all the eigenvalues, the percentage of variance and the cumulative percentage of variance}
  \item{link.group}{}
  \item{group}{}
  \item{ind}{a list of matrices with all the results for the individuals (coordinates, square cosine, contributions)}
  \item{quali.var}{a list of matrices with all the results for the qualitative variables (coordinates, square cosine, contributions, v.test)}
  \item{quanti.var}{a list of matrices with all the results for the quantitative variables (coordinates, correlation, square cosine, contributions)}
  \item{call}{a list with some statistics}

Returns the individuals factor map.
}


\references{
Pages J. (2004). Analyse factorielle de donnees mixtes. Revue Statistique Appliquee.  LII (4). pp. 93-111.
}

\author{Jeremy Mazet, Francois Husson \email{Francois.Husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{print.AFDM}}, \code{\link{plot.AFDM}}}

\examples{
\dontrun{
data(wine)
res.afdm = AFDM(wine[,c(1,2,30,31)],type=c("n","n","s","s"))
}
}

\keyword{multivariate}
