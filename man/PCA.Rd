\name{PCA}

\alias{PCA}

\title{Principal Component Analysis (PCA)}

\description{
Performs Principal Component Analysis (PCA) with supplementary individuals, supplementary quantitative 
variables and supplementary qualitative variables.
}

\usage{
PCA(X, scale.unit = TRUE, ncp = 5, ind.sup = NULL, 
    quanti.sup = NULL, quali.sup = NULL, row.w = NULL, 
    col.w = NULL, graph = TRUE)}

\arguments{
  \item{X}{a data frame with \emph{n} rows (individuals) and \emph{p} columns (numeric variables)}
  \item{ncp}{number of dimensions kept in the results (by default 5)}
  \item{scale.unit}{a boolean, if TRUE (value set by default) then data are scaled to unit variance}
  \item{ind.sup}{a vector indicating the indexes of the supplementary individuals}
  \item{quanti.sup}{a vector indicating the indexes of the quantitative supplementary variables}
  \item{quali.sup}{a vector indicating the indexes of the qualitative supplementary variables}
  \item{row.w}{an optional row weights (by default, uniform row weights)}
  \item{col.w}{an optional column weights (by default, uniform column weights)}
  \item{graph}{boolean, if TRUE a graph is displayed}
}

\value{
Returns a list including:
  \item{eig}{a numeric vector containing all the eigenvalues}
  \item{var}{a list of matrices containing all the results for the active variables (coordinates, correlation between variables and axes, square cosine, contributions)}
  \item{ind}{a list of matrices containing all the results for the active individuals (coordinates, square cosine, contributions)}
  \item{ind.sup}{a list of matrices containing all the results for the supplementary individuals (coordinates, square cosine)}
  \item{quanti.sup}{a list of matrices containing all the results for the supplementary quantitative variables (coordinates, correlation between variables and axes)}
  \item{quali.sup}{a list of matrices containing all the results for the supplementary qualitative variables (coordinates of each categories of each variables, and v.test which is a criterion with a Normal distribution)}

Returns the individuals factor map and the variables factor map.
}

\author{Jérémy Mazet, François Husson \email{François.Husson@agrocampus-rennes.fr}}

\seealso{ \code{\link{print.PCA}}, \code{\link{plot.PCA}}}

\examples{
data(decathlon)
res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup=13)
plot(res.pca,choix="ind",habillage=13)
}

\keyword{multivariate}
