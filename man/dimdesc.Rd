\name{dimdesc}

\alias{dimdesc}

\title{Dimension description}

\description{
  This function is designed to point out the variables and the categories that are the most characteristic according 
  to each dimension obtained by a Factor Analysis. \cr
}

\usage{
dimdesc(res, axes = 1:3, proba = 0.05)}

\arguments{
  \item{res}{an object of class PCA, MCA, CA, MFA or HMFA}
  \item{axes}{a vector with the dimensions to describe}
  \item{proba}{the significance threshold considered to caracterized the dimension (by default 0.05)}
}

\value{
Returns a list including:
  \item{quanti}{the description of the dimensions by the quantitative variables. The variables are sorted.}
  \item{quali}{the description of the dimensions by the qualitative variables}
}

\author{François Husson \email{François.Husson@agrocampus-rennes.fr}}

\seealso{ \code{\link{PCA}}, \code{\link{CA}}, \code{\link{MCA}}, \code{\link{MFA}}, \code{\link{HMFA}}}

\examples{
data(decathlon)
res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup=13, graph=FALSE)
dimdesc(res.pca)
}

\keyword{multivariate}
