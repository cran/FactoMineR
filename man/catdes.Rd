\name{catdes}

\alias{catdes}

\title{Categories description}

\description{
Description of the categories of one factor by qualitative variables and/or by quantitative variables
}

\usage{
catdes(donnee,num.var,proba = 0.05)}

\arguments{
  \item{donnee}{a data frame made up of at least one qualitative variables and a set of quantitative variables (sensory descriptors)}
  \item{num.var}{the indice of the variable to caracterized}
  \item{proba}{the significance threshold considered to caracterized the category (by default 0.05)}
}

\value{
Returns a list including:
  \item{test.chi}{The qualitative variables which characterized the factor are listed in ascending order (from the one which characterized the most
  the factor to the one which significantly characterized with the proba \code{proba}}
  \item{category}{description of the each category of the \code{num.var} by each category of all the qualitative variables}
  \item{quanti}{the description of the each category of the \code{num.var} variable by the quantitative variables.}
}

\author{François Husson \email{François.Husson@agrocampus-rennes.fr}}

\seealso{ }

\references{
Lebart, L., Morineau, A. and Piron, M. (1995)
Statistique exploratoire multidimensionnelle, \emph{Dunod}.
}

\examples{
data(wine)
catdes(wine, num.var=2)
}

\keyword{multivariate}
