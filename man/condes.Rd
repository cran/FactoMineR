\name{condes}

\alias{condes}

\title{Continuous variable description}

\description{
Description continuous by quantitative variables and/or by qualitative variables
}

\usage{
condes(donnee,num.var,proba = 0.05)}

\arguments{
  \item{donnee}{a data frame made up of at least one quantitative variable and a set of quantitative variables and/or qualitative variables}
  \item{num.var}{the indice of the variable to caracterized}
  \item{proba}{the significance threshold considered to caracterized the category (by default 0.05)}
}

\value{
Returns a list including:
  \item{quanti}{the description of the \code{num.var} variable by the quantitative variables. The variables are sorted in ascending order  (from the one which characterized the most to the one which significantly characterized with the proba \code{proba})}
  \item{quali}{The qualitative variables which characterized the continuous variables are listed in ascending order}
  \item{category}{description of the continuous variable \code{num.var} by each category of all the qualitative variables}
}

\author{Francois Husson \email{Francois.Husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{catdes}}}

\examples{
data(decathlon)
condes(decathlon, num.var=3)
}

\keyword{multivariate}
