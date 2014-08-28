\name{summary.CaGalt}

\alias{summary.CaGalt}

\title{Printing summaries of CaGalt objects}

\description{
Printing summaries of Correspondence Analysis on Generalised Aggregated Lexical Table (CaGalt) objects
}

\usage{
\method{summary}{CaGalt}(object, nb.dec = 3, nbelements=10, nbind = nbelements, 
  ncp = 3, align.names=TRUE, file="", \ldots)
}

\arguments{
  \item{object}{an object of class CaGalt}
  \item{nb.dec}{number of decimal printed}
  \item{nbelements}{number of elements written (variables, categories, frequencies); use nbelements = Inf if you want to have all the elements}
  \item{nbind}{number of individuals written; use nbind = Inf to have the results for all the individuals and nbind = 0 if you do not want the results for individuals}
  \item{ncp}{number of dimensions printed}
  \item{align.names}{boolean, if TRUE the names of the objects are written using the same number of characters}
  \item{file}{a connection, or a character string naming the file to print to}
  \item{...}{further arguments passed to or from other methods, such as cex, cex.main, ...}
}

\author{Belchin Kostov \email{badriyan@clinic.ub.es}, Monica Becue-Bertaut, Francois Husson}

\seealso{ \code{\link{CaGalt}}}

\examples{

## Not run: 
data(health)
res.cagalt<-CaGalt(health[,1:126],health[,127:129],type="n",conf.ellip=TRUE)
summary(res.cagalt)

## End(Not run)
}

\keyword{multivariate}