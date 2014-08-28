\name{print.CaGalt}

\alias{print.CaGalt}

\title{Print the Correspondence Analysis on Generalised Aggregated Lexical Table (CaGalt) results}

\description{
Print the Correspondence Analysis on Generalised Aggregated Lexical Table (CaGalt) results
}

\usage{
## S3 method for class 'CaGalt'
\method{print}{CaGalt}(x, file = NULL, sep = ";", \ldots)
}

\arguments{
  \item{x}{an object of class CaGalt}
  \item{file}{A connection, or a character string naming the file to print to. If NULL (the default), the results are not printed in a file}
  \item{sep}{character string to insert between the objects to print (if the argument file is not NULL)}
  \item{...}{further arguments passed to or from other methods}
}

\author{Belchin Kostov \email{badriyan@clinic.ub.es}, Monica Becue-Bertaut, Francois Husson}

\seealso{ \code{\link{CaGalt}}, \code{\link{write.infile}}}

\examples{

## Not run: 
data(health)
res.cagalt<-CaGalt(health[,1:126],health[,127:129],type="n")
print(res.cagalt, file="health.csv", sep = ";")

## End(Not run)
}

\keyword{multivariate}