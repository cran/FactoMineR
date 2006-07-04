\name{print.FDA}

\alias{print.FDA}

\title{Print the Factor Discriminant Analysis (FDA) results}

\description{
Print the Factor Discriminant Analysis (FDA) results.
}

\usage{
\method{print}{FDA}(x, file = NULL, sep = ";", \dots)
}

\arguments{
  \item{x}{an object of class FDA}
  \item{file}{A connection, or a character string naming the file to print to.  If NULL (the default), the results are not printed in a file}
  \item{sep}{character string to insert between the objects to print (if the argument file is not NULL}
  \item{\dots}{further arguments passed to or from other methods}
}

\author{Jérémy Mazet, François Husson \email{François.Husson@agrocampus-rennes.fr}}

\seealso{ \code{\link{FDA}}, \code{\link{write.infile}}}

\keyword{print}
