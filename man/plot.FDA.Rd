\name{plot.FDA}
\alias{plot.FDA}

\title{ Factorial Discriminant Analysis graphs }
\description{
 Plot the graphs for a Factorial Discriminant Analysis (FDA).
}

\usage{
plot.FDA(x, choix="ind", axes=c(1,2), invisible=NULL, 
    xlim=NULL, ylim=NULL, col.grpe=NULL, lab.ind=FALSE, 
    lab.var=TRUE, lab.cg=TRUE, lab.ind.sup=FALSE, conf.elli=0.95, \dots)
}

\arguments{
  \item{x          }{an object of class FDA}
  \item{choix      }{the graph to plot ("ind" for the individuals, "var" for the variables)}
  \item{axes       }{a length 2 vector specifying the components to plot}
  \item{invisible  }{string indicating if some points should not be drawn ("ind", "cg", "elli" or "ind.sup")}
  \item{xlim       }{range for the plotted 'x' values, defaulting to the range of the finite values of 'x'}
  \item{ylim       }{range for the plotted 'y' values, defaulting to the range of the finite values of 'y'}
  \item{col.grpe   }{a vector with the color to use for the individuals}
  \item{lab.ind    }{boolean, if TRUE, the label of the individuals are drwan}
  \item{lab.var    }{boolean, if TRUE, the label of the variables are drwan}
  \item{lab.cg     }{boolean, if TRUE, the label of the centers of gravity are drwan}
  \item{lab.ind.sup}{boolean, if TRUE, the label of the supplementary individuals are drwan}
  \item{conf.elli  }{the confidence level of the ellipses}
  \item{\dots}{further arguments passed to or from other methods}
}

\author{ Jeremy Mazet \email{jeremy.mazet@soredab.org} }

\seealso{ \code{\link{FDA}}, \code{\link{print.FDA}}}

\keyword{ multivariate }
