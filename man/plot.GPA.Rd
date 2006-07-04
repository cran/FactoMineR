\name{plot.GPA}

\alias{plot.GPA}

\title{Draw the General Procrustes Analysis (GPA) map}

\description{
Draw the General Procrustes Analysis (GPA) map.
}

\usage{
plot.GPA(x, axes = c(1, 2), 
    lab.ind.moy = TRUE, lab.par = FALSE, habillage = "ind",
    partial = "all", chrono = FALSE,
    xlim = NULL, ylim = NULL, cex = 1, title = NULL, \dots)
}

\arguments{
  \item{x}{an object of class GPA}
  \item{axes}{a length 2 vector specifying the components to plot}
  \item{lab.ind.moy}{boolean, if TRUE, the label of the mean points are drwan}
  \item{lab.par}{boolean, if TRUE, the label of the partial points are drwan}
  \item{habillage}{string corresponding to the color which are used. If "ind", one color is used for each individual;
  if "group" the individuals are colored according to the group}
  \item{partial}{list of the individuals or of the center of gravity for which the partial points should be drawn (by
  default, partial = "none" and no partial points are drawn)}
  \item{chrono}{boolean, if TRUE, the partial points of a same point are linked (useful when groups correspond to different moment)}
  \item{xlim}{range for the plotted 'x' values, defaulting to the range of the finite values of 'x'}
  \item{ylim}{range for the plotted 'y' values, defaulting to the range of the finite values of 'y'}
  \item{cex}{cf. function \code{\link{par}} in the \pkg{graphics} package}
  \item{title}{string corresponding to the title of the graph you draw (by default NULL and a title is chosen)}
  \item{\dots}{further arguments passed to or from other methods}
}

\value{
Returns the General Procrustes Analysis map.
}

\author{Elisabeth Morand, François Husson \email{François.Husson@agrocampus-rennes.fr}}

\seealso{ \code{\link{GPA}}}

\keyword{dplot}
