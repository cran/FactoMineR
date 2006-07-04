\name{plot.GPApartial}

\alias{plot.GPApartial}

\title{Draw an interactive General Procrustes Analysis (GPA) map}

\description{
Draw an interactive General Procrustes Analysis (GPA) map. The graph is interactive and clicking on a point will draw the partial points,
if you click on a point for which the partial points are yet drawn, the partial points are deleted.
To stop the interactive plot, click on the title (or in the top of the graph)
}

\usage{
plot.GPApartial(x, axes = c(1, 2), 
    lab.ind.moy = TRUE, lab.par = FALSE, habillage = "ind",
    chrono = FALSE, draw.partial = NULL, 
    xlim = NULL, ylim = NULL, cex = 1, title = NULL, \dots)
}

\arguments{
  \item{x}{an object of class GPA}
  \item{axes}{a length 2 vector specifying the components to plot}
  \item{lab.ind.moy}{boolean, if TRUE, the label of the mean points are drwan}
  \item{lab.par}{boolean, if TRUE, the label of the partial points are drwan}
  \item{habillage}{string corresponding to the color which are used. If "ind", one color is used for each individual;
  if "group" the individuals are colored according to the group}
  \item{chrono}{boolean, if TRUE, the partial points of a same point are linked (useful when groups correspond to different moment)}
  \item{draw.partial}{data frame of a boolean variable for all the individuals and all the centers of gravity and with for which the partial points should be drawn (by
  default, NULL and no partial points are drawn)}
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
