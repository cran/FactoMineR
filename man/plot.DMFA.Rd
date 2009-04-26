\name{plot.DMFA}

\alias{plot.DMFA}

\title{Make the Duale Multiple Factor Analysis (DMFA) graphs}

\description{
Plot the graphs for a Principal Component Analysis (DMFA) with supplementary individuals, supplementary quantitative 
variables and supplementary qualitative variables.
}

\usage{
plot.DMFA(x, axes = c(1, 2), choix = "ind", label="all",
    invisible = NULL, palette = NULL, new.plot = TRUE, \dots)
}

\arguments{
  \item{x}{an object of class DMFA}
  \item{axes}{a length 2 vector specifying the components to plot}
  \item{choix}{the graph to plot ("ind" for the individuals, "var" for the variables)}
  \item{label}{a list of character for the elements which are labelled (by default, all the elements are labelled ("ind", ind.sup", "quali", "var", "quanti.sup"}
  \item{invisible}{string indicating if some points should not be drawn ("ind" or "quali")}
  \item{palette}{the color palette used to draw the points. By default colors are chosen. If you want to define the colors : palette=palette(c("black","red","blue")); or you can use: palette=palette(rainbow(30)), or in black and white for example: palette=palette(gray(seq(0,.9,len=25)))}
  \item{new.plot}{boolean, if TRUE, a new graphical device is created}
  \item{\dots}{further arguments passed to or from other methods}
}

\value{
Returns the individuals factor map and the variables factor map, the partial variables representation and the groups factor map.
}

\author{Francois Husson \email{Francois.Husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{DMFA}}}


\keyword{dplot}
