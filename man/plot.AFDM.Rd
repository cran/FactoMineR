\name{plot.AFDM}

\alias{plot.AFDM}

\title{Draw the Multiple Factor Analysis for Mixt Data graphs}

\description{
Draw the Multiple Factor Analysis for Mixt Data graphs.
}

\usage{
\method{plot}{AFDM}(x, choix = "group", axes = c(1, 2), lab.grpe = TRUE,
    lab.var = TRUE, lab.ind = TRUE, habillage = "none", col.lab = FALSE,
    col.hab = NULL, invisible = NULL, lim.cos2.var = 0., xlim = NULL,
    ylim = NULL, cex = 1, title = NULL, palette=NULL, new.plot = TRUE, \dots)
}

\arguments{
  \item{x}{an object of class AFDM} 
  \item{choix}{a string corresponding to the graph that you want to do ("ind" for the individual or categorical
  variables graph, "group" for all the variables (quantitative and categorical), "var" for the correlation circle)}
  \item{axes}{a length 2 vector specifying the components to plot}
  \item{lab.grpe}{boolean, if TRUE, the label of the groups are drwan}
  \item{lab.var}{boolean indicating if the labelled of the variables should be drawn on the map}
  \item{lab.ind}{boolean indicating if the labelled of the individuals should be drawn on the map}
  \item{habillage}{string corresponding to the color which are used. If "ind", one color is used for each individual else if it is the name 
  or the position of a categorical variable, it colors 
  according to the different categories of this variable}
  \item{col.lab}{boolean indicating if the labelled should be colored}
  \item{col.hab}{vector indicating the colors to use to labelled the rows or columns elements chosen in habillage}
  \item{invisible}{list of string; for choix ="ind", the individuals can be omit (invisible = "ind"),
  or supplementary individuals (invisible="ind.sup") or the centerg of gravity of the categorical variables 
  (invisible= "quali"); if invisible = c("ind","ind.sup"), just the centers of gravity are drawn}
  \item{lim.cos2.var}{value of the square cosinus under the variables are not drawn}
  \item{xlim}{range for the plotted 'x' values, defaulting to the range of the finite values of 'x'}
  \item{ylim}{range for the plotted 'y' values, defaulting to the range of the finite values of 'y'}
  \item{cex}{cf. function \code{\link{par}} in the \pkg{graphics} package}
  \item{title}{string corresponding to the title of the graph you draw (by default NULL and a title is chosen)}
  \item{palette}{the color palette used to draw the points. By default colors are chosen. If you want to define the colors : palette=palette(c("black","red","blue")); or you can use: palette=palette(rainbow(30)), or in black and white for example: palette=palette(gray(seq(0,.9,len=25)))}
  \item{new.plot}{boolean, if TRUE, a new graphical device is created}
  \item{\dots}{further arguments passed to or from other methods}
}

\value{
Returns the individuals factor map and the variables factor map.
}

\author{Jeremy Mazet, Francois Husson \email{Francois.Husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{AFDM}}}

\keyword{dplot}
