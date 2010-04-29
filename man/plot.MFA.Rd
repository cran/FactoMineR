\name{plot.MFA}

\alias{plot.MFA}

\title{Draw the Multiple Factor Analysis (MFA) graphs}

\description{
Draw the Multiple Factor Analysis (MFA) graphs.
}

\usage{
\method{plot}{MFA}(x, axes = c(1, 2), choix = "ind", ellipse = NULL, ellipse.par = NULL,
    lab.grpe = TRUE, lab.var = TRUE, lab.ind.moy = TRUE, 
    lab.par = FALSE, habillage = "ind", col.hab = NULL, 
    invisible = NULL, partial = NULL, lim.cos2.var = 0., 
    chrono = FALSE, xlim = NULL, ylim = NULL, 
    cex = 1, title = NULL, palette = NULL, new.plot = TRUE, \dots)
}

\arguments{
  \item{x}{an object of class MFA}
  \item{choix}{a string corresponding to the graph that you want to do ("ind" for the individual or categorical
  variables graph, "var" for the quantitative variables graph, "axes" for the graph of the partial axes,
  "group" for the groups representation)}
  \item{axes}{a length 2 vector specifying the components to plot}
  \item{ellipse}{boolean (NULL by default), if not null, draw ellipses around the individuals, and use the results of \code{coord.ellipse}}
  \item{ellipse.par}{boolean (NULL by default), if not null, draw ellipses around the partial individuals, and use the results of \code{coord.ellipse}}
  \item{lab.grpe}{boolean, if TRUE, the label of the groups are drwan}
  \item{lab.var}{boolean, if TRUE, the label of the variables are drwan}
  \item{lab.ind.moy}{boolean, if TRUE, the label of the mean points are drwan}
  \item{lab.par}{boolean, if TRUE, the label of the partial points are drwan}
  \item{habillage}{string corresponding to the color which are used. If "ind", one color is used for each individual;
  if "group" the individuals are colored according to the group; else if it is the name or the position of a categorical variable, it colors 
  according to the different categories of this variable}
  \item{col.hab}{the colors to use. By default, colors are chosen}
  \item{invisible}{list of string; for choix ="ind", the individuals can be omit (invisible = "ind"),
  or supplementary individuals (invisible="ind.sup") or the centerg of gravity of the categorical variables 
  (invisible= "quali"); if invisible = c("ind","ind.sup"), just the centers of gravity are drawn}
  \item{partial}{list of the individuals or of the center of gravity for which the partial points should be drawn (by
  default, partial = NULL and no partial points are drawn)}
  \item{lim.cos2.var}{value of the square cosinus under with the points are not drawn}
  \item{chrono}{boolean, if TRUE, the partial points of a same point are linked (useful when groups correspond to different moment)}
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

\seealso{ \code{\link{MFA}}}

\examples{
\dontrun{
data(wine)
aa = MFA(wine,group=c(2,5,3,10,9,2),type=c("n",rep("s",5)),ncp=5,
    name.group=c("orig","olf","vis","olfag","gust","ens"),
    num.group.sup=c(1,6),graph=FALSE)
plot(aa, choix = "ind")
plot(aa, choix = "ind", partial="all")
plot(aa, choix = "Terroir")
plot(aa, choix = "var", habillage="group")
plot(aa, choix = "axes")
}
}
\keyword{dplot}
