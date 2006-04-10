\name{plot.PCA}

\alias{plot.PCA}

\title{Make the Principal Component Analysis (PCA) graphs}

\description{
Plot the graphs for a Principal Component Analysis (PCA) with supplementary individuals, supplementary quantitative 
variables and supplementary qualitative variables.
}

\usage{
plot.PCA(x, axes = c(1, 2), choix = "ind",
    ellipse = NULL, xlim = NULL, ylim = NULL, habillage = "none", 
    col.hab = NULL, col.ind = "black", col.ind.sup = "blue", 
    col.quali = "magenta", col.quanti.sup = "blue", 
    col.var = "black", invisible = NULL, lim.cos2.var = 0.1,
    cex = 1, title = NULL, \dots)
}

\arguments{
  \item{x}{an object of class PCA}
  \item{axes}{a length 2 vector specifying the components to plot}
  \item{choix}{the graph to plot ("ind" for the individuals, "var" for the variables)}
  \item{ellipse}{boolean (NULL by default), if not null, draw ellipses around the individuals, and use the results of \code{coord.ellipse}}
  \item{xlim}{range for the plotted 'x' values, defaulting to the range of the finite values of 'x'}
  \item{ylim}{range for the plotted 'y' values, defaulting to the range of the finite values of 'y'}
  \item{habillage}{allows to colored the individuals among a qualitative variable}
  \item{col.hab}{a vector with the color to use for the individuals}
  \item{col.ind}{a color for the individuals if there only is not habillage, if color ="none" the label is not written}
  \item{col.ind.sup}{a color for the supplementary individuals only if there is not habillage, if color ="none" the label is not written}
  \item{col.quali}{a color for the categories of qualitative variable sonly  if there is not habillage, if color ="none" the label is not written}
  \item{col.quanti.sup}{a color for the quantitative supplementary variables, if color ="none" the label is not written}
  \item{col.var}{a color for the variables, if color ="none" the label is not written}
  \item{invisible}{string indicating if some points should not be drawn ("ind" or "quali")}
  \item{lim.cos2.var}{value of the square cosinus under the variables are not drawn}
  \item{cex}{cf. function \code{\link{par}} in the \pkg{graphics} package}
  \item{title}{string corresponding to the title of the graph you draw (by default NULL and a title is chosen)}
  \item{\dots}{further arguments passed to or from other methods}
}

\value{
Returns the individuals factor map and the variables factor map.
}

\author{Jérémy Mazet, François Husson \email{François.Husson@agrocampus-rennes.fr}}

\seealso{ \code{\link{PCA}}}

\examples{
data(decathlon)
res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup = 13)
plot(res.pca, habillage = "quali", col.hab=c("green","blue"))
}

\keyword{dplot}
