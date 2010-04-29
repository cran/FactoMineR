\name{plot.PCA}

\alias{plot.PCA}

\title{Make the Principal Component Analysis (PCA) graphs}

\description{
Plot the graphs for a Principal Component Analysis (PCA) with supplementary individuals, supplementary quantitative 
variables and supplementary categorical variables.
}

\usage{
\method{plot}{PCA}(x, axes = c(1, 2), choix = "ind",
    ellipse = NULL, xlim = NULL, ylim = NULL, habillage = "none", 
    col.hab = NULL, col.ind = "black", col.ind.sup = "blue", 
    col.quali = "magenta", col.quanti.sup = "blue", 
    col.var = "black", label = "all", invisible = NULL, 
    lim.cos2.var = 0., cex = 1, title = NULL, palette=NULL,
    new.plot = TRUE, \dots)
    }

\arguments{
  \item{x}{an object of class PCA}
  \item{axes}{a length 2 vector specifying the components to plot}
  \item{choix}{the graph to plot ("ind" for the individuals, "var" for the variables)}
  \item{ellipse}{boolean (NULL by default), if not null, draw ellipses around the individuals, and use the results of \code{coord.ellipse}}
  \item{xlim}{range for the plotted 'x' values, defaulting to the range of the finite values of 'x'}
  \item{ylim}{range for the plotted 'y' values, defaulting to the range of the finite values of 'y'}
  \item{habillage}{give no color for the individuals ("none"), a color for each individual ("ind"),  or color the individuals among a categorical variable (give the number of the categorical variable)}
  \item{col.hab}{a vector with the color to use for the individuals}
  \item{col.ind}{a color for the individuals if there only is not habillage}
  \item{col.ind.sup}{a color for the supplementary individuals only if there is not habillage}
  \item{col.quali}{a color for the categories of categorical variable sonly  if there is not habillage}
  \item{col.quanti.sup}{a color for the quantitative supplementary variables}
  \item{col.var}{a color for the variables}
  \item{label}{a list of character for the elements which are labelled (by default, all the elements are labelled ("ind", ind.sup", "quali", "var", "quanti.sup"))}
  \item{invisible}{string indicating if some points should not be drawn ("ind", "ind.sup" or "quali" for the individual graph and "var" or "quanti.sup" for
  the correlation circle graph)}
  \item{lim.cos2.var}{value of the square cosinus under the variables are not drawn}
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

\seealso{ \code{\link{PCA}}}

\examples{
data(decathlon)
res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup = 13)
plot(res.pca, habillage = 13, col.hab=c("green","blue"))
## To automatically draw ellipses around the barycentres of all the categorical variables
plotellipses(res.pca)
## or another graph
aa=cbind.data.frame(decathlon[,13],res.pca$ind$coord)
bb=coord.ellipse(aa,bary=TRUE)
plot.PCA(res.pca,habillage=13,ellipse=bb)
}

\keyword{dplot}
