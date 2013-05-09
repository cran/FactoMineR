\name{plot.spMCA}

\alias{plot.spMCA}

\title{Draw the specific Multiple Correspondence Analysis (spMCA) graphs}

\description{
Draw the specific Multiple Correspondence Analysis (spMCA) graphs.
}

\usage{
\method{plot}{spMCA}(x, axes = c(1, 2),  choix=c("ind","var","quanti.sup"),
    xlim = NULL, ylim = NULL, invisible = NULL,
    col.ind = "blue", col.var = "red", col.quali.sup = "darkgreen",
    col.ind.sup = "darkblue", col.quanti.sup = "blue",
    label = "all", cex = 1, title = NULL, habillage = "none", 
    palette = NULL, autoLab = c("auto","yes","no"), new.plot = FALSE, 
	select = NULL, selectMod = NULL, unselect = 0.7, \dots)
}

\arguments{
  \item{x}{an object of class spMCA}
  \item{axes}{a length 2 vector specifying the components to plot}
  \item{choix}{the graph to plot ("ind" for the individuals and the categories, "var" for the variables)}
  \item{xlim}{range for the plotted 'x' values, defaulting to the range of the finite values of 'x'}
  \item{ylim}{range for the plotted 'y' values, defaulting to the range of the finite values of 'y'}
  \item{invisible}{string indicating if some points should not be drawn ("ind", "var")}
  \item{col.ind}{a color for the individuals, if color ="none" the label is not written}
  \item{col.var}{a color for the categories of categorical variables, if color ="none" the label is not written}
  \item{col.quali.sup}{not used. a color for the categorical supplementary variables, if color ="none" the label is not written}
  \item{col.ind.sup}{not used. a color for the supplementary individuals only if there is not habillage, if color ="none" the label is not written}
  \item{col.quanti.sup}{not used. a color for the supplementary quantitative variables, if color ="none" the label is not written}
  \item{label}{print the labels of the points}
  \item{cex}{cf. function \code{\link{par}} in the \pkg{graphics} package}
  \item{title}{string corresponding to the title of the graph you draw (by default NULL and a title is chosen)}
  \item{habillage}{string corresponding to the color which are used. If "none", one color is used for the individual, another one for the categorical variables;
  if "quali", one color is used for each categorical variables; else if it is the position of a categorical variable, it colors 
  according to the different categories of this variable}
  \item{palette}{the color palette used to draw the points. By default colors are chosen. If you want to define the colors : palette=palette(c("black","red","blue")); or you can use: palette=palette(rainbow(30)), or in black and white for example: palette=palette(gray(seq(0,.9,len=25)))}
  \item{autoLab}{if \code{autoLab="auto"}, \code{autoLab} is equal to "yes" if there are less than 50 elements and "no" otherwise; if "yes", the labels of the drawn elements are placed in a "good" way (can be time-consuming if many elements), and if "no" the elements are placed quickly but may overlap}
  \item{new.plot}{boolean, if TRUE, a new graphical device is created}
  \item{select}{a selection of the elements that are drawn; see the details section}
  \item{selectMod}{a selection of the categories that are drawn; see the details section}
  \item{unselect}{may be either a value between 0 and 1 that gives the transparency of the unselected objects (if \code{unselect=1} the transparceny is total and the elements are not drawn, if \code{unselect=0} the elements are drawn as usual but without any label) or may be a color (for example \code{unselect="grey60"})}
  \item{\dots}{further arguments passed to or from other methods, such as cex, cex.main, ...}
}

\value{
Returns the individuals factor map and the variables factor map.
}

\author{Francois Husson \email{Francois.Husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{spMCA}}}

\examples{
data (poison)
res <- spMCA (poison[,3:8],excl=c(1,3))
plot(res,invisible="ind")
plot(res,invisible="var")
## Categories that have a cos2 less than 0.6 are not drawn
plot(res,invisible="var",selectMod="cos2 0.6")
plot(res,choix="var")
}

\keyword{dplot}
