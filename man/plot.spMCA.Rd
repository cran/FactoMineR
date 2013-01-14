\name{plot.spMCA}

\alias{plot.spMCA}

\title{Draw the specific Multiple Correspondence Analysis (spMCA) graphs}

\description{
Draw the specific Multiple Correspondence Analysis (spMCA) graphs.
}

\usage{
\method{plot}{spMCA}(x, axes = c(1, 2),  choix="ind",
    xlim = NULL, ylim = NULL, invisible = NULL,
    col.ind = "blue", col.var = "red", col.quali.sup = "darkgreen",
    col.ind.sup = "darkblue", col.quanti.sup = "blue",
    label = "all", cex = 1, title = NULL, habillage = "none", 
    palette = NULL, new.plot = FALSE, \dots)
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
  \item{new.plot}{boolean, if TRUE, a new graphical device is created}
  \item{\dots}{further arguments passed to or from other methods}
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
plot(res,choix="var")
}

\keyword{dplot}
