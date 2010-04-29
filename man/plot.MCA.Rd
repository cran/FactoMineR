\name{plot.MCA}

\alias{plot.MCA}

\title{Draw the Multiple Correspondance Analysis (MCA) graphs}

\description{
Draw the Multiple Correspondance Analysis (MCA) graphs.
}

\usage{
\method{plot}{MCA}(x, axes = c(1, 2),  choix="ind",
    xlim = NULL, ylim = NULL, invisible = NULL,
    col.ind = "blue", col.var = "red", col.quali.sup = "darkgreen",
    col.ind.sup = "darkblue", col.quanti.sup = "black",
    label = "all", cex = 1, title = NULL, habillage = "none", 
    palette = NULL, new.plot = TRUE, \dots)
}

\arguments{
  \item{x}{an object of class MCA}
  \item{axes}{a length 2 vector specifying the components to plot}
  \item{choix}{the graph to plot ("ind" for the individuals and the categories, "var" for the variables, "quanti.sup" for the supplementary quantitative variables)}
  \item{xlim}{range for the plotted 'x' values, defaulting to the range of the finite values of 'x'}
  \item{ylim}{range for the plotted 'y' values, defaulting to the range of the finite values of 'y'}
  \item{invisible}{string indicating if some points should not be drawn ("ind", "var", "ind.sup", "quali.sup", "quanti.sup")}
  \item{col.ind}{a color for the individuals, if color ="none" the label is not written}
  \item{col.var}{a color for the categories of categorical variables, if color ="none" the label is not written}
  \item{col.quali.sup}{a color for the categorical supplementary variables, if color ="none" the label is not written}
  \item{col.ind.sup}{a color for the supplementary individuals only if there is not habillage, if color ="none" the label is not written}
  \item{col.quanti.sup}{a color for the supplementary quantitative variables, if color ="none" the label is not written}
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

\author{Jeremy Mazet, Francois Husson \email{Francois.Husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{MCA}}}

\examples{
data (poison)
res.mca = MCA (poison, quali.sup = 3:4, quanti.sup = 1:2, graph=FALSE)
plot.MCA(res.mca,invisible=c("var","quali.sup"))
plot.MCA(res.mca,invisible="ind")
plot.MCA(res.mca,choix="var")
}

\keyword{dplot}
