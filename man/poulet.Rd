\name{poulet}
\alias{poulet}
\docType{data}
\title{Donnees genomiques sur les poulets}
\description{
Donnees genomiques sur les poulets}
\usage{data(poulet)}
\format{
  Un data frame avec 43 observations sur 7407 variables.
  \describe{
    \item{\code{Régime}}{a factor with levels \code{J16} \code{J16R16} \code{J16R5} \code{J48} \code{J48R24} \code{N}}
    \item{Autre variable}{variable quantitative correspondant a l'expression d'un gene}
  }
}
\examples{
\dontrun{
data(poulet)
res.pca = PCA(poulet,quali.sup=1, graph=FALSE)
plot(res.pca)
plot(res.pca,habillage=1,label="quali",palette=palette(c("black","red","blue","darkgreen","purple","orange")))
dimdesc(res.pca)
## Dessine des ellipses autour des centres de gravite
aa=cbind.data.frame(poulet[,1],res.pca$ind$coord)
bb=coord.ellipse(aa,bary=TRUE)
plot.PCA(res.pca,habillage=1,ellipse=bb)
}
}
\keyword{datasets}
