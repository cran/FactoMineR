\name{tea}

\alias{tea}

\docType{data}

\title{tea (data)}

\description{
The data used here concern a questionnaire on tea.
We asked to 300 individuals how they drink tea (18 questions), what are their product's perception (12 questions) and some personal details (4 questions).
}

\usage{data(children)}

\format{
A data frame with 300 rows and 36 columns. Rows represent the individuals,
columns represent the different questions. The first 18 questions are active ones,
the 19th is a supplementary quantitative variable (the age) and the last variables 
are supplementary qualitative variables.
}

\examples{
data(tea)
res.mca=MCA(tea,quanti.sup=19,quali.sup=20:36)
plot(res.mca,invisible=c("var","quali.sup","quanti.sup"),cex=0.7)
plot(res.mca,invisible=c("ind","quali.sup","quanti.sup"),cex=0.8)
plot(res.mca,invisible=c("quali.sup","quanti.sup"),cex=0.8)
dimdesc(res.mca)
plotellipses(res.mca,keepvar=1:4)

## make a hierarchical clustering
res.mca=MCA(tea,quanti.sup=19,quali.sup=20:36,ncp=20,graph=FALSE)
library(cluster)
classif = agnes(res.mca$ind$coord,method="ward")
plot(classif,main="Dendrogram",ask=FALSE,which.plots=2,labels=FALSE)
clust = cutree(classif,k=3)
tea.comp = cbind.data.frame(tea,res.mca$ind$coord[,1:3],factor(clust))
## describe the clusters
catdes(tea.comp,ncol(tea.comp))
## represent the clusters*
res.aux=MCA(tea.comp,quanti.sup=c(19,37:39),quali.sup=c(20:36,40),graph=FALSE)
plot(res.aux,invisible=c("quali.sup","var","quanti.sup"),habillage=40)
}

\keyword{datasets}
