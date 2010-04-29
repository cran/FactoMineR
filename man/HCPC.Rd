\name{HCPC}

\alias{HCPC}

\title{Hierarchical Clustering on Principle Components (HCPC)}

\description{
  Performs an agglomerative hierarchical clustering on results from a
  factor analysis. It is possible to cut the tree by clicking at the
  suggested (or an other) level. Results include paragons, description
  of the clusters, graphics.}

\usage{
HCPC(res, nb.clust=0, consol=TRUE, iter.max=10, min=3, 
  max=NULL, metric="euclidean", method="ward", order=TRUE,
  graph.scale="inertia", nb.par=5, graph=TRUE, proba=0.05, 
  cluster.CA="rows",\dots)}

\arguments{
  \item{res}{Either the result of a factor analysis, a dataframe, or a vector.}
  \item{nb.clust}{an integer. If 0, the tree is cut at the level the user clicks
  on. If -1, the tree is automatically cut at the suggested level (see
  details). If a (positive) integer, the tree is cut with nb.cluters clusters.}
  \item{consol}{a boolean. If TRUE, a k-means consolidation is performed.}
  \item{iter.max}{An integer. The maximum number of iterations for the consolidation.}
  \item{min}{an integer. The least possible number of clusters suggested.}
  \item{max}{an integer. The higher possible number of clusters suggested; by default the minimum between 10 and the number of individuals divided by 2.}
  \item{metric}{The metric used to built the tree. See \code{\link{agnes}} for details.}
  \item{method}{The method used to built the tree. See \code{\link{agnes}} for details.}
  \item{order}{A boolean. If TRUE, clusters are ordered following their center
    coordinate on the first axis.}
  \item{graph.scale}{A character string. By default "inertia" and the height of the tree corresponds
   to the inertia gain, else "sqrt-inertia" the square root of the inertia gain.}
  \item{nb.par}{An integer. The number of edited paragons.}
  \item{graph}{If TRUE, graphics are displayed. If FALSE, no graph are displayed.}
  \item{proba}{The probability used to select axes and variables in
  catdes (see \code{\link{catdes}} for details.}
  \item{cluster.CA}{A string equals to "rows" or "columns" for the clustering of Correspondence Analysis results.}
  \item{\dots}{Other arguments from other methods.}
}

\details{
  The function first built the tree with agnes. Then the sum of the
  intra-cluster inertia are calculated for each partition. The suggested
  partition is the one with the higher relative loss of inertia
  (i(clusters n+1)/i(cluster n)).
  
  The absolut loss of inertia (i(cluster n)-i(cluster n+1)) is ploted
  with the tree.  
}

\value{
  Returns a list including:
  \item{data.clust}{The original data with a supplementary row called
    class containing the partition.}
  \item{desc.axes}{The description of the classes by the factors (axes). 
     See \code{\link{catdes}} for details.}
  \item{desc.fact}{The description of the classes by the variables.
     See \code{\link{catdes}} for details.}
  \item{call}{A list or parameters and internal objects.}
  \item{ind.desc}{The paragons (para) and the more typical individuals
    of each cluster. See details.}
  
  Returns the tree and a barplot of the inertia gains, the individual
  factor map with the tree (3D), the factor map with individuals colored
  by cluster (2D).
}

\author{Guillaume Le Ray, Quentin Molto, Francois Husson \email{husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{plot.HCPC}}, \code{\link{catdes}}}

\examples{
\dontrun{
data(iris)
# Principal Component Analysis:
res.pca <- PCA(iris[,1:4], ncp=10, graph=FALSE)
# Clustering, auto nb of clusters:
res.hcpc=HCPC(res.pca, nb.clust=-1)
}
}
\keyword{multivariate}
