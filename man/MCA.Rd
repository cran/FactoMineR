\name{MCA}

\alias{MCA}

\title{Multiple Correspondence Analysis (MCA)}

\description{
Performs Multiple Correspondence Analysis (MCA) with supplementary individuals, supplementary quantitative 
variables and supplementary categorical variables.\cr
Missing values are treated as an additional level, categories which are rare can be ventilated
}

\usage{
MCA(X, ncp = 5, ind.sup = NULL, quanti.sup = NULL, 
    quali.sup = NULL, graph = TRUE, level.ventil = 0, 
    axes = c(1,2), row.w = NULL,na.method="NA", tab.disj=NULL)}

\arguments{
  \item{X}{a data frame with \emph{n} rows (individuals) and \emph{p} columns (categorical variables)}
  \item{ncp}{number of dimensions kept in the results (by default 5)}
  \item{ind.sup}{a vector indicating the indexes of the supplementary individuals}
  \item{quanti.sup}{a vector indicating the indexes of the quantitative supplementary variables}
  \item{quali.sup}{a vector indicating the indexes of the categorical supplementary variables}
  \item{graph}{boolean, if TRUE a graph is displayed}
  \item{level.ventil}{a number corresponding to the level under which the category is ventilated; by default, 0 and no ventilation is done}
  \item{axes}{a length 2 vector specifying the components to plot}
  \item{row.w}{an optional row weights (by default, uniform row weights)}
  \item{na.method}{a string corresponding to the name of the method used if there are missing values; available methods are "NA" or "Average" (by default, "NA")}
  \item{tab.disj}{optional data.frame corresponding to the disjunctive table used for the analysis; it corresponds to a disjunctive table obtained from imputation method (see package missMDA).}
}

\value{
Returns a list including:
  \item{eig}{a matrix containing all the eigenvalues, the percentage of variance and the cumulative percentage of variance}
  \item{var}{a list of matrices containing all the results for the active variables (coordinates, square cosine, contributions, v.test)}
  \item{ind}{a list of matrices containing all the results for the active individuals (coordinates, square cosine, contributions)}
  \item{ind.sup}{a list of matrices containing all the results for the supplementary individuals (coordinates, square cosine)}
  \item{quanti.sup}{a matrix containing the coordinates of the supplementary quantitative variables (the correlation between a variable and an axis is equal to the variable coordinate on the axis)}
  \item{quali.sup}{a list of matrices with all the results for the supplementary categorical variables (coordinates of each categories of each variables, square cosine and v.test which is a criterion with a Normal distribution)}
  \item{call}{a list with some statistics}

Returns the graphs of the individuals and categories and the graph with the variables.
}

\author{Francois Husson \email{husson@agrocampus-ouest.fr}, Julie Josse, Jeremy Mazet}

\seealso{ \code{\link{plotellipses}},\code{\link{print.MCA}}, \code{\link{plot.MCA}}, \code{\link{dimdesc}}}

\examples{
\dontrun{
 data(tea)
 res.mca=MCA(tea,quanti.sup=19,quali.sup=20:36)
 plot(res.mca,invisible=c("var","quali.sup","quanti.sup"),cex=0.7)
 plot(res.mca,invisible=c("ind","quali.sup","quanti.sup"),cex=0.8)
 plot(res.mca,invisible=c("quali.sup","quanti.sup"),cex=0.8)
 dimdesc(res.mca)
 plotellipses(res.mca,keepvar=1:4)

## Example with missing values : use the missMDA package
require(missMDA)
data(vnf.example)
tab.disj.comp <- imputeMCA(vnf.example,ncp=2)
res.mca <- MCA(vnf.example,tab.disj=tab.disj.comp)
}
}
\keyword{multivariate}
