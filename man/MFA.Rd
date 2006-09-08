\name{MFA}

\alias{MFA}

\title{Multiple Factor Analysis (MFA)}

\description{
Performs Multiple Factor Analysis (MFA) with supplementary individuals and supplementary groups of variables.
Groups of variables can be quantitative or qualitative.
}

\usage{
MFA (base, group, type = rep("s",length(group)), ind.sup = NULL, 
    ncp = 5, name.group = NULL, num.group.sup = NULL, 
    graph = TRUE, weight.col.mfa = NULL)
}

\arguments{
  \item{base}{a data frame with \emph{n} rows (individuals) and \emph{p} columns (variables)}
  \item{group}{a list indicating the number of variables in each group}
  \item{type}{the type of variables in each group; three possibilities: "c" or "s" for quantitative variables
  (the difference is that for "s" variables are scaled to unit variance), "n" for qualitative variables; by default,
  all variables are quantitative and scaled to unit variance}
  \item{ind.sup}{a vector indicating the indexes of the supplementary individuals}
  \item{ncp}{number of dimensions kept in the results (by default 5)}
  \item{name.group}{a vector containing the name of the groups (by default, NULL and the group are named group.1, group.2 and so on)}
  \item{num.group.sup}{the indexes of the illustrative groups (by default, NULL and no group are illustrative)}
  \item{graph}{boolean, if TRUE a graph is displayed}
  \item{weight.col.mfa}{vector of weights, useful for HMFA method (by default, NULL and an MFA is performed)}
}

\value{
  \item{summary.quali}{a summary of the results for the qualitative variables}
  \item{summary.quanti}{a summary of the results for the quantitative variables}
  \item{separate.analyses}{the results for the separate analyses}
  \item{eig}{a numeric vector containing all the eigenvalues}
  \item{group}{a list of matrices containing all the results for the groups (Lg and RV coefficients, coordinates, square cosine, contributions, distance to the origin, the correlations between each group and each factor)}
  \item{rapport.inertie}{inertia ratio}
  \item{ind}{a list of matrices containing all the results for the active individuals (coordinates, square cosine, contributions)}
  \item{ind.sup}{a list of matrices containing all the results for the supplementary individuals (coordinates, square cosine)}
  \item{quanti.var}{a list of matrices containing all the results for the quantitative variables (coordinates, correlation between variables and axes)}
  \item{quali.var}{a list of matrices containing all the results for the supplementary qualitative variables (coordinates of each categories of each variables, and v.test which is a criterion with a Normal distribution)}
  \item{partial.axes}{a list of matrices containing all the results for the partial axes (coordinates, correlation between variables and axes, correlation between partial axes)}

Returns the individuals factor map, the variables factor map and the
groups factor map.
}

\references{
Escofier, B. and Pagès, J. (1994)
Multiple Factor Analysis (AFMULT package), \emph{Computational Statistics and Data Analysis}, \bold{18}, 121-140.
}

\author{Jérémy Mazet, François Husson \email{François.Husson@agrocampus-rennes.fr}}

\seealso{ \code{\link{print.MFA}}, \code{\link{plot.MFA}}}

\examples{
data(wine)
aa = MFA(wine, group=c(2,5,3,10,9,2), type=c("n",rep("s",5)),
    ncp=5, name.group=c("orig","olf","vis","olfag","gust","ens"),
    num.group.sup=c(1,6))

\dontrun{
#### Interactive graph
liste = plot.MFApartial(aa)
plot(aa,choix="ind",habillage = "Terroir")
}

###Example 2
data (poison)
MFA(poison, group=c(2,2,5,6), type=c("s","n","n","n"),
    name.group=c("desc","desc2","symptom","eat"),
    num.group.sup=1:2)
}

\keyword{multivariate}
