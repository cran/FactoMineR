\name{children}

\alias{children}

\docType{data}

\title{Children (data)}

\description{
The data used here is a contingency table that summarizes the answers given
by different categories of people to the 
following question : according to you, 
what are the reasons that can make hesitate a woman or a couple to have 
children?
}

\usage{data(children)}

\format{
A data frame with 18 rows and 8 columns. Rows represent the different reasons mentioned,
columns represent the different categories (education, age) people belong to.
}

\source{
Traitements Statistiques des Enquêtes (D. Grangé, L.Lebart, eds.) Dunod, 1993
}

\examples{
data(children)
res.ca <- CA (children, col.sup = 6:8, row.sup = 15:18)
}

\keyword{datasets}
