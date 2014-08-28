\name{mortalitySPAIN}

\alias{mortalitySPAIN}

\title{mortality in Spain (data)}

\description{
The death counts classified by main region of residence and cause for males greater than 35 years old. The mortality causes are classiFed using the international classification of diseases (ICD).
}

\usage{
data(mortalitySPAIN)
}

\format{
A data frame with 16 rows and 43 columns. Rows represent the autonomous regions (excluding Ceuta, Melilla and Canary Island), columns represent the mortality causes (columns 1 to 38) and autonomous regions' characteristics
}

\examples{

## Not run: 
data(mortalitySPAIN)
res.cagalt.quan<-CaGalt(mortalitySPAIN[,1:38],mortalitySPAIN[,39:43])

## End(Not run)
}

\keyword{multivariate}