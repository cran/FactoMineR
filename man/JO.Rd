\name{JO}
\alias{JO}
\docType{data}
\title{Jeu de donnes sur nombre de medailles par pays}
\description{
Le jeu de donnees est un tableau de contingence croisant en lignes les epreuves d'athletisme et en colonnes les differents
pays. Chaque cellule contient le nombre total de medailles (or, argent et bronze) obtenues
lors des olympiades de 1992 à 2008 (Barcelone 1992, Atlanta 1996, Sydney 2000, Athenes 2004, Pekin 2008).
}
\usage{data(JO)}
\format{
Un data frame avec en lignes les 24 disciplines d'athletisme et en colonne les 58 pays ayant obtenu au moins une medaille
}
\examples{
\dontrun{
data(JO)
res.ca <- CA(JO)
res.ca <- CA(JO, axes = 3:4)
}
}
\keyword{datasets}
