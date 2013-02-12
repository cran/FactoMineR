pkgname <- "FactoMineR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('FactoMineR')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("AFDM")
### * AFDM

flush(stderr()); flush(stdout())

### Name: AFDM
### Title: Factor Analysis for Mixed Data
### Aliases: AFDM
### Keywords: multivariate

### ** Examples

## Not run: 
##D data(geomorphology)
##D res.afdm = AFDM(geomorphology)
##D 
##D data(wine)
##D res.afdm = AFDM(wine[,c(1,2,30,31)])
## End(Not run)



cleanEx()
nameEx("AovSum")
### * AovSum

flush(stderr()); flush(stdout())

### Name: AovSum
### Title: Analysis of variance with the contrasts sum (the sum of the
###   coefficients is 0)
### Aliases: AovSum
### Keywords: models

### ** Examples

## Example two-way anova
data(senso)
res = AovSum (Score~ Product + Day , data=senso)
res

## Example two-way anova with interaction
data(senso)
res2 = AovSum (Score~ Product + Day + Product : Day, data=senso)
res2

## Example ancova
data(footsize)
res3 = AovSum (footsize ~ size + sex + size : sex, data=footsize)
res3



cleanEx()
nameEx("CA")
### * CA

flush(stderr()); flush(stdout())

### Name: CA
### Title: Correspondence Analysis (CA)
### Aliases: CA
### Keywords: multivariate

### ** Examples

data(children)
res.ca <- CA (children, col.sup = 6:8, row.sup = 15:18)



cleanEx()
nameEx("DMFA")
### * DMFA

flush(stderr()); flush(stdout())

### Name: DMFA
### Title: Dual Multiple Factor Analysis (DMFA)
### Aliases: DMFA
### Keywords: multivariate

### ** Examples

## Example with the famous Fisher's iris data
res.dmfa = DMFA ( iris, num.fact = 5)



cleanEx()
nameEx("HCPC")
### * HCPC

flush(stderr()); flush(stdout())

### Name: HCPC
### Title: Hierarchical Clustering on Principle Components (HCPC)
### Aliases: HCPC
### Keywords: multivariate

### ** Examples

## Not run: 
##D data(iris)
##D # Principal Component Analysis:
##D res.pca <- PCA(iris[,1:4], graph=FALSE)
##D # Clustering, auto nb of clusters:
##D hc <- HCPC(res.pca, nb.clust=-1)
##D 
##D ### Construct a hierarchical tree from a partition (with 10 clusters)
##D ### (useful when the number of individuals is very important)
##D hc2 <- HCPC(iris[,1:4], kk=10, nb.clust=-1)
## End(Not run)



cleanEx()
nameEx("HMFA")
### * HMFA

flush(stderr()); flush(stdout())

### Name: HMFA
### Title: Hierarchical Multiple Factor Analysis
### Aliases: HMFA
### Keywords: multivariate

### ** Examples
 
data(wine)
hierar <- list(c(2,5,3,10,9,2), c(4,2))
res.hmfa <- HMFA(wine, H = hierar, type=c("n",rep("s",5)))



cleanEx()
nameEx("JO")
### * JO

flush(stderr()); flush(stdout())

### Name: JO
### Title: Number of medals in athletism during olympic games per country
### Aliases: JO
### Keywords: datasets

### ** Examples

## Not run: 
##D data(JO)
##D res.ca <- CA(JO)
##D res.ca <- CA(JO, axes = 3:4)
## End(Not run)



cleanEx()
nameEx("MCA")
### * MCA

flush(stderr()); flush(stdout())

### Name: MCA
### Title: Multiple Correspondence Analysis (MCA)
### Aliases: MCA
### Keywords: multivariate

### ** Examples

## Not run: 
##D ## Tea example
##D  data(tea)
##D  res.mca=MCA(tea,quanti.sup=19,quali.sup=20:36)
##D  plot(res.mca,invisible=c("var","quali.sup","quanti.sup"),cex=0.7)
##D  plot(res.mca,invisible=c("ind","quali.sup","quanti.sup"),cex=0.8)
##D  plot(res.mca,invisible=c("quali.sup","quanti.sup"),cex=0.8)
##D  dimdesc(res.mca)
##D  plotellipses(res.mca,keepvar=1:4)
##D  plotellipses(res.mca,keepvar="Tea")
##D 
##D ## Hobbies example
##D data(hobbies)
##D res.mca <- MCA(hobbies,quali.sup=19:22,quanti.sup=23)
##D plot(res.mca,invisible=c("ind","quali.sup"),hab="quali") 
##D plot(res.mca,invisible=c("var","quali.sup"),cex=.5,label="none") 
##D plot(res.mca,invisible=c("ind","var"),hab="quali")
##D dimdesc(res.mca)
##D plotellipses(res.mca,keepvar=1:4)
##D 
##D ## Example with missing values : use the missMDA package
##D require(missMDA)
##D data(vnf.example)
##D completed <- imputeMCA(vnf.example,ncp=2)
##D res.mca <- MCA(vnf.example,tab.disj=completed$tab.disj)
## End(Not run)



cleanEx()
nameEx("MFA")
### * MFA

flush(stderr()); flush(stdout())

### Name: MFA
### Title: Multiple Factor Analysis (MFA)
### Aliases: MFA
### Keywords: multivariate

### ** Examples

data(wine)
res = MFA(wine, group=c(2,5,3,10,9,2), type=c("n",rep("s",5)),
    ncp=5, name.group=c("orig","olf","vis","olfag","gust","ens"),
    num.group.sup=c(1,6))
barplot(res$eig[,1],main="Eigenvalues",names.arg=1:nrow(res$eig))

## Not run: 
##D #### Confidence ellipses around categories per variable
##D plotellipses(res)
##D plotellipses(res,keepvar="Label") ## for 1 variable
##D 
##D #### Interactive graph
##D liste = plotMFApartial(res)
##D plot(res,choix="ind",habillage = "Terroir")
##D 
##D ###Example with groups of categorical variables
##D data (poison)
##D MFA(poison, group=c(2,2,5,6), type=c("s","n","n","n"),
##D     name.group=c("desc","desc2","symptom","eat"),
##D     num.group.sup=1:2)
##D 
##D ###Example with groups of frequency tables
##D data(mortality)
##D res<-MFA(mortality,group=c(9,9),type=c("f","f"),
##D     name.group=c("1979","2006"))
## End(Not run)



cleanEx()
nameEx("PCA")
### * PCA

flush(stderr()); flush(stdout())

### Name: PCA
### Title: Principal Component Analysis (PCA)
### Aliases: PCA
### Keywords: multivariate

### ** Examples

data(decathlon)
res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup=13)
## plot of the eigenvalues
## barplot(res.pca$eig[,1],main="Eigenvalues",names.arg=1:nrow(res.pca$eig))
plot(res.pca,choix="ind",habillage=13)
dimdesc(res.pca, axes = 1:2)
## To draw ellipses around the categories of the 13th variable (which is categorical)
plotellipses(res.pca,13)

## Example with missing data
## use package missMDA
## Not run: 
##D require(missMDA)
##D data(orange)
##D nb <- estim_ncpPCA(orange,ncp.min=0,ncp.max=5,method.cv="Kfold",nbsim=50)
##D imputed <- imputePCA(orange,ncp=nb$ncp)
##D res.pca <- PCA(imputed$completeObs)
## End(Not run)



cleanEx()
nameEx("RegBest")
### * RegBest

flush(stderr()); flush(stdout())

### Name: RegBest
### Title: Select variables in multiple linear regression
### Aliases: RegBest
### Keywords: models

### ** Examples

data(milk)
res = RegBest(y=milk[,6],x=milk[,-6])
res$best



cleanEx()
nameEx("catdes")
### * catdes

flush(stderr()); flush(stdout())

### Name: catdes
### Title: Categories description
### Aliases: catdes
### Keywords: multivariate

### ** Examples

data(wine)
catdes(wine, num.var=2)



cleanEx()
nameEx("children")
### * children

flush(stderr()); flush(stdout())

### Name: children
### Title: Children (data)
### Aliases: children
### Keywords: datasets

### ** Examples

data(children)
res.ca <- CA (children, col.sup = 6:8, row.sup = 15:18)



cleanEx()
nameEx("coeffRV")
### * coeffRV

flush(stderr()); flush(stdout())

### Name: coeffRV
### Title: Calculate the RV coefficient and test its significance
### Aliases: coeffRV
### Keywords: multivariate

### ** Examples

data(wine)
X <- wine[,3:7]
Y <- wine[,11:20]
coeffRV(X,Y)



cleanEx()
nameEx("condes")
### * condes

flush(stderr()); flush(stdout())

### Name: condes
### Title: Continuous variable description
### Aliases: condes
### Keywords: multivariate

### ** Examples

data(decathlon)
condes(decathlon, num.var=3)



cleanEx()
nameEx("decathlon")
### * decathlon

flush(stderr()); flush(stdout())

### Name: decathlon
### Title: Performance in decathlon (data)
### Aliases: decathlon
### Keywords: datasets

### ** Examples

data(decathlon)
res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup=13)



cleanEx()
nameEx("desfreq")
### * desfreq

flush(stderr()); flush(stdout())

### Name: descfreq
### Title: Description of frequencies
### Aliases: descfreq
### Keywords: multivariate

### ** Examples

data(children)
descfreq(children[1:14,1:5])    ## desc of rows
descfreq(t(children[1:14,1:5])) ## desc of columns



cleanEx()
nameEx("dimdesc")
### * dimdesc

flush(stderr()); flush(stdout())

### Name: dimdesc
### Title: Dimension description
### Aliases: dimdesc
### Keywords: multivariate

### ** Examples

data(decathlon)
res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup=13, graph=FALSE)
dimdesc(res.pca)



cleanEx()
nameEx("estim_ncp")
### * estim_ncp

flush(stderr()); flush(stdout())

### Name: estim_ncp
### Title: Estimate the number of components in Principal Component
###   Analysis
### Aliases: estim_ncp
### Keywords: multivariate

### ** Examples

data(decathlon)
nb.dim <- estim_ncp(decathlon[,1:10],scale=TRUE)



cleanEx()
nameEx("footsize")
### * footsize

flush(stderr()); flush(stdout())

### Name: footsize
### Title: footsize
### Aliases: footsize
### Keywords: datasets

### ** Examples


data(footsize)
res3 <- AovSum (footsize ~ size + sex + size :sex, data=footsize)
res3



cleanEx()
nameEx("geomorphology")
### * geomorphology

flush(stderr()); flush(stdout())

### Name: geomorphology
### Title: geomorphology(data)
### Aliases: geomorphology
### Keywords: datasets

### ** Examples

data(geomorphology)
res.afdm <- AFDM(geomorphology)
plot(res.afdm,choix="ind",habillage=4)



cleanEx()
nameEx("gpa")
### * gpa

flush(stderr()); flush(stdout())

### Name: GPA
### Title: Generalised Procrustes Analysis
### Aliases: GPA
### Keywords: multivariate

### ** Examples

## Not run: 
##D data(wine)
##D res.gpa <- GPA(wine[,-(1:2)], group=c(5,3,10,9,2),
##D     name.group=c("olf","vis","olfag","gust","ens"))
##D 
##D ### If you want to construct the partial points for some individuals only
##D plotGPApartial (res.gpa)
## End(Not run)



cleanEx()
nameEx("graph.var")
### * graph.var

flush(stderr()); flush(stdout())

### Name: graph.var
### Title: Make graph of variables
### Aliases: graph.var
### Keywords: dplot

### ** Examples

data(decathlon)
res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup = 13, graph = FALSE)
graph.var (res.pca, draw = c("var","Points"), 
    label = c("Long.jump", "Points"))



cleanEx()
nameEx("hobbies")
### * hobbies

flush(stderr()); flush(stdout())

### Name: hobbies
### Title: hobbies (data)
### Aliases: hobbies
### Keywords: datasets

### ** Examples

data(hobbies)
## Not run: 
##D res.mca <- MCA(hobbies,quali.sup=19:22,quanti.sup=23,method="Burt")
##D plot(res.mca,invisible=c("ind","quali.sup"),hab="quali") ### active var. only
##D plot(res.mca,invisible=c("var","quali.sup"),cex=.5,label="none") ### individuals only
##D plot(res.mca,invisible=c("ind","var"),hab="quali") ### supp. qualitative var. only
##D 
##D dimdesc(res.mca)
##D plotellipses(res.mca,keepvar=1:4)
## End(Not run)




cleanEx()
nameEx("milk")
### * milk

flush(stderr()); flush(stdout())

### Name: milk
### Title: milk
### Aliases: milk
### Keywords: datasets

### ** Examples


data(milk)
res = RegBest(y=milk[,6],x=milk[,-6])
res$best



cleanEx()
nameEx("mortality")
### * mortality

flush(stderr()); flush(stdout())

### Name: mortality
### Title: The cause of mortality in France in 1979 and 2006
### Aliases: mortality
### Keywords: datasets

### ** Examples

data(mortality)

## Not run: 
##D res<-MFA(mortality,group=c(9,9),type=c("f","f"),
##D     name.group=c("1979","2006"))
##D 
##D plot(res,choix="freq",invisible="ind",habillage="group")
##D lines(res$freq$coord[1:9,1],mfa$freq$coord[1:9,2],col="red")
##D lines(res$freq$coord[10:18,1],mfa$freq$coord[10:18,2],col="green")
##D     
##D     
## End(Not run)



cleanEx()
nameEx("plot.AFDM")
### * plot.AFDM

flush(stderr()); flush(stdout())

### Name: plot.AFDM
### Title: Draw the Multiple Factor Analysis for Mixt Data graphs
### Aliases: plot.AFDM
### Keywords: dplot

### ** Examples

data(geomorphology)
res.afdm <- AFDM(geomorphology)
plot(res.afdm,choix="ind",habillage=4)



cleanEx()
nameEx("plot.CA")
### * plot.CA

flush(stderr()); flush(stdout())

### Name: plot.CA
### Title: Draw the Correspondence Analysis (CA) graphs
### Aliases: plot.CA
### Keywords: dplot

### ** Examples

data(children)
res.ca <- CA (children, col.sup = 6:8, row.sup = 15:18)



cleanEx()
nameEx("plot.HCPC")
### * plot.HCPC

flush(stderr()); flush(stdout())

### Name: plot.HCPC
### Title: Plots for Hierarchical Classification on Principle Components
###   (HCPC) results
### Aliases: plot.HCPC
### Keywords: dplot

### ** Examples

data(iris)
# Clustering, auto nb of clusters:
res.hcpc=HCPC(iris[1:4], nb.clust=3)
# 3D graph from a different point of view:
plot.HCPC(res.hcpc, choice="3D.map", angle=60)



cleanEx()
nameEx("plot.HMFA")
### * plot.HMFA

flush(stderr()); flush(stdout())

### Name: plot.HMFA
### Title: Draw the Hierarchical Multiple Factor Analysis (HMFA) graphs
### Aliases: plot.HMFA
### Keywords: dplot

### ** Examples

data(wine)
hierar <- list(c(2,5,3,10,9,2), c(4,2))
res.hmfa <- HMFA(wine, H = hierar, type=c("n",rep("s",5)), graph = FALSE)
plot(res.hmfa, invisible="quali")
plot(res.hmfa, invisible="ind")



cleanEx()
nameEx("plot.MCA")
### * plot.MCA

flush(stderr()); flush(stdout())

### Name: plot.MCA
### Title: Draw the Multiple Correspondence Analysis (MCA) graphs
### Aliases: plot.MCA
### Keywords: dplot

### ** Examples

data (poison)
res.mca = MCA (poison, quali.sup = 3:4, quanti.sup = 1:2, graph=FALSE)
plot.MCA(res.mca,invisible=c("var","quali.sup"))
plot.MCA(res.mca,invisible="ind")
plot.MCA(res.mca,choix="var")



cleanEx()
nameEx("plot.MFA")
### * plot.MFA

flush(stderr()); flush(stdout())

### Name: plot.MFA
### Title: Draw the Multiple Factor Analysis (MFA) graphs
### Aliases: plot.MFA
### Keywords: dplot

### ** Examples

## Not run: 
##D data(wine)
##D aa = MFA(wine,group=c(2,5,3,10,9,2),type=c("n",rep("s",5)),ncp=5,
##D     name.group=c("orig","olf","vis","olfag","gust","ens"),
##D     num.group.sup=c(1,6),graph=FALSE)
##D plot(aa, choix = "ind")
##D plot(aa, choix = "ind", partial="all")
##D plot(aa, choix = "Terroir")
##D plot(aa, choix = "var", habillage="group")
##D plot(aa, choix = "axes")
## End(Not run)



cleanEx()
nameEx("plot.MFApartial")
### * plot.MFApartial

flush(stderr()); flush(stdout())

### Name: plotMFApartial
### Title: Plot an interactive Multiple Factor Analysis (MFA) graph
### Aliases: plotMFApartial
### Keywords: dplot

### ** Examples

## Not run: 
##D data(wine)
##D res.wine = MFA(wine,group=c(2,5,3,10,9,2),type=c("n",rep("s",5)),ncp=5,
##D     name.group=c("orig","olf","vis","olfag","gust","ens"),
##D     num.group.sup=c(1,6),graph=FALSE)
##D liste = plotMFApartial(res.wine)
## End(Not run)



cleanEx()
nameEx("plot.PCA")
### * plot.PCA

flush(stderr()); flush(stdout())

### Name: plot.PCA
### Title: Draw the Principal Component Analysis (PCA) graphs
### Aliases: plot.PCA
### Keywords: dplot

### ** Examples

data(decathlon)
res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup = 13)
plot(res.pca, habillage = 13, col.hab=c("green","blue"))
## To automatically draw ellipses around the barycentres of all the categorical variables
plotellipses(res.pca)
## or another graph
aa=cbind.data.frame(decathlon[,13],res.pca$ind$coord)
bb=coord.ellipse(aa,bary=TRUE)
plot.PCA(res.pca,habillage=13,ellipse=bb)



cleanEx()
nameEx("plot.catdes")
### * plot.catdes

flush(stderr()); flush(stdout())

### Name: plot.catdes
### Title: Plots for description of clusters (catdes)
### Aliases: plot.catdes
### Keywords: dplot

### ** Examples

## Not run: 
##D data(wine)
##D res.c=catdes(wine, num.var=2)
##D plot.catdes(res.c)
## End(Not run)



cleanEx()
nameEx("plot.spMCA")
### * plot.spMCA

flush(stderr()); flush(stdout())

### Name: plot.spMCA
### Title: Draw the specific Multiple Correspondence Analysis (spMCA)
###   graphs
### Aliases: plot.spMCA
### Keywords: dplot

### ** Examples

data (poison)
res <- spMCA (poison[,3:8],excl=c(1,3))
plot(res,invisible="ind")
plot(res,invisible="var")
plot(res,choix="var")



cleanEx()
nameEx("plotellipses")
### * plotellipses

flush(stderr()); flush(stdout())

### Name: plotellipses
### Title: Draw confidence ellipses around the categories
### Aliases: plotellipses
### Keywords: multivariate

### ** Examples

## Not run: 
##D data(poison)
##D res.mca = MCA(poison, quali.sup = 3:4, quanti.sup = 1:2)
##D plotellipses(res.mca)
##D plotellipses(res.mca,keepvar=1:4)
## End(Not run)

data(decathlon)
res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup=13)
plotellipses(res.pca,keepvar=13)



cleanEx()
nameEx("poison")
### * poison

flush(stderr()); flush(stdout())

### Name: poison
### Title: Poison
### Aliases: poison
### Keywords: datasets

### ** Examples

data(poison)
res.mca <- MCA(poison, quanti.sup = 1:2, quali.sup=c(3,4))



cleanEx()
nameEx("poison.text")
### * poison.text

flush(stderr()); flush(stdout())

### Name: poison.text
### Title: Poison
### Aliases: poison.text
### Keywords: datasets

### ** Examples

data(poison.text)
res.text <- textual(poison.text, num.text = 3, contingence.by = c(1,2))
## Contingence table for the sex variable, the sich variable and the couple
## of variable sick-sex
res.text2 <- textual(poison.text, num.text = 3, contingence.by = list(1,2,c(1,2)))



cleanEx()
nameEx("poulet")
### * poulet

flush(stderr()); flush(stdout())

### Name: poulet
### Title: Donnees genomiques sur les poulets
### Aliases: poulet
### Keywords: datasets

### ** Examples

## Not run: 
##D data(poulet)
##D res.pca = PCA(poulet,quali.sup=1, graph=FALSE)
##D plot(res.pca)
##D plot(res.pca,habillage=1,label="quali",palette=palette(c("black","red","blue","darkgreen","purple","orange")))
##D dimdesc(res.pca)
##D ## Dessine des ellipses autour des centres de gravite
##D aa=cbind.data.frame(poulet[,1],res.pca$ind$coord)
##D bb=coord.ellipse(aa,bary=TRUE)
##D plot.PCA(res.pca,habillage=1,ellipse=bb)
## End(Not run)



cleanEx()
nameEx("prefpls")
### * prefpls

flush(stderr()); flush(stdout())

### Name: prefpls
### Title: Scatter plot and additional variables with quality of
###   representation contour lines
### Aliases: prefpls
### Keywords: dplot

### ** Examples

data(decathlon)
prefpls(decathlon[,c(11,12,1:10)])



cleanEx()
nameEx("print.PCA")
### * print.PCA

flush(stderr()); flush(stdout())

### Name: print.PCA
### Title: Print the Principal Component Analysis (PCA) results
### Aliases: print.PCA
### Keywords: print

### ** Examples

## Not run: 
##D data(decathlon)
##D res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup = 13)
##D print(res.pca, file="c:/essai.csv", sep = ";")
## End(Not run)



cleanEx()
nameEx("reconst")
### * reconst

flush(stderr()); flush(stdout())

### Name: reconst
### Title: Reconstruction of the data from the PCA or MFA results
### Aliases: reconst
### Keywords: multivariate

### ** Examples

data(decathlon)
res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup=13, graph=FALSE)
rec <- reconst(res.pca,ncp=2)



cleanEx()
nameEx("senso")
### * senso

flush(stderr()); flush(stdout())

### Name: senso
### Title: senso
### Aliases: senso
### Keywords: datasets

### ** Examples

## Example of 2-way analysis of variance
data(senso)
res <- AovSum (Score~ Product + Day, data=senso)
res

## Example of 2-way analysis of variance with interaction
data(senso)
res2 <- AovSum (Score~ Product + Day + Product : Day, data=senso)
res2




cleanEx()
nameEx("spMCA")
### * spMCA

flush(stderr()); flush(stdout())

### Name: spMCA
### Title: Specific Multiple Correspondence Analysis (spMCA)
### Aliases: spMCA
### Keywords: multivariate

### ** Examples

## Not run: 
##D data (poison)
##D res <- spMCA (poison[,3:8],excl=c(1,3))
## End(Not run)



cleanEx()
nameEx("tea")
### * tea

flush(stderr()); flush(stdout())

### Name: tea
### Title: tea (data)
### Aliases: tea
### Keywords: datasets

### ** Examples

data(tea)
res.mca=MCA(tea,quanti.sup=19,quali.sup=20:36)
plot(res.mca,invisible=c("var","quali.sup","quanti.sup"),cex=0.7)
plot(res.mca,invisible=c("ind","quali.sup","quanti.sup"),cex=0.8)
plot(res.mca,invisible=c("quali.sup","quanti.sup"),cex=0.8)
dimdesc(res.mca)
plotellipses(res.mca,keepvar=1:4)

## make a hierarchical clustering: click on the tree to define the number of clusters
## HCPC(res.mca)



cleanEx()
nameEx("textual")
### * textual

flush(stderr()); flush(stdout())

### Name: textual
### Title: Text mining
### Aliases: textual
### Keywords: multivariate

### ** Examples

data(poison.text)
res.text <- textual(poison.text, num.text = 3, contingence.by = 1)
descfreq(res.text$cont.table)
## Contingence table for the couple of variable sick-sex
res.text2 <- textual(poison.text, num.text = 3, contingence.by = list(c(1,2)))
descfreq(res.text2$cont.table)
## Contingence table for sex, sick and the couple of variable sick-sex
res.text2 <- textual(poison.text, num.text = 3, contingence.by = list(1,2,c(1,2)))



cleanEx()
nameEx("wine")
### * wine

flush(stderr()); flush(stdout())

### Name: wine
### Title: Wine
### Aliases: wine
### Keywords: datasets

### ** Examples

data(wine)

## Example of PCA
res.pca = PCA(wine,ncp=5, quali.sup = 1:2)

## Not run: 
##D ## Example of MCA
##D res.mca = MCA(wine,ncp=5, quanti.sup = 3:ncol(wine))
##D 
##D ## Example of MFA
##D res.mfa = MFA(wine,group=c(2,5,3,10,9,2),type=c("n",rep("s",5)),ncp=5,
##D     name.group=c("orig","olf","vis","olfag","gust","ens"),
##D     num.group.sup=c(1,6),graph=FALSE)
##D plotellipses(res.mfa)
##D plotellipses(res.mfa,keepvar="Label") ## for 1 variable
## End(Not run)



cleanEx()
nameEx("write.infile")
### * write.infile

flush(stderr()); flush(stdout())

### Name: write.infile
### Title: Print in a file
### Aliases: write.infile
### Keywords: print

### ** Examples

## Not run: 
##D data(decathlon)
##D res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup = 13)
##D write.infile(res.pca, file="c:/essai.csv", sep = ";")
## End(Not run)



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
