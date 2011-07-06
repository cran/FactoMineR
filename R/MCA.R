MCA <- function (X, ncp = 5, ind.sup = NULL, quanti.sup = NULL, quali.sup = NULL,
    graph = TRUE, level.ventil = 0, axes = c(1, 2), row.w = NULL, 
    na.method="NA", tab.disj=NULL){
    
############
ventil.tab <- function (tab, level.ventil=0.05,row.w=NULL,ind.sup=NULL,quali.sup=NULL,quanti.sup=NULL) {
 if (is.null(row.w)) row.w <- rep(1,nrow(tab)-length(ind.sup))
 col.var <- 1:ncol(tab)
 if (!is.null(c(quali.sup,quanti.sup))) col.var = col.var[-c(quali.sup,quanti.sup)]
 for (i in col.var) {
   if (is.factor(tab[,i])){
      tab[,i] <- ventilation(tab[,i],level.ventil=level.ventil,row.w=row.w,ind.sup=ind.sup)
   }
   if (is.ordered(tab[,i])){
      tab[,i] <- ventilation.ordonnee(tab[,i],level.ventil=level.ventil,row.w=row.w,ind.sup=ind.sup)
   }
 }
 return(tab)
}

ventilation <- function(Xqual,level.ventil=0.05,row.w=NULL,ind.sup=NULL) {
 if (!is.factor(Xqual)) stop("Xqual should be a factor \n")
 modalites <- levels(Xqual)
 if (length(modalites)<=1) stop("not enough levels \n")
 if (is.null(ind.sup)) {
  ind.act <- (1:length(Xqual))
 } else {ind.act <- (1:length(Xqual))[-ind.sup]}
 tabl <- table(Xqual[ind.act])
 if (!is.null(row.w)){
   for (j in 1:nlevels(Xqual)) tabl[j] <- sum((Xqual[ind.act]==levels(Xqual)[j])*row.w,na.rm=TRUE)
 }
 selecti <- (tabl/sum(tabl,na.rm=TRUE))< level.ventil
 if (sum(selecti)==length(modalites)) return(Xqual)

 if (!any(selecti)) return(Xqual) else {
  lesquels <- modalites[!selecti]
#  if (length(lesquels)==1) return(NULL) else {
  if (length(lesquels)==1) return(Xqual) else {
   prov <- factor(Xqual[(Xqual%in%lesquels)],levels=lesquels)
   prov <- table(prov)
   proba <- prov/sum(prov)

   for (j in modalites[selecti]) {
    Xqual[which(Xqual==j)] <- sample(lesquels,sum(Xqual==j,na.rm=TRUE), replace=TRUE,prob=proba)
   }
   Xqualvent <- factor(as.character(Xqual))
  }
 }
 return(Xqualvent)
}

ventilation.ordonnee <- function(Xqual,level.ventil=0.05,ind.sup=NULL,row.w=NULL) {
 if (!is.ordered(Xqual)) stop("Xqual must be ordered \n")
 mod <- levels(Xqual)
 if (length(mod)<=1) stop("not enough levels \n")
 if (is.null(ind.sup)) {
  ind.act <- (1:length(Xqual))
 } else {ind.act <- (1:length(Xqual))[-ind.sup]}
 tabl <- table(Xqual[ind.act])
 if (!is.null(row.w)){
   for (j in 1:nlevels(Xqual)) tabl[j] <- sum((Xqual[ind.act]==levels(Xqual)[j])*row.w,na.rm=TRUE)
 }
 selecti <- (tabl/sum(tabl))<level.ventil
 if (!any(selecti)) return(Xqual) else {
  numero <- which(selecti)
  while(any((tabl/sum(tabl))<level.ventil)) {
   j <- which(((tabl/sum(tabl))<level.ventil))[1]
   K <- length(mod)
   if (j<K) {
    if ((j>1)&(j<K-1)) levels(Xqual) <- c(mod[1:(j-1)],paste(mod[j],mod[j+1],sep="."),paste(mod[j],mod[j+1],sep="."),mod[j+2:K])
    if (j==1) levels(Xqual) <- c(paste(mod[j],mod[j+1],sep="."),paste(mod[j],mod[j+1],sep="."),mod[j+2:K])
    if (j==(K-1)) levels(Xqual) <- c(mod[1:(j-1)],paste(mod[j],mod[j+1],sep="."),paste(mod[j],mod[j+1],sep="."))
   } else {
      levels(Xqual) <- c(mod[1:(j-2)],paste(mod[j-1],mod[j],sep="."),paste(mod[j-1],mod[j],sep="."))
   }
  }
 }
# if (nlevels(Xqual)>1) return(Xqual)
# else return(NULL)
 return(Xqual)
}

tab.disj.prop<-function (tab) 
{
    tab <- as.data.frame(tab)
    modalite.disjonctif <- function(i) {
        moda <- tab[, i]
        nom <- names(tab)[i]
        n <- length(moda)
        moda <- as.factor(moda)
        x <- matrix(0, n, length(levels(moda)))
          ind<-(1:n) + n * (unclass(moda) - 1)
          indNA<-which(is.na(ind))
                
        x[(1:n) + n * (unclass(moda) - 1)] <- 1
        if (length(indNA)!=0) x[indNA,]<- matrix(rep(apply(x,2,sum)/sum(x),each=length(indNA)),nrow=length(indNA))
        if ((ncol(tab) != 1) & (levels(moda)[1] %in% c(1:nlevels(moda),"n", "N", "y", "Y"))) 
            dimnames(x) <- list(row.names(tab), paste(nom, levels(moda),sep = "."))
        else dimnames(x) <- list(row.names(tab), levels(moda))
        return(x)
    }
    if (ncol(tab) == 1) 
        res <- modalite.disjonctif(1)
    else {
        res <- lapply(1:ncol(tab), modalite.disjonctif)
        res <- as.matrix(data.frame(res, check.names = FALSE))
    }
    return(res)
}

#############
## Main program    
#############

  X <- as.data.frame(X)
  if (is.null(rownames(X))) rownames(X) = 1:nrow(X)
  if (is.null(colnames(X))) colnames(X) = paste("V", 1:ncol(X), sep = "")
  if (!is.null(ind.sup)) ind.act <- (1:nrow(X))[-ind.sup]
  else ind.act <- (1:nrow(X))

  ## avoid problem when a category has 0 individuals
    for (j in 1:ncol(X)) {
      if (!is.numeric(X[,j])) levels(X[,j])[which(table(X[ind.act,j])==0)] <- levels(X[,j])[which(table(X[ind.act,j])!=0)[1]]
    }

    if (level.ventil > 0) X <- ventil.tab(X,level.ventil=level.ventil,row.w=row.w,ind.sup=ind.sup,quali.sup=quali.sup,quanti.sup=quanti.sup)

  niveau <- NULL
  for (j in 1:ncol(X)) niveau = c(niveau, levels(X[, j]))
  for (j in 1:ncol(X)) {
      if (sum(niveau %in% levels(X[, j])) != nlevels(X[, j])) levels(X[, j]) = paste(colnames(X)[j], levels(X[, j]), sep = "_")
  }

nonact <- c(quanti.sup,quali.sup)
if (!is.null(nonact)) act <- (1:ncol(X))[-nonact]
else act <- (1:ncol(X))
Z <- tab.disjonctif(X[, act])
if (any(is.na(X[,act]))){
 if (is.null(tab.disj)){
  if (na.method=="Average") {
    tab.disj <- tab.disj.prop(X[ind.act, act])
    Z[ind.act,] <- tab.disj
  }
  if (na.method=="NA"){
    warnings('Missing values for one variable are considered as a new category; you can use method="Average" or use the imputeMCA function of the missMDA package')
    for (j in act) X[,j] <- as.factor(replace(as.character(X[,j]),is.na(X[,j]),paste(colnames(X)[j],".NA",sep="")))
    Z <- tab.disjonctif(X[, act])
  }
 } else Z[ind.act,] <- tab.disj
}
Ztot <- Z

col.sup <- NULL
if (!is.null(quali.sup)){
     if (any(is.na(X[,quali.sup,drop=FALSE]))){
       for (j in quali.sup) X[,j] <- as.factor(replace(as.character(X[,j]),is.na(X[,j]),paste(colnames(X)[j],".NA",sep="")))
     }
     X[,quali.sup] <- ventil.tab(X[,quali.sup,drop=FALSE],level.ventil=level.ventil,row.w=row.w,ind.sup=ind.sup)
     Zqs <- tab.disjonctif(X[, quali.sup])
     Ztot <- cbind.data.frame(Z, Zqs)
     col.sup <- (ncol(Z) + 1):ncol(Ztot)
}
Xact <- X[,act]

if (!is.null(quanti.sup)){
     if (any(is.na(X[,quanti.sup,drop=FALSE]))){
       for (j in quanti.sup) X[,j] <- replace(X[,j],is.na(X[,j]), mean(X[,j], na.rm=TRUE))
     }
     X.quanti.sup <- as.matrix(X[, quanti.sup])
     if (!is.null(ind.sup)) X.quanti.sup <- X.quanti.sup[ind.act, ,drop=FALSE]
     colnames(X.quanti.sup) = colnames(X)[quanti.sup]
}

    if (is.null(row.w)) row.w = rep(1, nrow(X) - length(ind.sup))
    if (length(row.w) != nrow(X) - length(ind.sup)) stop("length of vector row.w should be the number of active rows")
    res.mca <- CA(Ztot, ncp = ncp, row.sup = ind.sup, col.sup = col.sup, graph = FALSE, row.w = row.w)
    if (is.null(ncol(res.mca$row$coord))) res.mca$row$coord = matrix(res.mca$row$coord,ncol=1) 
    ncp <- ncol(res.mca$row$coord)
    res.mca$call$X <- X
    res.mca$call$ind.sup = ind.sup
    res.mca$call$quali = (1:ncol(X))
    if (!is.null(quali.sup) | !is.null(quanti.sup)) res.mca$call$quali <- res.mca$call$quali[-c(quali.sup, quanti.sup)]
    res.mca$call$quali.sup = quali.sup
    res.mca$call$quanti.sup = quanti.sup
    res.mca$call$row.w = row.w
    if (length(act)>1) res.mca$eig <- res.mca$eig[1:(sum(unlist(lapply(Xact,nlevels)))-ncol(Xact)),]
    else res.mca$eig <- res.mca$eig[1:(nlevels(Xact)-1),]
    names(res.mca)[3] <- "ind"
    res.mca$ind <- res.mca$ind[1:3]
    names(res.mca$ind) <- c("coord", "contrib", "cos2")
    names(res.mca)[4] <- "var"
    res.mca$var <- res.mca$var[1:3]
    names(res.mca$var) <- c("coord", "contrib", "cos2")
    indice <- 6
    if (!is.null(ind.sup)) {
        names(res.mca)[indice] <- "ind.sup"
        names(res.mca$ind.sup) <- c("coord", "cos2")
        indice <- indice + 1
        Xact = X[ind.act,act ,drop=FALSE]
    }
    if (!is.null(quali.sup)) {
        names(res.mca)[indice] <- "quali.sup"
        names(res.mca$quali.sup) <- c("coord", "cos2")
    }
    if (!is.null(ind.sup)) Z = Z[ind.act, ]
    Nj <- apply(Z * row.w, 2, sum)
    N <- sum(Nj)/(ncol(X) - length(quali.sup) - length(quanti.sup))
    coef <- sqrt(Nj * ((N - 1)/(N - Nj)))
    vtest <- sweep(as.data.frame(res.mca$var$coord), 1, coef, "*")
    res.mca$var$v.test <- vtest
#    eta2 = matrix(NA, ncol(Xact), ncp)
#    colnames(eta2) = paste("Dim", 1:ncp)
#    rownames(eta2) = colnames(Xact)
#    for (k in 1:ncol(Xact)) {
#        for (i in 1:ncp) {
#            auxi <- summary(aov(res.mca$ind$coord[, i] ~ Xact[, k]))[[1]]
#            eta2[k, i] <- auxi[1, 2]/sum(auxi[, 2])
#        }
#    }
    variable <- rep(colnames(Xact),unlist(lapply(Xact,nlevels)))
    if (length(act)>1){
      CTR <- aggregate(res.mca$var$contrib/100,by=list(factor(variable)),FUN=sum)
      rownames(CTR) <- CTR[,1]
      CTR <- sweep(CTR[,-1,drop=FALSE],2,res.mca$eig[1:ncp,1],FUN="*")*ncol(Xact)
      eta2 <- CTR[colnames(Xact),,drop=FALSE]
      res.mca$var$eta2 <- eta2
    }

    if (!is.null(quali.sup)) {
        if (!is.null(ind.sup)) Zqs = Zqs[ind.act, ]
        Nj <- apply(Zqs * row.w, 2, sum)
        coef <- sqrt(Nj * ((N - 1)/(N - Nj)))
        res.mca$quali.sup$v.test <- sweep(res.mca$quali.sup$coord, 1, coef, "*")

        eta2 = matrix(NA, length(quali.sup), ncp)
        colnames(eta2) = paste("Dim", 1:ncp)
        rownames(eta2) = colnames(X[, quali.sup, drop = FALSE])
        for (k in 1:length(quali.sup)) {
            for (i in 1:ncp) {
                auxi <- summary(aov(res.mca$ind$coord[, i] ~ X[rownames(Xact), quali.sup[k]]))[[1]]
                eta2[k, i] <- auxi[1, 2]/sum(auxi[, 2])
            }
        }
        res.mca$quali.sup$eta2 <- eta2
    }
    if (!is.null(quanti.sup)) {
        U <- res.mca$svd$U
        coord.quanti.sup <- matrix(NA, ncol(X.quanti.sup), ncp)
        for (i in 1:ncp) {
            for (j in 1:ncol(X.quanti.sup)) coord.quanti.sup[j, i] <- cor(U[, i], X.quanti.sup[, j], method = "pearson")
        }
        dimnames(coord.quanti.sup) <- list(colnames(X.quanti.sup), paste("Dim", 1:ncp, sep = "."))
        res.mca$quanti.sup$coord <- coord.quanti.sup
    }
    class(res.mca) <- c("MCA", "list")
    if (graph) {
        plot.MCA(res.mca, choix = "ind", axes = axes)
        plot.MCA(res.mca, choix = "var", axes = axes)
        if (!is.null(quanti.sup)) plot.MCA(res.mca, choix = "quanti.sup", axes = axes)
    }
    return(res.mca)
}
