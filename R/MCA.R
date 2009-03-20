MCA <- function (X, ncp = 5, ind.sup = NULL, quanti.sup = NULL, quali.sup = NULL,
    graph = TRUE, level.ventil = 0, axes = c(1, 2), row.w = NULL){
    
############
ventil.tab <- function (tab, level.ventil=0.05,row.w=NULL,ind.sup=NULL,quali.sup=NULL,quanti.sup=NULL) {
 col.to.delete <- NULL
 if (is.null(row.w)) row.w <- rep(1,nrow(tab))
 col.var = 1:ncol(tab)
 if (!is.null(c(quali.sup,quanti.sup))) {
   tab <- cbind.data.frame(tab[,-c(quali.sup,quanti.sup)],tab[,c(quali.sup,quanti.sup)])
   col.var <- 1:(ncol(tab)-length(c(quali.sup,quanti.sup)))
 }
 for (i in col.var) {
   if (is.factor(tab[,i])){
      aux <- ventilation(tab[,i],level.ventil=level.ventil,row.w=row.w,ind.sup=ind.sup)
      if (!is.null(aux)) tab[,i]<- aux
      else col.to.delete <- c(col.to.delete,i)
   }
   if (is.ordered(tab[,i])){
      aux <- ventilation.ordonnee(tab[,i],level.ventil=level.ventil,row.w=row.w,ind.sup=ind.sup)
      if (!is.null(aux)) tab[,i]<- aux
      else col.to.delete <- c(col.to.delete,i)
   }
 }
 if (!is.null(col.to.delete)){
   tab <- tab[,-col.to.delete]
   nb.act <- ncol(tab)-length(c(quali.sup,quanti.sup))
   if (!is.null(quali.sup)) quali.sup <- (nb.act+1):(nb.act+length(quali.sup))
   if (!is.null(quanti.sup)) quanti.sup <- (nb.act+length(quali.sup)+1):(nb.act+length(c(quali.sup,quanti.sup)))
 }
 res = list()
 res$ventil=tab
 res$quali.sup=quali.sup
 res$quanti.sup=quanti.sup
 return(res)
}

ventilation <- function(Xqual,level.ventil=0.05,row.w=NULL,ind.sup=NULL) {
 if (!is.factor(Xqual)) stop("Xqual should be a factor \n")
 modalites <- levels(Xqual)
 if (length(modalites)<=1) stop("not enough levels \n")
 if (is.null(ind.sup)) tabl <- table(Xqual)
 else tabl <- table(Xqual[-ind.sup])
 if (!is.null(row.w)){
   for (j in 1:nlevels(Xqual)) tabl[j] <- sum((Xqual==levels(Xqual)[j])*row.w)
 }
 selecti <- (tabl/sum(tabl))< level.ventil
 if (!any(selecti)) return(Xqual) else {
  lesquels <- modalites[!selecti]
  if (length(lesquels)==1) return(NULL) else {
   prov <- factor(Xqual[(Xqual%in%lesquels)],levels=lesquels)
   prov <- table(prov)
   proba <- prov/sum(prov)
   for (j in modalites[selecti]) {
    Xqual[Xqual==j]<-sample(lesquels,sum(Xqual==j), replace=T,prob=proba)
   }
   Xqualvent <- factor(as.character(Xqual))
  }
 }
 return(Xqualvent)
}

ventilation.ordonnee <- function(Xqual,level.ventil=0.05,ind.sup=NULL,row.w=NULL) {
 if (!is.ordered(Xqual)) stop("Xqual must be ordered \n")
 modalites <- levels(Xqual)
 if (length(modalites)<=1) stop("not enough levels \n")
 if (is.null(ind.sup)) tabl <- table(Xqual)
 else tabl <- table(Xqual[-ind.sup])
 if (!is.null(row.w)){
   for (j in 1:nlevels(Xqual)) tabl[j] <- sum((Xqual==levels(Xqual)[j])*row.w)
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
##   tabl <- table(Xqual)
##   mod <- levels(Xqual)
  }
 }
 if (nlevels(Xqual)>1) return(Xqual)
 else return(NULL)
}
#############
## Main program    
#############
    X <- as.data.frame(X)
    if (is.null(rownames(X))) rownames(X) = 1:nrow(X)
    if (is.null(colnames(X))) colnames(X) = paste("V", 1:ncol(X), sep = "")
    for (j in 1:ncol(X)) if (colnames(X)[j] == "") colnames(X)[j] = paste("V", j, sep = "")
    for (j in 1:nrow(X)) if (is.null(rownames(X)[j])) rownames(X)[j] = paste("row", j, sep = "")

    if (any(is.na(X))){
      for (j in 1:ncol(X)){
        if (is.numeric(X[,j])) X[,j] <- replace(X[,j],is.na(X[,j]), mean(X[,j], na.rm=TRUE))
        else X[,j] <- as.factor(replace(as.character(X[,j]),is.na(X[,j]),"NA"))
      }
    }

    if (level.ventil > 0) {
      aux <- ventil.tab(X,level.ventil=level.ventil,row.w=row.w,ind.sup=ind.sup,quali.sup=quali.sup,quanti.sup=quanti.sup)
      X <- aux$ventil
      quali.sup <- aux$quali.sup
      quanti.sup <- aux$quanti.sup
    }  

    Xtot <- Xact <- X
    niveau = NULL
    for (j in 1:ncol(X)) niveau = c(niveau, levels(X[, j]))
    for (j in 1:ncol(X)) {
        if (sum(niveau %in% levels(X[, j])) != nlevels(X[, j])) levels(X[, j]) = paste(colnames(X)[j], levels(X[, j]), sep = "_")
    }
    col.sup <- NULL
        
    if (!is.null(quali.sup)) {
        Zqs <- tab.disjonctif(X[, quali.sup])
        Z <- tab.disjonctif(X[, -c(quanti.sup, quali.sup)])
        Ztot <- cbind.data.frame(Z, Zqs)
        col.sup <- (ncol(Z) + 1):(ncol(Z) + ncol(Zqs))
        Xact = X[, -c(quanti.sup, quali.sup), drop = FALSE]
    }
    else {
        if (!is.null(quanti.sup)) {
            Z <- Ztot <- tab.disjonctif(X[, -quanti.sup])
            Xact = X[, -c(quanti.sup), drop = FALSE]
        }
        else Z <- Ztot <- tab.disjonctif(X)
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
    res.mca$eig <- res.mca$eig[1:(sum(unlist(lapply(Xact,nlevels)))-ncol(Xact)),]
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
        Xact = Xact[-ind.sup, ]
    }
    if (!is.null(quali.sup)) {
        names(res.mca)[indice] <- "quali.sup"
        names(res.mca$quali.sup) <- c("coord", "cos2")
    }
    if (!is.null(ind.sup)) Z = Z[-ind.sup, ]
    Nj <- apply(Z * row.w, 2, sum)
    N <- sum(Nj)/(ncol(X) - length(quali.sup) - length(quanti.sup))
    coef <- sqrt(Nj * ((N - 1)/(N - Nj)))
    vtest <- sweep(as.data.frame(res.mca$var$coord), 1, coef, "*")
    res.mca$var$v.test <- vtest
    eta2 = matrix(NA, ncol(Xact), ncp)
    colnames(eta2) = paste("Dim", 1:ncp)
    rownames(eta2) = colnames(Xact)
    for (k in 1:ncol(Xact)) {
        for (i in 1:ncp) {
            auxi <- summary(aov(res.mca$ind$coord[, i] ~ Xact[, k]))[[1]]
            eta2[k, i] <- auxi[1, 2]/sum(auxi[, 2])
        }
    }
    res.mca$var$eta2 <- eta2
    if (!is.null(quali.sup)) {
        if (!is.null(ind.sup)) Zqs = Zqs[-ind.sup, ]
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
        X.quanti.sup <- as.matrix(Xtot[, quanti.sup])
        if (!is.null(ind.sup)) X.quanti.sup <- X.quanti.sup[-ind.sup, ,drop=FALSE]
        colnames(X.quanti.sup) = colnames(Xtot)[quanti.sup]
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
