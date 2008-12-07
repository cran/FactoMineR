MCA <- function (X, ncp = 5, ind.sup = NULL, quanti.sup = NULL, quali.sup = NULL, graph = TRUE, axes=c(1,2),row.w=NULL) {

    X <- as.data.frame(X)
    if (is.null(rownames(X))) rownames(X) = 1:nrow(X)
    if (is.null(colnames(X))) colnames(X) = paste("V",1:ncol(X),sep="")
    for (j in 1:ncol(X)) if (colnames(X)[j]=="") colnames(X)[j] = paste("V",j,sep="")
    for (j in 1:nrow(X)) if (is.null(rownames(X)[j])) rownames(X)[j] = paste("row",j,sep="")
    Xtot <- X
    niveau = NULL
    for (j in 1:ncol(X)) niveau = c(niveau,levels(X[,j]))
    for (j in 1:ncol(X)) {
      if (sum(niveau%in%levels(X[,j]))!=nlevels(X[,j])) levels(X[,j]) = paste(colnames(X)[j],levels(X[,j]),sep="_")
    }
    col.sup <- NULL
    if (!is.null(quali.sup)) {
      Zqs <- tab.disjonctif(X[,quali.sup])
      Z <- tab.disjonctif(X[,-c(quanti.sup,quali.sup)])
      Ztot <- cbind.data.frame(Z,Zqs)
      col.sup <- (ncol(Z)+1):(ncol(Z)+ncol(Zqs))
    }
    else {
     if (!is.null(quanti.sup)) Z <- Ztot <- tab.disjonctif(X[,-quanti.sup])
     else Z <- Ztot <- tab.disjonctif(X)
    }
## 2 lignes ajoutées
    if (is.null(row.w)) row.w = rep(1,nrow(X)-length(ind.sup))
    if (length(row.w)!=nrow(X)-length(ind.sup)) stop("length of vector row.w should be the number of active rows")

## ligne modifiée
##    res.mca <- CA(Ztot, ncp = ncp, row.sup = ind.sup, col.sup =col.sup, graph = FALSE)
    res.mca <- CA(Ztot, ncp = ncp, row.sup = ind.sup, col.sup =col.sup, graph = FALSE, row.w = row.w)
    res.mca$call$X <- X
    res.mca$call$ind.sup = ind.sup
    res.mca$call$quali = (1:ncol(X))
    if (!is.null(quali.sup)|!is.null(quanti.sup)) res.mca$call$quali <- res.mca$call$quali[-c(quali.sup,quanti.sup)]
    res.mca$call$quali.sup = quali.sup
    res.mca$call$quanti.sup = quanti.sup
    names(res.mca)[3] <- "ind"
    names(res.mca$ind) <- c("coord", "contrib", "cos2")
    names(res.mca)[4] <- "var"
    names(res.mca$var) <- c("coord", "contrib", "cos2")
    indice <- 6
    if (!is.null(ind.sup)){
      names(res.mca)[indice]  <- "ind.sup"
      names(res.mca$ind.sup) <- c("coord", "cos2")
      indice <- indice +1
    }
    if (!is.null(quali.sup)){
      names(res.mca)[indice]  <- "quali.sup"
      names(res.mca$quali.sup) <- c("coord", "cos2")
    }
## 2 lignes modifiées
##    N <- nrow(Z)
##    Nj <- apply(Z, 2, sum)
    if (!is.null(ind.sup)) Z = Z[-ind.sup,]
    Nj <- apply(Z*row.w, 2, sum)
    N <- sum(Nj)/(ncol(X)-length(quali.sup)-length(quanti.sup))
    coef <- sqrt(Nj * ((N - 1)/(N - Nj)))
    vtest <- sweep(as.data.frame(res.mca$var$coord), 1, coef, "*")
    res.mca$var$vtest <- vtest

    if (!is.null(quali.sup)) {
## 1 ligne supprimée, 1 lignes modifiée
##      N <- nrow(Zqs)
##      Nj <- apply(Zqs, 2, sum)
    if (!is.null(ind.sup)) Zqs = Zqs[-ind.sup,]
    Nj <- apply(Zqs*row.w, 2, sum)
      coef <- sqrt(Nj * ((N - 1)/(N - Nj)))
      res.mca$quali.sup$vtest <- sweep(res.mca$quali.sup$coord, 1, coef, "*")
    }
    
    if (!is.null(quanti.sup)){
      X.quanti.sup <- as.matrix(Xtot[,quanti.sup])
      if (!is.null(ind.sup)) X.quanti.sup <- X.quanti.sup [-ind.sup,]
      colnames(X.quanti.sup) = colnames(Xtot)[quanti.sup]
      U <- res.mca$svd$U
      coord.quanti.sup <- matrix(NA, ncol(X.quanti.sup), ncp)
      for (i in 1:ncp) {
        for (j in 1:ncol(X.quanti.sup)) coord.quanti.sup[j, i] <- cor(U[, i], X.quanti.sup[, j], method = "pearson")
      }
      dimnames(coord.quanti.sup) <- list(colnames(X.quanti.sup),paste("Dim",1:ncp,sep="."))
      res.mca$quanti.sup$coord <- coord.quanti.sup
    }
    class(res.mca) <- c("MCA", "list")
    if (graph) {
      plot.MCA(res.mca,axes=axes)
    }
    return(res.mca)
}
