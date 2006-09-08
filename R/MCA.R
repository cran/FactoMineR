MCA <- function (X, ncp = 5, ind.sup = NULL, quanti.sup = NULL, quali.sup = NULL, graph = TRUE) {

    Xtot <- X
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
    res.mca <- CA(Ztot, ncp = ncp, row.sup = ind.sup, col.sup =col.sup, graph = FALSE)
    res.mca$call$X <- X
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
    N <- nrow(Z)
    Nj <- apply(Z, 2, sum)
    coef <- sqrt(Nj * ((N - 1)/(N - Nj)))
    vtest <- sweep(as.data.frame(res.mca$var$coord), 1, coef, "*")
    res.mca$var$vtest <- vtest

    if (!is.null(quali.sup)) {
      N <- nrow(Zqs)
      Nj <- apply(Zqs, 2, sum)
      coef <- sqrt(Nj * ((N - 1)/(N - Nj)))
      res.mca$quali.sup$vtest <- sweep(res.mca$quali.sup$coord, 1, coef, "*")
    }
    
    if (!is.null(quanti.sup)){
      X.quanti.sup <- Xtot[,quanti.sup]
      if (!is.null(ind.sup)) X.quanti.sup <- X.quanti.sup [-ind.sup,]
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
      plot.MCA(res.mca)
      plot.MCA(res.mca, invisible = c("ind","ind.sup"))
      plot.MCA(res.mca, invisible = "quali")
    }
    return(res.mca)
}
