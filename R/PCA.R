 PCA <- function (X, scale.unit = TRUE, ncp = 5, ind.sup = NULL, quanti.sup = NULL, quali.sup = NULL, row.w = NULL, col.w = NULL, graph = TRUE){
    moy.p <- function(V, poids){
      res <- sum(V*poids) / sum(poids)
    }
    ec <- function(V, poids) {
        res <- sqrt(sum(V^2 * poids)/sum(poids))
    }
    
    if (any(is.na(X))){
      if (is.null(quali.sup)) for (j in 1:ncol(X)) X[,j] <- replace(X[,j],is.na(X[,j]),mean(X[,j],na.rm=TRUE))
      else for (j in (1:ncol(X))[-quali.sup]) X[,j] <- replace(X[,j],is.na(X[,j]),mean(X[,j],na.rm=TRUE))
    }
    Xtot <- X
    if (!is.null(quali.sup)) X <- X[,-c(quanti.sup,quali.sup)]
    else  if (!is.null(quanti.sup)) X <- X[,-quanti.sup]
    if (!is.null(ind.sup)){
      X.ind.sup <- X[ind.sup,]
      X <- X[-ind.sup,]
    }
    ncp <- min(ncp, nrow(X)-1, ncol(X))
    if (is.null(row.w)) row.w <- rep(1, nrow(X))
    Pl <- diag(row.w/sum(row.w))
    if (is.null(col.w)) col.w <- rep(1, ncol(X))
    Pc <- diag(col.w)
    centre <- apply(X, 2, moy.p, row.w)
    data <- X
    X <- as.matrix(sweep(X, 2, centre, FUN = "-"))
    if (scale.unit) {
        ecart.type <- apply(X, 2, ec, row.w)
        ecart.type[ecart.type <= 1e-16] <- 1
        X <- as.matrix(sweep(X, 2, ecart.type, FUN = "/"))
    }
    else  ecart.type <- rep(1,length(centre))
    res.call <- list(row.w = (row.w/sum(row.w)), col.w = col.w,
        scale.unit = scale.unit,ncp = ncp, centre = centre, ecart.type = ecart.type, X = data)
    tmp <- svd.triplet(X, Pl = Pl, Pc = Pc)
    eig <- tmp$vs^2
    vp <- as.data.frame(matrix(NA, length(eig), 3))
    rownames(vp) <- paste("comp", 1:length(eig))
    colnames(vp) <- c("eigenvalue", "inertia", "cumulative inertia")
    vp[, "eigenvalue"] <- eig
    vp[, "inertia"] <- (eig/sum(eig)) * 100
    vp[1, "cumulative inertia"] <- vp[1, "inertia"]
    if (length(eig)>2) for (i in 2:length(eig)) vp[i, "cumulative inertia"] <- vp[i, "inertia"] + vp[i - 1, "cumulative inertia"]
    V <- tmp$V
    U <- tmp$U
    coord.ind <- sweep(U, 2, sqrt(eig), FUN = "*")
    coord.var <- sweep(V, 2, sqrt(eig), FUN = "*")
    contrib.var <- sweep(coord.var^2, 2, eig, "/")
    contrib.var <- sweep(contrib.var, 1, col.w, "*")
    dist2 <- apply(coord.var^2,1,sum)
    cor.var <- sweep(coord.var, 1, sqrt(dist2), FUN = "/")
    cos2.var <- cor.var^2
    rownames(coord.var) <- rownames(cos2.var) <- rownames(cor.var) <- rownames(contrib.var) <- colnames(X)
    colnames(coord.var) <- colnames(cos2.var) <- colnames(cor.var) <- colnames(contrib.var) <- paste("Dim", c(1:ncol(V)), sep = ".")

    res.var <- list(coord = coord.var[, 1:ncp], cor = cor.var[,1:ncp], cos2 = cos2.var[, 1:ncp], contrib = contrib.var[,1:ncp] * 100)
    dist2 <- apply(coord.ind^2,1,sum)
    cos2.ind <- sweep(coord.ind^2, 1, dist2, FUN = "/")
    contrib.ind <- sweep(coord.ind^2,1,row.w/sum(row.w), FUN = "*")
    contrib.ind <- sweep(contrib.ind,2,eig, FUN = "/")
    rownames(coord.ind) <- rownames(cos2.ind) <- rownames(contrib.ind) <- rownames(X)
    colnames(coord.ind) <- colnames(cos2.ind) <- colnames(contrib.ind) <- paste("Dim", c(1:ncol(U)), sep = ".")
    
    res.ind <- list(coord = coord.ind[, 1:ncp], cos2 = cos2.ind[,1:ncp], contrib = contrib.ind[, 1:ncp] * 100)
    res <- list(eig = vp, var = res.var,ind = res.ind, svd = tmp)
###  For illustrative individuals
    if (!is.null(ind.sup)){
      if (is.null(ecart.type))  ecart.type <- rep(1, length(centre))
      X.ind.sup <- as.matrix(sweep(X.ind.sup, 2, centre, FUN = "-"))
      X.ind.sup <- as.matrix(sweep(X.ind.sup, 2, ecart.type, FUN = "/"))
      coord.ind.sup <- X.ind.sup %*% diag(col.w) %*% tmp$V
      dist2 <- apply(coord.ind.sup^2,1,sum)
      cos2.ind.sup <- sweep(coord.ind.sup^2, 1, dist2, FUN = "/")
      coord.ind.sup <- as.data.frame(coord.ind.sup)[,1:ncp]
      cos2.ind.sup <- as.data.frame(cos2.ind.sup)[,1:ncp]
      colnames(coord.ind.sup) <- colnames(cos2.ind.sup) <- paste("Dim", c(1:ncp), sep = ".")
      rownames(coord.ind.sup) <- rownames(cos2.ind.sup) <- rownames(X.ind.sup)
      res.ind.sup <- list(coord = coord.ind.sup, cos2 = cos2.ind.sup)
      res$ind.sup = res.ind.sup
      res.call$ind.sup = ind.sup
    }    
    
### For illustrative variables
    if (!is.null(quanti.sup)){
      X.quanti.sup <- as.data.frame(Xtot [ , quanti.sup])
      if (!is.null(ind.sup)) X.quanti.sup <- as.data.frame(X.quanti.sup [-ind.sup , ])
      res.call$quanti.sup = X.quanti.sup
      centre.sup <- apply(X.quanti.sup, 2, moy.p, row.w)
      X.quanti.sup <- as.matrix(sweep(X.quanti.sup, 2, centre.sup, FUN = "-"))
      if (scale.unit) {
          ecart.type.sup <- apply(X.quanti.sup, 2, ec, row.w)
          ecart.type.sup[ecart.type.sup <= 1e-16] <- 1
          X.quanti.sup <- as.matrix(sweep(X.quanti.sup, 2, ecart.type.sup, FUN = "/"))
      }
      coord.vcs <- t(X.quanti.sup) %*% Pl %*% tmp$U
      col.w.vcs <- rep (1,ncol(coord.vcs))
      contrib.vcs <- sweep (coord.vcs^2,2,eig, FUN ="/")
      contrib.vcs <- sweep (contrib.vcs,1,col.w.vcs, FUN ="*")
      cor.vcs <- matrix(NA, ncol(X.quanti.sup), ncol(tmp$U))
      sigma <- apply(X.quanti.sup, 2, ec, row.w)
      cor.vcs <- sweep(coord.vcs,1,sigma,FUN = "/")
      cos2.vcs <- as.data.frame(cor.vcs^2)

      coord.vcs <- as.data.frame(coord.vcs)
      contrib.vcs <- as.data.frame(contrib.vcs)
      cor.vcs <- as.data.frame(cor.vcs)
      colnames(coord.vcs) <- colnames(cor.vcs) <- paste("Dim", c(1:ncol(cor.vcs)), sep = ".") -> colnames(contrib.vcs) 
      rownames(coord.vcs) <- rownames(cor.vcs) <- colnames(X.quanti.sup)  -> rownames(contrib.vcs)
      res.quanti.sup <- list(coord = coord.vcs[, 1:ncp], cor = cor.vcs[, 1:ncp], cos2 = cos2.vcs[,1:ncp], contrib = contrib.vcs[, 1:ncp] * 100)
      res$quanti.sup = res.quanti.sup
    }

### For illustrative qualitative variables
    if (!is.null(quali.sup)){
      X.quali.sup <- as.data.frame(Xtot [ , quali.sup])
      if (!is.null(ind.sup)) X.quali.sup <- as.data.frame(X.quali.sup [-ind.sup , ])
      N <- nrow(X)
      nombre <- modalite <- NULL
      for (i in 1:ncol(X.quali.sup)) {
        var <- as.factor(X.quali.sup[, i])
        n.mod <- nlevels(var)
        modalite <- c(modalite, n.mod)
        bary <- matrix(NA, n.mod, ncol(X))
        for (j in 1:n.mod) {
            ind <- levels(var)[j]
            bary[j, ] <- apply(data[which(var == ind), ], 2, moy.p, row.w[which(var == ind)])
            nombre <- c(nombre, length(var[which(var == ind)]))
        }
        colnames(bary) <- colnames(X)
        if ((levels(var)[1]%in%(1:nrow(X)))|(levels(var)[1]%in%c("y","Y","n","N"))) row.names(bary) <- paste(colnames(X.quali.sup)[i], as.character(levels(var)))
        else row.names(bary) <- as.character(levels(var))
        if (i == 1)  barycentre <- as.data.frame(bary)
        else  barycentre <- rbind(barycentre, as.data.frame(bary))
      }
      bary <- as.matrix(sweep(barycentre, 2, centre, FUN = "-"))
      if (!is.null(ecart.type)) bary <- as.matrix(sweep(bary, 2, ecart.type, FUN = "/"))
      coord.barycentre <- bary %*% Pc %*% tmp$V
      colnames(coord.barycentre) <- paste("Dim", 1:ncol(coord.barycentre), sep = ".")
      dist2 <- apply(coord.barycentre^2,1,sum)
      cos2.bary.sup <- sweep(coord.barycentre^2,1,dist2,FUN = "/")
      
      vtest <- sweep(coord.barycentre,2,sqrt(eig),FUN ="/")
      vtest <- sweep(vtest,1,sqrt(nombre/((N - nombre)/(N - 1))),FUN ="*")
      cos2.bary.sup <- cos2.bary.sup[,1:ncp]
      coord.barycentre <- coord.barycentre[,1:ncp]
      vtest <- vtest[,1:ncp]
      dimnames(cos2.bary.sup) <- dimnames(vtest) <- dimnames(coord.barycentre)
      res.quali.sup <- list(coord = coord.barycentre, cos2 = cos2.bary.sup, vtest = vtest)
      call.quali.sup <- list(quali.sup = X.quali.sup, modalite = modalite, nombre = nombre, barycentre = barycentre)
      res$quali.sup = res.quali.sup
      res.call$quali.sup = call.quali.sup
}
res$call = res.call
    
class(res) <- c("PCA", "list ")
if (graph) {
  plot(res, choix="ind")
  plot(res, choix="var")
}
    return(res)
}
