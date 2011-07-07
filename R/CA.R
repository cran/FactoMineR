CA <- function (X, ncp = 5, row.sup = NULL, col.sup = NULL, graph = TRUE, axes=c(1,2), row.w=NULL){
    X <- as.data.frame(X)
    if (is.null(rownames(X))) rownames(X) = 1:nrow(X)
    if (is.null(colnames(X))) colnames(X) = paste("V",1:ncol(X),sep="")
    colnames(X)[colnames(X)==""] <- paste("V",1:sum(colnames(X)==""),sep="")
    rownames(X)[is.null(rownames(X))] <- paste("row",1:sum(rownames(X)==""),sep="")
#    for (j in 1:ncol(X)) if (colnames(X)[j]=="") colnames(X)[j] = paste("V",j,sep="")
#    for (j in 1:nrow(X)) if (is.null(rownames(X)[j])) rownames(X)[j] = paste("row",j,sep="")
    Xtot <- X
    if (!any(apply(X,2,is.numeric))){
      auxi = NULL
      for (j in 1:ncol(X)) if (!is.numeric(X[,j])) auxi = c(auxi,colnames(X)[j])
      stop(paste("\nThe following variables are not quantitative: ", auxi))
    }
    if (!inherits(X, "data.frame")) stop("X is not a data.frame")
    if (!is.null(row.sup)) X <- as.data.frame(X[-row.sup,])
    if (!is.null(col.sup)) X <- as.data.frame(X[,-col.sup])
### 3 lignes rajoutées
    if (is.null(row.w)) row.w = rep(1,nrow(X))
	row.w.init <- row.w
    if (length(row.w)!=nrow(X)) stop("length of vector row.w should be the number of active rows")
    total <- sum(sweep(X,1,row.w,FUN="*"))
#    row.w = row.w/sum(row.w)*nrow(X)
    F <- sweep(as.matrix(X/total),1,row.w,FUN="*")
    marge.col <- apply(F, 2, sum)
    marge.row <- apply(F, 1, sum)
    ncp <- min(ncp, (nrow(X) - 1), (ncol(X) - 1))
    T <- sweep(F,1,marge.row,FUN="/")
    T <- sweep(T,2,marge.col,FUN="/")
    Tc <- T - 1
    tmp <- svd.triplet(Tc, row.w = marge.row, col.w = marge.col,ncp=ncp)
    eig <- tmp$vs^2
    vp <- as.data.frame(matrix(NA, length(eig), 3))
    rownames(vp) <- paste("dim", 1:length(eig))
    colnames(vp) <- c("eigenvalue", "percentage of variance", "cumulative percentage of variance")
    vp[, "eigenvalue"] <- eig
    vp[, "percentage of variance"] <- (eig/sum(eig))*100
    vp[1, "cumulative percentage of variance"] <- vp[1, "percentage of variance"]
    if (length(eig)>1) for (i in 2:length(eig))  vp[i, "cumulative percentage of variance"] <- vp[i, "percentage of variance"] + vp[i - 1, "cumulative percentage of variance"]
    V <- tmp$V
    U <- tmp$U
	eig <- eig[1:ncol(U)]
    coord.col <- sweep(V,2,sqrt(eig),FUN="*")
    coord.row <- sweep(U,2,sqrt(eig),FUN="*")
dist2.col <- apply(sweep(Tc^2,1,marge.row,FUN="*"),2,sum)
##    dist2.col <- apply(coord.col^2,1,sum)
    contrib.col <- sweep(coord.col^2,1,marge.col,FUN="*")
    contrib.col <- sweep(contrib.col,2,eig,FUN="/")
    cos2.col <- sweep(coord.col^2,1,dist2.col,FUN="/")
    colnames(coord.col) <- colnames(contrib.col) <- colnames(cos2.col) <- paste("Dim", 1:length(eig))
    rownames(coord.col) <- rownames(contrib.col) <- rownames(cos2.col) <- colnames(X)
dist2.row <- apply(sweep(Tc^2,2,marge.col,FUN="*"),1,sum)
##    dist2.row <- apply(coord.row^2,1,sum)
    contrib.row <- sweep(coord.row^2,1,marge.row,FUN="*")
    contrib.row <- sweep(contrib.row,2,eig,FUN="/")
    cos2.row <- sweep(coord.row^2,1,dist2.row,FUN="/")
    colnames(coord.row) <- colnames(contrib.row) <- colnames(cos2.row) <- paste("Dim", 1:length(eig))
    rownames(coord.row) <- rownames(contrib.row) <- rownames(cos2.row) <- rownames(X)
    inertia.row = marge.row*dist2.row
    inertia.col = marge.col*dist2.col
    names(inertia.col) <- rownames(coord.col)
    names(inertia.row) <- rownames(coord.row)
    
    res.call <- list(X = X, marge.col = marge.col, marge.row = marge.row, ncp = ncp, row.w=row.w)
    res.col <- list(coord = as.matrix(coord.col[, 1:ncp]), contrib = as.matrix(contrib.col[, 1:ncp] * 100), cos2 = as.matrix(cos2.col[, 1:ncp]), inertia=inertia.col)
    res.row <- list(coord = coord.row[, 1:ncp], contrib = contrib.row[, 1:ncp] * 100, cos2 = cos2.row[, 1:ncp], inertia=inertia.row)
    res <- list(eig = vp, call = res.call, row = res.row, col = res.col, svd = tmp)
  if (!is.null(row.sup)){
    X.row.sup <- as.data.frame(Xtot[row.sup,])
    if (!is.null(col.sup)) X.row.sup <- as.data.frame(X.row.sup[,-col.sup])
    somme.row <- apply(X.row.sup, 1, sum)
    X.row.sup <- sweep(X.row.sup, 1, somme.row, "/")
    coord.row.sup <- as.matrix(X.row.sup) %*% V
dist2.row <- apply(sweep(sweep(X.row.sup,2,marge.col,FUN="-")^2,2,1/marge.col,FUN="*"),1,sum)
##    dist2.row <- apply(coord.row.sup^2,1,sum)
    cos2.row.sup <- sweep(coord.row.sup^2,1,dist2.row,FUN="/")
    coord.row.sup <- as.data.frame(coord.row.sup)[, 1:ncp,drop=FALSE]
    cos2.row.sup <- as.data.frame(cos2.row.sup)[, 1:ncp,drop=FALSE]
    colnames(coord.row.sup) <- colnames(cos2.row.sup) <- paste("Dim", 1:ncp)
    rownames(coord.row.sup) <- rownames(cos2.row.sup) <- rownames(X.row.sup)
    res.row.sup <- list(coord = coord.row.sup, cos2 = cos2.row.sup)
    res$row.sup <- res.row.sup
    res$call$row.sup <- row.sup
}

 if (!is.null(col.sup)){
    X.col.sup <- as.data.frame(Xtot[,col.sup])
    if (!is.null(row.sup)) X.col.sup <- as.data.frame(X.col.sup[-row.sup,])
## 1 ligne rajoutée
    X.col.sup <- sweep(X.col.sup,1,row.w,FUN="*")
    colnames(X.col.sup) <- colnames(Xtot)[col.sup]
    somme.col <- apply(X.col.sup, 2, sum)
    X.col.sup <- sweep(X.col.sup, 2, somme.col, "/")
    coord.col.sup <- as.data.frame(t(as.matrix(X.col.sup)) %*% U)
	
dist2.col <- apply(sweep(sweep(X.col.sup,1,marge.row,FUN="-")^2,1,1/marge.row,FUN="*"),2,sum)
##    dist2.col <- apply(coord.col.sup^2,1,sum)
    cos2.col.sup <- sweep(coord.col.sup^2,1,dist2.col,FUN="/")
    coord.col.sup <- as.data.frame(coord.col.sup)[,1:ncp]
    cos2.col.sup <- as.data.frame(cos2.col.sup)[,1:ncp]
    colnames(coord.col.sup) <- colnames(cos2.col.sup) <- paste("Dim", 1:ncp)
    rownames(coord.col.sup) <- rownames(cos2.col.sup) <- colnames(X.col.sup)
    res.col.sup <- list(coord = coord.col.sup, cos2 = cos2.col.sup)
    res$col.sup <- res.col.sup
    res$call$col.sup <- col.sup
}
    class(res) <- c("CA", "list")
    if (graph) plot.CA(res,axes=axes)
    return(res)
}
