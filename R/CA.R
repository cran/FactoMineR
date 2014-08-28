CA <- function (X, ncp = 5, row.sup = NULL, col.sup = NULL, quanti.sup=NULL, quali.sup=NULL, graph = TRUE, axes=c(1,2), row.w=NULL){

fct.eta2 <- function(vec,x,weights) {
  res <- summary(lm(x~vec,weights=weights))$r.squared
}

    if (is.table(X)) X <- X[,]
	X <- as.data.frame(X)
    if (is.null(rownames(X))) rownames(X) = 1:nrow(X)
    if (is.null(colnames(X))) colnames(X) = paste("V",1:ncol(X),sep="")
    colnames(X)[colnames(X)==""] <- paste("V",1:sum(colnames(X)==""),sep="")
    rownames(X)[is.null(rownames(X))] <- paste("row",1:sum(rownames(X)==""),sep="")
#    for (j in 1:ncol(X)) if (colnames(X)[j]=="") colnames(X)[j] = paste("V",j,sep="")
#    for (j in 1:nrow(X)) if (is.null(rownames(X)[j])) rownames(X)[j] = paste("row",j,sep="")
    Xtot <- X
    if (any(!sapply(X, is.numeric))) {
#    if (!any(apply(X,2,is.numeric))){
      auxi = NULL
      for (j in (1:ncol(X))[!((1:ncol(X))%in%quali.sup)]) if (!is.numeric(X[,j])) auxi = c(auxi,colnames(X)[j])
      if (!is.null(auxi)) stop(paste("\nThe following variables are not quantitative: ", auxi))
    }
    if (!inherits(X, "data.frame")) stop("X is not a data.frame")
    if (!is.null(row.sup)) X <- as.data.frame(X[-row.sup,])
    if ((!is.null(col.sup))||(!is.null(quanti.sup))||(!is.null(quali.sup))) X <- as.data.frame(X[,-c(col.sup,quanti.sup,quali.sup)])
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
    vp[, "cumulative percentage of variance"] <- cumsum(vp[, "percentage of variance"])
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
    
    res.call <- list(X = X, marge.col = marge.col, marge.row = marge.row, ncp = ncp, row.w=row.w,call=sys.calls()[[1]],Xtot=Xtot,N=sum(row.w*apply(X,1,sum)))
    res.col <- list(coord = as.matrix(coord.col[, 1:ncp]), contrib = as.matrix(contrib.col[, 1:ncp] * 100), cos2 = as.matrix(cos2.col[, 1:ncp]), inertia=inertia.col)
    res.row <- list(coord = coord.row[, 1:ncp], contrib = contrib.row[, 1:ncp] * 100, cos2 = cos2.row[, 1:ncp], inertia=inertia.row)
    res <- list(eig = vp, call = res.call, row = res.row, col = res.col, svd = tmp)
  if (!is.null(row.sup)){
    X.row.sup <- as.data.frame(Xtot[row.sup,])
    if ((!is.null(col.sup))||(!is.null(quanti.sup))||(!is.null(quali.sup))) X.row.sup <- as.data.frame(X.row.sup[,-c(col.sup,quanti.sup,quali.sup)])
    somme.row <- apply(X.row.sup, 1, sum)
    X.row.sup <- sweep(X.row.sup, 1, somme.row, "/")
    coord.row.sup <- as.matrix(X.row.sup) %*% V
dist2.row <- apply(sweep(sweep(X.row.sup,2,marge.col,FUN="-")^2,2,1/marge.col,FUN="*"),1,sum)
##    dist2.row <- apply(coord.row.sup^2,1,sum)
    cos2.row.sup <- sweep(coord.row.sup^2,1,dist2.row,FUN="/")
    coord.row.sup <- coord.row.sup[, 1:ncp,drop=FALSE]
    cos2.row.sup <- cos2.row.sup[, 1:ncp,drop=FALSE]
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
    coord.col.sup <- as.matrix(coord.col.sup[,1:ncp,drop=FALSE])
    cos2.col.sup <- cos2.col.sup[,1:ncp,drop=FALSE]
    colnames(coord.col.sup) <- colnames(cos2.col.sup) <- paste("Dim", 1:ncp)
    rownames(coord.col.sup) <- rownames(cos2.col.sup) <- colnames(X.col.sup)
    res.col.sup <- list(coord = coord.col.sup, cos2 = cos2.col.sup)
    res$col.sup <- res.col.sup
    res$call$col.sup <- col.sup
}
## Ajout variable quanti supp.
    if (!is.null(quanti.sup)) {
        coord.quanti.sup <- matrix(NA, length(quanti.sup), ncp)
        if (is.null(row.sup)) coord.quanti.sup <- cov.wt(cbind.data.frame(res$row$coord,Xtot[,quanti.sup,drop=FALSE]),cor=TRUE,wt=marge.row,method="ML")$cor[-(1:ncp),1:ncp,drop=FALSE]
        else coord.quanti.sup <- cov.wt(cbind.data.frame(res$row$coord,Xtot[-row.sup,quanti.sup,drop=FALSE]),wt=marge.row,cor=TRUE,method="ML")$cor[-(1:ncp),1:ncp,drop=FALSE]
        dimnames(coord.quanti.sup) <- list(colnames(Xtot)[quanti.sup], paste("Dim", 1:ncp, sep = "."))
        res$quanti.sup$coord <- coord.quanti.sup
        res$quanti.sup$cos2 <- coord.quanti.sup^2
        res$call$quanti.sup <- quanti.sup
    }
## Ajout variable quali supp.
	if (!is.null(quali.sup)) {
	    res$quali.sup$coord <- NULL
		if (!is.null(row.sup)) {
		  for (j in 1:length(quali.sup)) res$quali.sup$coord <- rbind.data.frame(res$quali.sup$coord,sweep(sapply(as.data.frame(sweep(res$row$coord,1,marge.row,FUN="*")),tapply,Xtot[-row.sup,quali.sup[j]],sum), 1, tapply(marge.row,Xtot[-row.sup,quali.sup[j]],sum),FUN="/"))
		} else {
		  for (j in 1:length(quali.sup)) res$quali.sup$coord <- rbind.data.frame(res$quali.sup$coord,sweep(sapply(as.data.frame(sweep(res$row$coord,1,marge.row,FUN="*")),tapply,Xtot[,quali.sup[j]],sum), 1, tapply(marge.row,Xtot[,quali.sup[j]],sum),FUN="/"))
		}
        rownames(res$quali.sup$coord) <- paste(rep(colnames(Xtot)[quali.sup],lapply(Xtot[,quali.sup,drop=FALSE],nlevels)) , unlist(lapply(Xtot[,quali.sup,drop=FALSE],levels)),sep=".")

        if (!is.null(row.sup)) Zqs <- tab.disjonctif(Xtot[-row.sup,quali.sup])
		else Zqs <- tab.disjonctif(Xtot[,quali.sup])
        Nj <- apply(Zqs * row.w, 2, sum)
        Nj <- apply(Zqs * marge.row, 2, sum)*total
        if (total>1) coef <- sqrt(Nj * ((total - 1)/(total - Nj)))
		else coef <- sqrt(Nj)
        res$quali.sup$v.test <- sweep(res$quali.sup$coord, 1, coef, "*")

        eta2 = matrix(NA, length(quali.sup), ncp)
        colnames(eta2) = paste("Dim", 1:ncp)
        rownames(eta2) = colnames(Xtot)[quali.sup]
        if (is.null(row.sup)) {
		  for (i in 1:ncp)  eta2[, i] <- unlist(lapply(as.data.frame(Xtot[, quali.sup]),fct.eta2,res$row$coord[,i],weights=marge.row))
		} else {
		  for (i in 1:ncp)  eta2[, i] <- unlist(lapply(as.data.frame(Xtot[-row.sup, quali.sup]),fct.eta2,res$row$coord[,i],weights=marge.row))
		}	  
        res$quali.sup$eta2 <- eta2
        res$call$quali.sup <- quali.sup
    }

    class(res) <- c("CA", "list")
    if (graph) {
	  plot(res,axes=axes)
	  if (!is.null(quanti.sup)) plot(res, choix="quanti.sup",axes=axes,new.plot=TRUE)
	}
    return(res)
}
