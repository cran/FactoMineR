spMCA <- function(data,excl=NULL,ncp=5) {
    n <- nrow(as.data.frame(data))
    Q <- ncol(as.data.frame(data))
    Z <- tab.disjonctif(data)
    K <- ncol(Z)
    eI <- matrix(rep(1,length=n),ncol=1)
    eK <- matrix(rep(1,length=K),ncol=1)
    Z0 <- Z-(1/n)*crossprod(t(eI))%*%Z
    NK <- diag(colSums(Z))
    Z0t <- Z0[,-excl]
    NKp <- NK[-excl,-excl]
    H0t <- (1/sqrt(Q))*Z0t%*%diag(1/sqrt(colSums(Z)[-excl]))
    svd <- svd(H0t)
    dims <- paste('dim',1:ncp,sep='.')
    noms <- vector(length=ncol(Z))
    id=0
    for(i in 1:Q) {
      for(j in 1:length(levels(data[,i]))) {
        id=id+1
        noms[id] <- paste(colnames(data)[i],levels(data[,i])[j],sep='.')
      }}
    YIt <- sqrt(n)*svd$u%*%diag(svd$d)
    YKpt <- sqrt(n*Q)*diag(1/sqrt(colSums(Z)[-excl]))%*%svd$v%*%diag(svd$d)
    vp <- svd$d*svd$d
    eig <- as.data.frame(matrix(NA, length(vp), 3))
    rownames(eig) <- paste("comp", 1:length(vp))
    colnames(eig) <- c("eigenvalue", "percentage of variance", "cumulative percentage of variance")
    eig[, "eigenvalue"] <- svd$d*svd$d
    eig[, "percentage of variance"] <- (vp/sum(vp)) * 100
    eig[, "cumulative percentage of variance"] <- cumsum(eig[, "percentage of variance"])
    coord <- YIt[,1:ncp]
    contrib <- 100/n*coord*coord/matrix(rep(eig[[1]][1:ncp],times=n),ncol=ncp,nrow=n,byrow=T)
    dimnames(coord) <- list(1:n,dims)
    dimnames(contrib) <- list(1:n,dims)
    ind <- list(coord=coord,contrib=round(contrib,6))
    coord <- YKpt[,1:ncp]
    fK <- colSums(Z)[-excl]/n
    contrib <- 100*(fK/Q)*coord*coord/matrix(rep(eig[[1]][1:ncp],times=ncol(Z0t)),ncol=ncp,nrow=ncol(Z0t),byrow=T)
    s <- vector()
    for(i in 1:Q) s <- c(s,rep(i,times=length(levels(data[,i]))))
    s <- s[-excl]
    v.contrib <- aggregate(contrib,list(s),sum)[,-1]
    dimnames(v.contrib) <- list(colnames(data),dims)
    ctr.cloud <- data.frame(100*(1-fK)/(ncol(Z0t)-Q))
    colnames(ctr.cloud) <- 'ctr.cloud'
    vctr.cloud <- aggregate(ctr.cloud,list(s),FUN=sum)[-1]
    dimnames(vctr.cloud) <- list(colnames(data),'vctr.cloud')
    cos2 <- coord*coord/((1/fK)-1)
    dimnames(coord) <- list(noms[-excl],dims)
    dimnames(contrib) <- list(noms[-excl],dims)
    dimnames(cos2) <- list(noms[-excl],dims)
    eta2 <- matrix(nrow=Q,ncol=ncp)
    for(j in 1:Q) {
      vrc <- aggregate(ind$coord,list(data[,j]),var)[,-1]
      # for(i in 1:ncol(x)) x[is.na(x[,i]),i] <- 0
      wi <- apply(vrc,2,weighted.mean,w=as.numeric(table(data[,j])))
      be <- eig[[1]][1:ncp]-wi
      eta2[j,] <- be/eig[[1]][1:ncp]
      }
    dimnames(eta2) <- list(colnames(data),dims)
    v.test <- sqrt(cos2)*sqrt(n-1)*(((abs(coord)+coord)/coord)-1)
    var <- list(coord=coord,contrib=round(contrib,6),cos2=round(cos2,6),v.test=round(v.test,6),eta2=round(eta2,6),v.contrib=v.contrib)
    X <- data
    marge.col <- colSums(Z[,-excl])/(n*Q)
    names(marge.col) <- noms[-excl]
    marge.row <- rep(1/(n*Q),times=n)
    names(marge.row) <- 1:n
    quali <- 1:Q
    call <- list(X=X,mar.col=marge.col,marge.row=marge.row,ncp=ncp,quali=quali,excl=excl)
    RES <- list(eig=eig,call=call,ind=ind,var=var,svd=list(vs=svd$d,U=svd$u,V=svd$v))
    attr(RES,'class') <- c('spMCA','list')
    return(RES)
}

