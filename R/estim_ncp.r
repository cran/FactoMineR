estim_ncp <- function(X, ncp.min=0,ncp.max=NULL, scale=TRUE,method="Smooth"){

## Pas de NA dans X
method <- tolower(method)
p=ncol(X)
n=nrow(X)
if (is.null(ncp.max)) ncp.max <- ncol(X)-1
ncp.max <- min(nrow(X)-2,ncol(X)-1,ncp.max)
crit <- NULL
if (ncp.min==0) crit = mean((X-rep(colMeans(X,na.rm=TRUE),each=nrow(X)))^2,na.rm=TRUE)
for (q in max(ncp.min,1):ncp.max){
    res.pca = PCA(X,scale=scale,graph=FALSE,ncp=max(q,2))
    rec = reconst(res.pca,ncp=q)
    if (method=="smooth"){
      f=res.pca$ind$coord[,1:q,drop=F]
      u=sweep(res.pca$var$coord[,1:q,drop=F],2,sqrt(res.pca$eig[1:q,1]),FUN="/")
      a=f%*%solve(t(f)%*%f)%*%t(f)
      b=u%*%solve(t(u)%*%u)%*%t(u)
      zz=sweep(rec-X,1,1-diag(a),FUN="/")
      sol2 = sweep(zz,2,1-diag(b),FUN="/")
      crit=c(crit,mean(sol2^2))
    }    
    if (method=="gcv") crit=c(crit,mean(( (n*p-sum(is.na(X)))*(X-rec)/ (n*p-sum(is.na(X))- q*(n+p-q)))^2,na.rm=T))
  }
  return(list(ncp = which.min(crit)+ncp.min-1,criterion=crit))
}
