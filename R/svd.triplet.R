svd.triplet<-function(X, Pl=NULL, Pc=NULL) {
    if( is.null(Pl)) Pl<-diag(rep(1, nrow(X)))
    if( is.null(Pc)) Pl<-diag(rep(1, ncol(X)))
    Z<-Pl^0.5%*%X%*%Pc^0.5
    svd.usuelle<-svd(Z)
    for (i in 1:ncol(svd.usuelle$v)){
      if (sum(svd.usuelle$v[,i]) <0){
         svd.usuelle$v[,i] <- -svd.usuelle$v[,i]
         svd.usuelle$u[,i] <- -svd.usuelle$u[,i]
      }
    }
    U<-diag(diag(Pl^-0.5))%*%svd.usuelle$u[,1:min(ncol(X),nrow(X)-1)]
    V<-diag(diag(Pc^-0.5))%*%svd.usuelle$v[,1:min(ncol(X),nrow(X)-1)]
    vs<-svd.usuelle$d[1:min(ncol(X),nrow(X)-1)]
    res<-list(vs=vs, U=U, V=V)
    return(res)
}
