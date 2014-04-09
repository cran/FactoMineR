svd.triplet = function (X, row.w = NULL, col.w = NULL,ncp=Inf) {


tryCatch.W.E <- function(expr){  ## function proposed by Maechlmr
    W <- NULL
    w.handler <- function(w){ # warning handler
        W <<- w
        invokeRestart("muffleWarning")
    }
    list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
                                     warning = w.handler),
         warning = W)
}


   if (is.null(row.w)) row.w <- rep(1/nrow(X), nrow(X))
   if (is.null(col.w)) col.w <- rep(1, ncol(X))
   ncp <- min(ncp,nrow(X)-1,ncol(X))
   row.w = row.w / sum(row.w)
    X = sweep(X, 2, sqrt(col.w), FUN = "*")
    X = sweep(X, 1, sqrt(row.w), FUN = "*")
if (ncol(X)<nrow(X)){
##    svd.usuelle <- svd(X,nu=ncp,nv=ncp)
## lignes suivantes pour éviter qq pb de convergence de l'algo LINPACK de svd
	svd.usuelle=tryCatch.W.E(svd(X,nu=ncp,nv=ncp))$val
    if (names(svd.usuelle)[[1]]=="message"){
	  svd.usuelle=tryCatch.W.E(svd(t(X),nu=ncp,nv=ncp))$val
	  if (names(svd.usuelle)[[1]]=="d"){
	    aux=svd.usuelle$u
		svd.usuelle$u=svd.usuelle$v
		svd.usuelle$v=aux
      } else{
          bb=eigen(t(X)%*%X,symmetric=TRUE)
          svd.usuelle <- vector(mode = "list", length = 3)
          svd.usuelle$d[svd.usuelle$d<0]=0
	      svd.usuelle$d=sqrt(svd.usuelle$d)
          svd.usuelle$v=bb$vec[,1:ncp]
          svd.usuelle$u=sweep(X%*%svd.usuelle$v,2,svd.usuelle$d[1:ncp],FUN="/")
	  }
    }
    U <- svd.usuelle$u
    V <- svd.usuelle$v
    if (ncp >1){
	  mult <- sign(apply(V,2,sum))
	  mult[mult==0] <- 1
      U <- sweep(U,2,mult,FUN="*")
      V <- sweep(V,2,mult,FUN="*")
    }
    U <- sweep(as.matrix(U), 1, sqrt(row.w), FUN = "/")
    V <- sweep(as.matrix(V), 1, sqrt(col.w), FUN = "/")
}
else{
##    svd.usuelle <- svd(t(X),nu=ncp,nv=ncp)
	svd.usuelle=tryCatch.W.E(svd(t(X),nu=ncp,nv=ncp))$val
    if (names(svd.usuelle)[[1]]=="message"){
	  svd.usuelle=tryCatch.W.E(svd(X,nu=ncp,nv=ncp))$val
	  if (names(svd.usuelle)[[1]]=="d"){
	    aux=svd.usuelle$u
		svd.usuelle$u=svd.usuelle$v
		svd.usuelle$v=aux
      } else{
          bb=eigen(X%*%t(X),symmetric=TRUE)
          svd.usuelle <- vector(mode = "list", length = 3)
          svd.usuelle$d[svd.usuelle$d<0]=0
	      svd.usuelle$d=sqrt(svd.usuelle$d)
          svd.usuelle$v=bb$vec[,1:ncp]
          svd.usuelle$u=sweep(t(X)%*%svd.usuelle$v,2,svd.usuelle$d[1:ncp],FUN="/")
	  }
    }
    U <-  svd.usuelle$v
    V <- svd.usuelle$u
    mult <- sign(apply(V,2,sum))
	mult[mult==0] <- 1
    V <- sweep(V,2,mult,FUN="*")
    U <- sweep(U,2,mult,FUN="*")
    U <- sweep(U, 1, sqrt(row.w), FUN = "/")
    V <- sweep(V, 1, sqrt(col.w), FUN = "/")
}
    vs <- svd.usuelle$d[1:min(ncol(X),nrow(X)-1)]
	num <- which(vs[1:ncp]<1e-15)
    if (length(num)==1){
	  U[,num] <- U[,num]*vs[num]
      V[,num] <- V[,num]*vs[num]
	} 
    if (length(num)>1){
	  U[,num] <- sweep(U[,num],2,vs[num],FUN="*")
      V[,num] <- sweep(V[,num],2,vs[num],FUN="*")
	}
    res <- list(vs = vs, U = U, V = V)
    return(res)
}
