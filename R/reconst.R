reconst <- function (res, ncp=NULL){
  if (is.null(ncp)) ncp = ncol(res$ind$coord)
  if (inherits(res, "MFA")) coord.var = sweep(as.matrix(res$quanti.var$coord)[,1:ncp,drop=F],1,res$call$col.w,FUN="*")
  if (inherits(res, "PCA")) coord.var = as.matrix(res$var$coord)[,1:ncp,drop=F]
  coord.ind = as.matrix(res$ind$coord)[,1:ncp,drop=F]
  Yrec = coord.ind%*%t(sweep(coord.var,2,sqrt(res$eig[1:ncp,1]),FUN="/"))
if (inherits(res, "PCA")) {
  Yrec = sweep(Yrec,2,res$call$ecart.type,FUN="*")
  Yrec = sweep(Yrec,2,res$call$centre,FUN="+")
}
if (inherits(res, "MFA")) {
  ecarttype=res$separate.analyses[[1]][["call"]][["ecart.type"]]
  for (g in 2:length(res$call$group)) ecarttype=c(ecarttype,res$separate.analyses[[g]][["call"]][["ecart.type"]])
  moy=res$separate.analyses[[1]][["call"]][["centre"]]
  for (g in 2:length(res$call$group)) moy=c(moy,res$separate.analyses[[g]][["call"]][["centre"]])
  Yrec = sweep(Yrec,2,ecarttype,FUN="*")
  Yrec = sweep(Yrec,2,res$call$col.w,FUN="/")
  Yrec = sweep(Yrec,2,moy,FUN="+")
  }
  return(Yrec)
}

