dimdesc <- function (res, axes=1:3, proba=0.05){

  if (!inherits(res, "PCA")&!inherits(res, "CA")&!inherits(res, "MCA")&!inherits(res, "MFA")&!inherits(res, "HMFA")&!inherits(res, "FDA")&!inherits(res, "DMFA")&!inherits(res, "AFMD")) stop("non convenient data")

  resultat=list()
  quanti = quanti.sup = quali = quali.sup = NULL
  if (inherits(res, "PCA")){
    quanti = res$var$coord
    if (!is.null(res$quanti.sup$coord)) quanti.sup = res$quanti.sup$coord
    if (!is.null(res$quali.sup$vtest)) quali.sup = res$quali.sup$vtest
  }
  if (inherits(res, "MFA")){
    if (!is.null(res$quanti.var$coord)) quanti = res$quanti.var$coord
    if (!is.null(res$quanti.var.sup$coord)) quanti.sup = res$quanti.var.sup$coord
    if (!is.null(res$quali.var$v.test)) quali = res$quali.var$v.test
    if (!is.null(res$quali.var.sup$v.test)) quali.sup = res$quali.var.sup$v.test
  }
  if (inherits(res, "HMFA")){
    if (!is.null(res$quanti.var$coord)) quanti = res$quanti.var$coord
    if (!is.null(res$quanti.var.sup$coord)) quanti.sup = res$quanti.var.sup$coord
    if (!is.null(res$quali.var$v.test)) quali = res$quali.var$v.test
    if (!is.null(res$quali.var.sup$v.test)) quali.sup = res$quali.var.sup$v.test
  }
  if (inherits(res, "MCA")){
    if (!is.null(res$quanti.sup$coord)) quanti.sup = res$quanti.sup$coord
    if (!is.null(res$var$vtest)) quali = res$var$vtest
    if (!is.null(res$quali.sup$vtest)) quali.sup = res$quali.sup$vtest
  }
  if (inherits(res, "CA")){
    quanti = rbind(res$row$coord,res$col$coord)
    if (!is.null(res$row.sup$coord)) quanti.sup = res$row.sup$coord
    if (!is.null(res$col.sup$coord)) quanti.sup = rbind(quanti.sup,res$col.sup$coord)
  }
  if (inherits(res, "AFDM")){
    if (!is.null(res$quanti.var$coord)) quanti = res$quanti.var$coord
    if (!is.null(res$quanti.var.sup$coord)) quanti.sup = res$quanti.var.sup$coord
    if (!is.null(res$quali.var$v.test)) quali = res$quali.var$v.test
    if (!is.null(res$quali.var.sup$v.test)) quali.sup = res$quali.var.sup$v.test
  }
  if (inherits(res, "DMFA")){
    if (!is.null(res$var$coord)) quanti = res$var$coord
    if (!is.null(res$quanti.var.sup$coord)) quanti.sup = res$quanti.var.sup$coord
    if (!is.null(res$quali.sup$v.test)) quali.sup = res$quali.sup$v.test
  }

  if (!is.null(quanti)) axes = axes[axes%in%(1:ncol(quanti))]
  else axes = axes[axes%in%(1:ncol(quali))]
  tab.quanti = rbind(quanti,quanti.sup)
  tab.quali = rbind(quali,quali.sup)
  result = structure(vector(mode = "list", length = length(axes)), names = colnames(tab.quanti)[axes])
  if (!is.null(tab.quanti)) tab.quanti = as.matrix(tab.quanti)
  if (!is.null(tab.quali)) tab.quali = as.matrix(tab.quali)
  k=0
  for (j in axes){
    k=k+1
    if (!is.null(tab.quanti)){
      seuil.cor = sqrt(1-1/(1+qf(1-proba,1,nrow(tab.quanti)-2)/(nrow(tab.quanti)-2)))
      aux = rev(sort(tab.quanti[abs(tab.quanti[,j])>seuil.cor,j]))      
        result[[k]]$quanti <- as.data.frame(aux)
        if (nrow(result[[k]]$quanti)==1) {
          result[[k]]$quanti <- matrix(aux,nrow=1)
          rownames(result[[k]]$quanti) <- names(rev(sort(tab.quanti[abs(tab.quanti[,j])>0,j]))[1])
        }
        colnames(result[[k]]$quanti) = colnames(tab.quanti)[j]
    }
    if (!is.null(tab.quali)){
      result[[k]]$quali <- as.data.frame(rev(sort(tab.quali[abs(tab.quali[,j])>qnorm(1-proba/2),j])))
      colnames(result[[k]]$quali) = colnames(tab.quali)[j]
    }
  }
  return(result)
}
