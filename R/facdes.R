facdes <- function (res){

  if (!inherits(res, "PCA")&!inherits(res, "CA")&!inherits(res, "MCA")&!inherits(res, "MFA")) stop("non convenient data")

  resultat=list()
  for (j in 1:ncol(res$var$coord)){
  print(res$var$coord[,j])
    resultat[[j]] <- as.data.frame(sort(res$var$coord[,j]))
  }
  names(resultat) <- colnames(res$var$coord)
  
  return(res)
}
