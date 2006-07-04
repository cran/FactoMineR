AFDM <- function (base, type, ncp = 5, graph = TRUE,sup.var=NULL, ind.sup = NULL){
    resultats <- MFA(base=base, group = rep(1,ncol(base)), type=type, name.group = colnames(base), num.group.sup = sup.var, ind.sup = ind.sup, graph=FALSE)
    
    class(resultats) <- c("MFA", "list")
    if (graph){
      if (any("n"%in%type)) plot(resultats,choix="ind")
      if (any("n"%in%type)) plot(resultats,choix="ind",invisible=c("ind","ind.sup"),habillage="quali")
      if (any("c"%in%type)|any("s"%in%type)) plot(resultats,choix="var")
      plot(resultats,choix="ind",invisible="quali")
      plot(resultats,choix="group",title="Variables representation")
    }
    resultats$ind <- list(coord = resultats$ind$coord, contrib = resultats$ind$contrib, cos2 = resultats$ind$cos2)
    res <- list(ind = resultats$ind, group = resultats$group)
    if (!is.null(ind.sup)) res$ind.sup <- resultats$ind.sup
    if (!is.null(resultats["quali.var"]$quali.var)) res$quali.var <- list(coord = resultats$quali.var$coord, contrib = resultats$quali.var$contrib, cos2 = resultats$quali.var$cos2, v.test = resultats$quali.var$v.test)
    if (!is.null(resultats["quanti.var"]$quanti.var)) res$quanti.var <- resultats$quanti.var
    if (!is.null(resultats$quali.var.sup)) res$quali.var.sup <- list(coord = resultats$quali.var.sup$coord, contrib = resultats$quali.var.sup$contrib, cos2 = resultats$quali.var.sup$cos2, v.test = resultats$quali.var.sup$v.test)
    if (!is.null(resultats$quanti.var.sup)) res$quanti.var.sup <- resultats$quanti.var.sup
    return(res)
}
