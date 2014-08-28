dimdesc=function (res, axes = 1:3, proba = 0.05) 
{
    if (!inherits(res, "PCA") & !inherits(res, "CA") & !inherits(res, "MCA") & !inherits(res, 
        "MFA") & !inherits(res, "HMFA") & !inherits(res, "DMFA") &  !inherits(res, "FAMD")) 
        stop("non convenient data")
      if (inherits(res, "CA")) {
          result <- structure(vector(mode = "list", length = length(axes)), names = colnames(res$row$coord)[axes])
		  tableau <- res$row$coord[,axes,drop=FALSE]
          if (!is.null(res$call$row.sup))  tableau = rbind.data.frame(tableau,res$row.sup$coord[,axes,drop=FALSE])	
		  tableaucol <- res$col$coord[, axes,drop=FALSE]
          if (!is.null(res$call$col.sup))  tableaucol = rbind.data.frame(tableaucol,res$col.sup$coord[,axes,drop=FALSE])	
          for (k in 1:length(axes)) {
		    tab <- tableau[order(tableau[,k,drop=FALSE]),k,drop=FALSE]
		    colnames(tab)="coord"
            quali <- NULL
		    if (!is.null(res$call$quali.sup)) {
## Faut il mettre un poids ou pas pour le test ?
			  w=res$call$marge.row*length(res$call$marge.row)
			  quali <- condes(cbind.data.frame(res$row$coord[,axes[k],drop=FALSE],res$call$Xtot[,res$call$quali.sup,drop=FALSE]),1,weights=w,proba=proba)
			}
		    tabcol <- tableaucol[order(tableaucol[,k,drop=FALSE]),k,drop=FALSE]
		    colnames(tabcol)="coord"
		    if (!is.null(quali)) result[[k]] = list(row=tab,col=tabcol,quali=quali$quali,category=quali$category)
		    else result[[k]] = list(row=tab,col=tabcol)
		  }
      }
     # if (inherits(res, "CA")) {
         # result <- structure(vector(mode = "list", length = length(axes)), names = colnames(res$row$coord)[axes])
         # for (k in 1:length(axes)) {
		   # w=NULL
		   ## w=res$call$marge.row*res$call$N
		   # if (!is.null(res$call$row.sup)) tabcol <- condes(cbind.data.frame(res$row$coord[,axes[k],drop=FALSE],res$call$Xtot[-res$call$row.sup,]),1,w=w,proba=proba)
		   # else tabcol <- condes(cbind.data.frame(res$row$coord[,axes[k],drop=FALSE],res$call$Xtot),1,w=w,proba=proba)
		   ## w=res$call$marge.col*res$call$N
		   # if (!is.null(res$call$col.sup)) tab <- condes(cbind.data.frame(res$col$coord[,axes[k],drop=FALSE],t(res$call$Xtot[,-c(res$call$col.sup,res$call$quali.sup)])),1,w=w,proba=proba)
		   # else {
		   # if (!is.null(res$call$quali.sup)) tab <- condes(cbind.data.frame(res$col$coord[,axes[k],drop=FALSE],t(res$call$Xtot[,-res$call$quali.sup])),1,w=w,proba=proba)
		   # else tab <- condes(cbind.data.frame(res$col$coord[,axes[k],drop=FALSE],t(res$call$X)),1,w=w,proba=proba)
		   # }
		   # result[[k]] = list(row=tab,col=tabcol)
		 # }
     # }
    else {
        ind.supp = res$call$ind.sup
        result = structure(vector(mode = "list", length = length(axes)), names = colnames(res$ind$coord)[axes])
        for (k in 1:length(axes)) {
            if (!is.null(ind.supp)) tableau = cbind.data.frame(res$ind$coord[, axes[k],drop=FALSE], res$call$X[-ind.supp, ])
            else tableau = cbind.data.frame(res$ind$coord[, axes[k],drop=FALSE], res$call$X)
#            result[[k]] <- condes(tableau, 1, proba = proba)
            result[[k]] <- condes(tableau, 1, proba = proba, weights=res$call$row.w.init)
        }
    }
    return(result)
}