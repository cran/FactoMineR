dimdesc=function (res, axes = 1:3, proba = 0.05) 
{
    if (!inherits(res, "PCA") & !inherits(res, "CA") & !inherits(res, "MCA") & !inherits(res, 
        "MFA") & !inherits(res, "HMFA") & !inherits(res, "DMFA") &  !inherits(res, "FAMD")) 
        stop("non convenient data")
    if (inherits(res, "CA")) {
        result <- structure(vector(mode = "list", length = length(axes)), names = colnames(res$row$coord)[axes])
		tableau <- res$row$coord[,axes]
        if (!is.null(res$call$row.sup))  tableau = rbind.data.frame(tableau,res$row.sup$coord[,axes])	
		tableaucol <- res$col$coord[, axes]
        if (!is.null(res$call$col.sup))  tableaucol = rbind.data.frame(tableaucol,res$col.sup$coord[,axes])	
        for (k in 1:length(axes)) {
		  tab <- tableau[order(tableau[,k]),k,drop=FALSE]
		  colnames(tab)="coord"
		  tabcol <- tableaucol[order(tableaucol[,k]),k,drop=FALSE]
		  colnames(tabcol)="coord"
		  result[[k]] = list(row=tab,col=tabcol)
		}
    }
    else {
        ind.supp = res$call$ind.sup
        result = structure(vector(mode = "list", length = length(axes)), names = colnames(res$ind$coord)[axes])
        for (k in 1:length(axes)) {
            if (!is.null(ind.supp)) tableau = cbind.data.frame(res$ind$coord[, axes[k]], res$call$X[-ind.supp, ])
            else tableau = cbind.data.frame(res$ind$coord[, axes[k]], res$call$X)
            result[[k]] = condes(tableau, 1, proba = proba)
        }
    }
    return(result)
}