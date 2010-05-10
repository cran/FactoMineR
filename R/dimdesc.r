dimdesc=function (res, axes = 1:3, proba = 0.05) 
{
    if (!inherits(res, "PCA") & !inherits(res, "CA") & !inherits(res, "MCA") & !inherits(res, 
        "MFA") & !inherits(res, "HMFA") & !inherits(res, "DMFA") & 
        !inherits(res, "AFMD")) 
        stop("non convenient data")
    if (inherits(res, "CA")) {
        row.supp = res$call$row.sup
        result = structure(vector(mode = "list", length = length(axes)), 
            names = colnames(res$row$coord)[axes])
        for (k in axes) {
            if (!is.null(row.supp)) 
                tableau = cbind.data.frame(res$row$coord[, axes[k]], 
                  res$call$X[-row.supp, ])
            else tableau = cbind.data.frame(res$row$coord[, axes[k]], 
                res$call$X)
            result[[k]] = condes(tableau, 1, proba = proba)
        }
    }
    else {
        ind.supp = res$call$ind.sup
        result = structure(vector(mode = "list", length = length(axes)), 
            names = colnames(res$ind$coord)[axes])
        for (k in axes) {
            if (!is.null(ind.supp)) 
                tableau = cbind.data.frame(res$ind$coord[, axes[k]], 
                  res$call$X[-ind.supp, ])
            else tableau = cbind.data.frame(res$ind$coord[, axes[k]], 
                res$call$X)
            result[[k]] = condes(tableau, 1, proba = proba)
        }
    }
    return(result)
}