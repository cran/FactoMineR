svd.triplet = function (X, row.w = NULL, col.w = NULL) {
   if (is.null(row.w)) row.w <- rep(1/nrow(X), nrow(X))
   if (is.null(col.w)) col.w <- rep(1, ncol(X))
   row.w = row.w / sum(row.w)
    X = sweep(X, 2, sqrt(col.w), FUN = "*")
    X = sweep(X, 1, sqrt(row.w), FUN = "*")
if (ncol(X)<nrow(X)){
    svd.usuelle <- svd(X)
    for (i in 1:ncol(svd.usuelle$v)) {
        if (sum(svd.usuelle$v[, i]) < 0) {
            svd.usuelle$v[, i] <- - svd.usuelle$v[, i]
            svd.usuelle$u[, i] <- - svd.usuelle$u[, i]
        }
    }
    U <-  svd.usuelle$u[, 1:min(ncol(X), nrow(X) - 1)]
    U = sweep(as.matrix(U), 1, sqrt(row.w), FUN = "/")
    V <- svd.usuelle$v[, 1:min(ncol(X), nrow(X) - 1)]
    V = sweep(as.matrix(V), 1, sqrt(col.w), FUN = "/")
}
else{
    svd.usuelle <- svd(t(X))
    for (i in 1:ncol(svd.usuelle$u)) {
        if (sum(svd.usuelle$u[, i]) < 0) {
            svd.usuelle$u[, i] <- -svd.usuelle$u[, i]
            svd.usuelle$v[, i] <- -svd.usuelle$v[, i]
        }
    }
    U <-  svd.usuelle$v[, 1:min(ncol(X), nrow(X) - 1)]
    U = sweep(U, 1, sqrt(row.w), FUN = "/")
    V <- svd.usuelle$u[, 1:min(ncol(X), nrow(X) - 1)]
    V = sweep(V, 1, sqrt(col.w), FUN = "/")
}

    vs <- svd.usuelle$d[1:min(ncol(X), nrow(X) - 1)]
    res <- list(vs = vs, U = U, V = V)
    return(res)
}
