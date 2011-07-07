PCA <- function (X, scale.unit = TRUE, ncp = 5, ind.sup = NULL, quanti.sup = NULL, 
    quali.sup = NULL, row.w = NULL, col.w = NULL, graph = TRUE, 
    axes = c(1, 2)) 
{
    moy.p <- function(V, poids) {
        res <- sum(V * poids)/sum(poids)
    }
    ec <- function(V, poids) {
        res <- sqrt(sum(V^2 * poids)/sum(poids))
    }
    X <- as.data.frame(X)
    if (any(is.na(X))) {
        warnings("Missing values are imputed by the mean of the variable: you should use the imputePCA function of the missMDA package")
        if (is.null(quali.sup)) 
#            for (j in 1:ncol(X)) X[, j] <- replace(X[, j], is.na(X[, j]), mean(X[, j], na.rm = TRUE))
            X[is.na(X)] = matrix(apply(X,2,mean,na.rm=TRUE),ncol=ncol(X),nrow=nrow(X),byrow=TRUE)[is.na(X)]
        else for (j in (1:ncol(X))[-quali.sup]) X[, j] <- replace(X[, j], is.na(X[, j]), mean(X[, j], na.rm = TRUE))
    }
    if (is.null(rownames(X))) rownames(X) = 1:nrow(X)
    if (is.null(colnames(X))) colnames(X) = paste("V", 1:ncol(X), sep = "")
    colnames(X)[colnames(X)==""] <- paste("V",1:sum(colnames(X)==""),sep="")
    rownames(X)[is.null(rownames(X))] <- paste("row",1:sum(rownames(X)==""),sep="")
#    for (j in 1:ncol(X)) if (colnames(X)[j] == "") 
#        colnames(X)[j] = paste("V", j, sep = "")
#    for (j in 1:nrow(X)) if (is.null(rownames(X)[j])) 
#        rownames(X)[j] = paste("row", j, sep = "")
    Xtot <- X
    if (!is.null(quali.sup)) 
        X <- X[, -quali.sup]
    if (!any(apply(X, 2, is.numeric))) {
        auxi = NULL
        for (j in 1:ncol(X)) if (!is.numeric(X[, j])) 
            auxi = c(auxi, colnames(X)[j])
        stop(paste("\nThe following variables are not quantitative: ", 
            auxi))
    }
    todelete <- c(quali.sup, quanti.sup)
    if (!is.null(todelete)) X <- Xtot[, -todelete]
    if (!is.null(ind.sup)) {
        X.ind.sup <- X[ind.sup, , drop = FALSE]
        X <- X[-ind.sup, , drop = FALSE]
    }
    ncp <- min(ncp, nrow(X) - 1, ncol(X))
    if (is.null(row.w)) row.w <- rep(1, nrow(X))
    row.w.init <- row.w
    row.w <- row.w/sum(row.w)
    if (is.null(col.w)) col.w <- rep(1, ncol(X))
    centre <- apply(X, 2, moy.p, row.w)
    data <- X
    X <- as.matrix(sweep(as.matrix(X), 2, centre, FUN = "-"))
    if (scale.unit) {
        ecart.type <- apply(X, 2, ec, row.w)
        ecart.type[ecart.type <= 1e-16] <- 1
        X <- sweep(as.matrix(X), 2, ecart.type, FUN = "/")
    }
    else ecart.type <- rep(1, length(centre))
dist2.ind <- apply(sweep(X,2,sqrt(col.w),FUN="*")^2,1,sum)
dist2.var <- apply(sweep(X,1,sqrt(row.w),FUN="*")^2,2,sum)
    res.call <- list(row.w = (row.w/sum(row.w)), col.w = col.w, 
        scale.unit = scale.unit, ncp = ncp, centre = centre, 
        ecart.type = ecart.type, X = Xtot)
    tmp <- svd.triplet(X, row.w = row.w, col.w = col.w,ncp=ncp)
    eig <- tmp$vs^2
    vp <- as.data.frame(matrix(NA, length(eig), 3))
    rownames(vp) <- paste("comp", 1:length(eig))
    colnames(vp) <- c("eigenvalue", "percentage of variance", 
        "cumulative percentage of variance")
    vp[, "eigenvalue"] <- eig
    vp[, "percentage of variance"] <- (eig/sum(eig)) * 100
    vp[1, "cumulative percentage of variance"] <- vp[1, "percentage of variance"]
    if (length(eig) >= 2) 
        for (i in 2:length(eig)) vp[i, "cumulative percentage of variance"] <- vp[i, 
            "percentage of variance"] + vp[i - 1, "cumulative percentage of variance"]
    V <- tmp$V
    U <- tmp$U
	eig <- eig[1:ncp]
    coord.ind <- sweep(as.matrix(U), 2, sqrt(eig), FUN = "*")
    coord.var <- sweep(as.matrix(V), 2, sqrt(eig), FUN = "*")
    contrib.var <- sweep(as.matrix(coord.var^2), 2, eig, "/")
    contrib.var <- sweep(as.matrix(contrib.var), 1, col.w, "*")
##    dist2 <- apply(coord.var^2, 1, sum)
dist2 <- dist2.var
    cor.var <- sweep(as.matrix(coord.var), 1, sqrt(dist2), FUN = "/")
    cos2.var <- cor.var^2
    rownames(coord.var) <- rownames(cos2.var) <- rownames(cor.var) <- rownames(contrib.var) <- colnames(X)
    colnames(coord.var) <- colnames(cos2.var) <- colnames(cor.var) <- colnames(contrib.var) <- paste("Dim", 
        c(1:ncol(V)), sep = ".")
    res.var <- list(coord = coord.var[, 1:ncp], cor = cor.var[, 
        1:ncp], cos2 = cos2.var[, 1:ncp], contrib = contrib.var[, 
        1:ncp] * 100)
##    dist2 <- apply(coord.ind^2, 1, sum)
dist2 <- dist2.ind
    cos2.ind <- sweep(as.matrix(coord.ind^2), 1, dist2, FUN = "/")
    contrib.ind <- sweep(as.matrix(coord.ind^2), 1, row.w/sum(row.w), 
        FUN = "*")
    contrib.ind <- sweep(as.matrix(contrib.ind), 2, eig, FUN = "/")
    rownames(coord.ind) <- rownames(cos2.ind) <- rownames(contrib.ind) <- names(dist2) <- rownames(X)
    colnames(coord.ind) <- colnames(cos2.ind) <- colnames(contrib.ind) <- paste("Dim", 
        c(1:ncol(U)), sep = ".")
    res.ind <- list(coord = coord.ind[, 1:ncp], cos2 = cos2.ind[, 
        1:ncp], contrib = contrib.ind[, 1:ncp] * 100, dist = sqrt(dist2))
    res <- list(eig = vp, var = res.var, ind = res.ind, svd = tmp)
    if (!is.null(ind.sup)) {
        if (is.null(ecart.type)) 
            ecart.type <- rep(1, length(centre))
        X.ind.sup <- as.matrix(sweep(as.matrix(X.ind.sup), 2, 
            centre, FUN = "-"))
        X.ind.sup <- as.matrix(sweep(as.matrix(X.ind.sup), 2, 
            ecart.type, FUN = "/"))
        coord.ind.sup <- sweep(as.matrix(X.ind.sup), 2, col.w, 
            FUN = "*")
##        coord.ind.sup <- coord.ind.sup %*% tmp$V
        coord.ind.sup <- crossprod(t(coord.ind.sup),tmp$V)
dist2 <- apply(sweep(X.ind.sup,2,sqrt(col.w),FUN="*")^2,1,sum)
        cos2.ind.sup <- sweep(as.matrix(coord.ind.sup^2), 1, 
            dist2, FUN = "/")
        coord.ind.sup <- as.data.frame(coord.ind.sup[, 1:ncp, 
            drop = F])
        cos2.ind.sup <- as.data.frame(cos2.ind.sup[, 1:ncp, drop = F])
        colnames(coord.ind.sup) <- colnames(cos2.ind.sup) <- paste("Dim", 
            c(1:ncp), sep = ".")
        rownames(coord.ind.sup) <- rownames(cos2.ind.sup) <- names(dist2) <- rownames(X.ind.sup)
        res.ind.sup <- list(coord = coord.ind.sup, cos2 = cos2.ind.sup, 
            dist = sqrt(dist2))
        res$ind.sup = res.ind.sup
        res.call$ind.sup = ind.sup
    }
    if (!is.null(quanti.sup)) {
        X.quanti.sup <- as.data.frame(Xtot[, quanti.sup])
        if (!is.null(ind.sup)) 
            X.quanti.sup <- as.data.frame(X.quanti.sup[-ind.sup, 
                ])
        colnames(X.quanti.sup) <- colnames(Xtot)[quanti.sup]        
        res.call$quanti.sup = X.quanti.sup
        centre.sup <- apply(X.quanti.sup, 2, moy.p, row.w)
        X.quanti.sup <- as.matrix(sweep(as.matrix(X.quanti.sup), 
            2, centre.sup, FUN = "-"))
        if (scale.unit) {
            ecart.type.sup <- apply(X.quanti.sup, 2, ec, row.w)
            ecart.type.sup[ecart.type.sup <= 1e-16] <- 1
            X.quanti.sup <- as.matrix(sweep(as.matrix(X.quanti.sup), 
                2, ecart.type.sup, FUN = "/"))
        }
        coord.vcs <- sweep(as.matrix(t(X.quanti.sup)), 2, row.w, 
            FUN = "*")
##        coord.vcs <- coord.vcs %*% tmp$U
        coord.vcs <- crossprod(t(coord.vcs),tmp$U)
        col.w.vcs <- rep(1, ncol(coord.vcs))
        cor.vcs <- matrix(NA, ncol(X.quanti.sup), ncol(tmp$U))
        sigma <- apply(X.quanti.sup, 2, ec, row.w)
dist2 <- apply(sweep(X.quanti.sup,1,sqrt(row.w),FUN="*")^2,2,sum)
##        cor.vcs <- sweep(as.matrix(coord.vcs), 1, sigma, FUN = "/")
    cor.vcs <- sweep(as.matrix(coord.vcs), 1, sqrt(dist2), FUN = "/")
        cos2.vcs <- as.data.frame(cor.vcs^2)
        coord.vcs <- as.data.frame(coord.vcs)
        cor.vcs <- as.data.frame(cor.vcs)
        colnames(coord.vcs) <- colnames(cor.vcs) <- colnames(cos2.vcs) <- paste("Dim", 
            c(1:ncol(cor.vcs)), sep = ".")
        rownames(coord.vcs) <- rownames(cor.vcs) <- rownames(cos2.vcs) <- colnames(Xtot)[quanti.sup]
        res.quanti.sup <- list(coord = coord.vcs[, 1:ncp], cor = cor.vcs[, 
            1:ncp], cos2 = cos2.vcs[, 1:ncp])
        res$quanti.sup = res.quanti.sup
    }
    if (!is.null(quali.sup)) {
        X.quali.sup <- as.data.frame(Xtot[, quali.sup])
        if (!is.null(ind.sup)) 
            X.quali.sup <- as.data.frame(X.quali.sup[-ind.sup, 
                ])
        colnames(X.quali.sup) <- colnames(Xtot)[quali.sup]
        nombre <- modalite <- NULL
        for (i in 1:ncol(X.quali.sup)) {
            var <- as.factor(X.quali.sup[, i])
            n.mod <- nlevels(var)
            modalite <- c(modalite, n.mod)
            bary <- matrix(NA, n.mod, ncol(X))
            for (j in 1:n.mod) {
                ind <- levels(var)[j]
                bary[j, ] <- apply(data[which(var == ind), ], 2, moy.p, row.w[which(var == ind)])
### modif Avril 2011
##                nombre <- c(nombre, length(var[which(var == ind)]))
                nombre <- c(nombre, sum(row.w.init[which(var == ind)]))
            }
            colnames(bary) <- colnames(X)
            if ((levels(var)[1] %in% (1:nrow(X))) | (levels(var)[1] %in% c("y", "Y", "n", "N"))) row.names(bary) <- paste(colnames(X.quali.sup)[i], as.character(levels(var)))
            else row.names(bary) <- as.character(levels(var))
            if (i == 1)  barycentre <- as.data.frame(bary)
            else barycentre <- rbind(barycentre, as.data.frame(bary))
        }
        bary <- as.matrix(sweep(as.matrix(barycentre), 2, centre, FUN = "-"))
        if (!is.null(ecart.type)) bary <- as.matrix(sweep(as.matrix(bary), 2, ecart.type, FUN = "/"))
dist2 <- apply(sweep(as.matrix(bary)^2,2,col.w,FUN="*"),1,sum)
        coord.barycentre <- sweep(as.matrix(bary), 2, col.w, FUN = "*")
        coord.barycentre <- crossprod(t(coord.barycentre),tmp$V)
        colnames(coord.barycentre) <- paste("Dim", 1:ncol(coord.barycentre), sep = ".")
##        dist2 <- apply(coord.barycentre^2, 1, sum)
        cos2.bary.sup <- sweep(as.matrix(coord.barycentre^2), 1, dist2, FUN = "/")
        vtest <- sweep(as.matrix(coord.barycentre), 2, sqrt(eig), FUN = "/")
        vtest <- sweep(as.matrix(vtest), 1, sqrt(nombre/((sum(row.w.init) - nombre)/(sum(row.w.init) - 1))), FUN = "*")
        cos2.bary.sup <- cos2.bary.sup[, 1:ncp]
        coord.barycentre <- coord.barycentre[, 1:ncp]
        vtest <- vtest[, 1:ncp]
        dimnames(cos2.bary.sup) <- dimnames(vtest) <- dimnames(coord.barycentre)
        names(dist2) <- rownames(coord.barycentre)
        res.quali.sup <- list(coord = coord.barycentre, cos2 = cos2.bary.sup, v.test = vtest, dist = sqrt(dist2))
        call.quali.sup <- list(quali.sup = X.quali.sup, modalite = modalite, nombre = nombre, barycentre = barycentre, numero = quali.sup)
        res$quali.sup = res.quali.sup
        res.call$quali.sup = call.quali.sup
    }
    res$call = res.call
    class(res) <- c("PCA", "list ")
    if (graph) {
        plot.PCA(res, choix = "ind", axes = axes)
        plot.PCA(res, choix = "var", axes = axes)
    }
    return(res)
}