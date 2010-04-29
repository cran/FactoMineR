MFA <- function (base, group, type = rep("s",length(group)), ind.sup = NULL, ncp = 5, name.group = NULL, num.group.sup = NULL, graph = TRUE, weight.col.mfa = NULL, row.w = NULL, axes=c(1,2)){

    moy.p <- function(V, poids){
      res <- sum(V*poids) / sum(poids)
    }
    ec <- function(V, poids) {
        res <- sqrt(sum(V^2 * poids)/sum(poids))
    }

    base <- as.data.frame(base)
    if (!is.null(ind.sup)) {
      base <- rbind.data.frame(base[-ind.sup,],base[ind.sup,])
      ind.sup <- (nrow(base)-length(ind.sup)+1) : nrow(base)
    }
    if (is.null(rownames(base))) rownames(base) = 1:nrow(base)
    if (is.null(colnames(base))) colnames(base) = paste("V",1:ncol(base),sep="")
    for (j in 1:ncol(base)) if (colnames(base)[j]=="") colnames(base)[j] = paste("V",j,sep="")
    for (j in 1:nrow(base)) if (is.null(rownames(base)[j])) rownames(base)[j] = paste("row",j,sep="")
    nbre.var <- ncol(base)
    nbre.group <- length(group)
    group.actif <- NULL
    if ("n"%in%type){
      niveau = NULL
      for (j in 1:ncol(base)){
        if (!is.numeric(base[,j])) niveau = c(niveau,levels(base[,j]))
      }
      for (j in 1:ncol(base)) {
        if (!is.numeric(base[,j])){
          if (sum(niveau%in%levels(base[,j]))!=nlevels(base[,j])) levels(base[,j]) = paste(colnames(base)[j],levels(base[,j]),sep="_")
        }
      }
    }
    for (i in 1:nbre.group) if (!(i%in%num.group.sup)) group.actif <- c(group.actif,i)
    group.mod <- group
    nbre.ind <- nrow(base)
    nb.actif <- nbre.ind-length(ind.sup)
    if (nbre.var != sum(group)) stop("not convenient group definition")
    if (nbre.group != length(type)) stop("not convenient type definition")
    base <- as.data.frame(base)
    if (!inherits(base, "data.frame")) stop("base should be a data.frame")
    res.separe <- vector(mode = "list", length = nbre.group)
    if (is.null(name.group)) name.group <- paste("group", 1:nbre.group, sep = ".")
    names(res.separe) <- name.group
    ind.grpe <- 0
    if (any(is.na(base))){
      if (!("n"%in%type)) for (j in 1:ncol(base)) base[,j] <- replace(base[,j],is.na(base[,j]),mean(base[,j],na.rm=TRUE))
      else{
        if (type[1]!="n") for (j in 1:group[1]) base[,j] <- replace(base[,j],is.na(base[,j]),mean(base[,j],na.rm=TRUE))
        for (g in 2:nbre.group){
         if (type[g]!="n") for (j in (sum(group[1:(g-1)])+1):sum(group[1:g])) base[,j] <- replace(base[,j],is.na(base[,j]),mean(base[,j],na.rm=TRUE))
      }}
    }
    if (is.null(row.w)) {
      row.w <- rep(1,nb.actif)
      row.w.moy.ec <- as.integer(!((1:nbre.ind)%in%ind.sup))
    } else row.w.moy.ec <- row.w
    for (g in 1:nbre.group) {
        aux.base <- as.data.frame(base[, (ind.grpe + 1):(ind.grpe + group[g])])
        dimnames(aux.base) <- list(rownames(base),colnames(base)[(ind.grpe + 1):(ind.grpe + group[g])])
        if (type[g] == "s") res.separe[[g]] <- PCA(aux.base, ind.sup =ind.sup, scale.unit = TRUE, ncp = ncp, row.w=row.w, graph = FALSE, col.w = weight.col.mfa[(ind.grpe + 1):(ind.grpe + group[g])])
        if (type[g] == "c") res.separe[[g]] <- PCA(aux.base, ind.sup =ind.sup, scale.unit = FALSE, ncp = ncp, row.w=row.w,graph = FALSE, col.w = weight.col.mfa[(ind.grpe + 1):(ind.grpe + group[g])])
        if (type[g] == "n") {
          for (v in (ind.grpe + 1):(ind.grpe + group[g])) {
            if (!is.factor(base[, v])) stop("factors are not defined in the qualitative groups")
          }
          res.separe[[g]] <- MCA(aux.base, ind.sup = ind.sup, graph = FALSE, row.w=row.w)
        }
        ind.grpe <- ind.grpe + group[g]
    }
    data <- matrix(0,nbre.ind,0)
    ind.grpe <- ind.grpe.mod <- 0
    ponderation <- vector(length = sum(group.mod))
if (is.null(weight.col.mfa)) weight.col.mfa <- rep(1,length(ponderation))
    for (g in 1:nbre.group) {
        aux.base <- base[, (ind.grpe + 1):(ind.grpe + group[g])]
        aux.base <- as.data.frame(aux.base)
        colnames(aux.base) <- colnames(base)[(ind.grpe + 1):(ind.grpe + group[g])]
        if (type[g] == "s") {
          centre.aux.base <- apply(as.data.frame(aux.base), 2, moy.p, row.w.moy.ec)
          aux.base <- as.matrix(sweep(as.data.frame(aux.base), 2, centre.aux.base, FUN = "-"))
          ecart.type.aux.base <- apply(as.data.frame(aux.base), 2, ec, row.w.moy.ec)
          ecart.type.aux.base[ecart.type.aux.base <= 1e-08] <- 1
          aux.base <- as.matrix(sweep(as.data.frame(aux.base), 2, ecart.type.aux.base, FUN = "/"))
          type[g]="c"
        }
        if (type[g] == "c") {
          data <- cbind.data.frame(data, aux.base)
          ponderation[(ind.grpe.mod + 1):(ind.grpe.mod + group.mod[g])] <- 1/res.separe[[g]]$eig[1,1]
        }
        if (type[g] == "n") {
            tmp <- tab.disjonctif(aux.base)
            group.mod[g] <- ncol(tmp)
#            poids.tmp <- apply(tmp[1:nb.actif,], 2, sum)
#            poids.tmp <- (nb.actif - poids.tmp)/nb.actif
#            ponderation[(ind.grpe.mod + 1):(ind.grpe.mod + group.mod[g])] <- poids.tmp/(res.separe[[g]]$eig[1,1] * group[g])
            tmp <- sweep(tmp,1,row.w.moy.ec/sum(row.w.moy.ec),FUN="*")
            poids.tmp <- apply(tmp, 2, sum)
            ponderation[(ind.grpe.mod + 1):(ind.grpe.mod + group.mod[g])] <- poids.tmp/(res.separe[[g]]$eig[1,1] * group[g])
            centre.tmp <- apply(tmp, 2, moy.p, row.w.moy.ec)
            tmp <- as.matrix(sweep(tmp, 2, centre.tmp, FUN = "-"))
            ecart.type.tmp <- apply(tmp, 2, ec, row.w.moy.ec)
            ecart.type.tmp[ecart.type.tmp <= 1e-08] <- 1
            tmp <- as.matrix(sweep(tmp, 2, ecart.type.tmp, FUN = "/"))
            data <- cbind.data.frame(data, as.data.frame(tmp))
        }
        ind.grpe <- ind.grpe + group[g]
        ind.grpe.mod <- ind.grpe.mod + group.mod[g]
    }
    data.group.sup.indice <- data.group.sup <- NULL
    data.pca <- data
    if (!is.null(num.group.sup)){
      ponderation.tot <- ponderation
      ponderation.group.sup <- NULL
      nb.of.var <- 0
      supp.quanti <- supp.quali <- NULL
      colnames.data.group.sup <- NULL
      for (i in 1:nbre.group) {
        if (i%in%num.group.sup){
          if (type[i]=="c") supp.quanti = c(supp.quanti,(1+nb.of.var):(nb.of.var+group.mod[i]))
          if (type[i]=="n") supp.quali = c(supp.quali,(1+nb.of.var):(nb.of.var+group.mod[i]))
          if (is.null(data.group.sup)) data.group.sup <- as.data.frame(data[,(1+nb.of.var):(nb.of.var+group.mod[i])])
          else data.group.sup <- cbind.data.frame(data.group.sup,data[,(1+nb.of.var):(nb.of.var+group.mod[i])])
          if (ncol(data.group.sup)>1) colnames.data.group.sup <- c(colnames.data.group.sup,colnames(data)[(1+nb.of.var):(nb.of.var+group.mod[i])])
          else colnames.data.group.sup <- colnames(data)[1+nb.of.var]
          ponderation.group.sup <- c(ponderation.group.sup,ponderation[(1+nb.of.var):(nb.of.var+group.mod[i])])
        }
        nb.of.var <- nb.of.var + group.mod[i]
      }
      colnames(data.group.sup) <- colnames.data.group.sup
      ponderation <- ponderation.tot[-c(supp.quanti,supp.quali)]
      data <- data[,-c(supp.quanti,supp.quali)]
      data.group.sup.indice <- (ncol(data)+1):(ncol(data)+ncol(data.group.sup))
      data.pca <- cbind.data.frame(data,data.group.sup)
    }
    ncp.tmp <- min(nb.actif-1, ncol(data))
    ind.var <- 0
    ind.quali <- NULL
    for (g in 1:nbre.group) {
        if (type[g] == "n")  ind.quali <- c(ind.quali, c((ind.var + 1):(ind.var + group[g])))
        ind.var <- ind.var + group[g]
    }
    aux.quali.sup.indice <- aux.quali.sup <- data.sup <- NULL
    if (!is.null(ind.quali)){
      aux.quali.sup <- as.data.frame(base[, ind.quali])
      if (is.null(data.group.sup)) aux.quali.sup.indice <- (ncol(data)+1):(ncol(data)+ncol(aux.quali.sup))
      else aux.quali.sup.indice <- (ncol(data)+ncol(data.group.sup)+1):(ncol(data)+ncol(data.group.sup)+ncol(aux.quali.sup))
      data.pca <- cbind.data.frame(data.pca,aux.quali.sup)
    }
#    res.globale <- PCA(data.pca, scale.unit = FALSE, col.w = ponderation, ncp = ncp.tmp, ind.sup = ind.sup, quali.sup = aux.quali.sup.indice, quanti.sup = data.group.sup.indice, graph = FALSE)
row.w = row.w[1:nb.actif]
    res.globale <- PCA(data.pca, scale.unit = FALSE, col.w = ponderation, row.w=row.w,ncp = ncp.tmp, ind.sup = ind.sup, quali.sup = aux.quali.sup.indice, quanti.sup = data.group.sup.indice, graph = FALSE)
    ncp <- min(ncp, nrow(res.globale$eig))
    call <- res.globale$call
    call$group <- group
    call$type <- type
    call$ncp <- ncp
    call$group.mod <- group.mod
    call$num.group.sup <- num.group.sup
    call$name.group <- name.group
    call$X <- base
    call$XTDC <- data
    contrib.group <- matrix(NA, length(group.actif), ncp.tmp)
    dimnames(contrib.group) <- list(name.group[group.actif], paste("Dim", c(1:ncp.tmp), sep = "."))
    dist2.group <- vector(length = length(group.actif))
    ind.var <- ind.var.sup <- 0
    for (g in 1:length(group.actif)) {
      if (group.mod[group.actif[g]]!=1) contrib.group[g, ] <- apply(res.globale$var$contrib[(ind.var + 1):(ind.var + group.mod[group.actif[g]]), ]/100, 2, sum)
      else contrib.group[g, ] <- res.globale$var$contrib[ind.var + 1, ]/100
      ind.var <- ind.var + group.mod[group.actif[g]]
      dist2.group[g] <- sum((res.separe[[group.actif[g]]]$eig[,1]/res.separe[[group.actif[g]]]$eig[1,1])^2)
    }
    coord.group <- sweep(contrib.group, 2, res.globale$eig[,1], "*")
    cos2.group <- sweep(coord.group^2, 1, dist2.group, "/")

    if (!is.null(num.group.sup)){
      coord.group.sup <- matrix(NA, length(num.group.sup), ncp.tmp)
      dimnames(coord.group.sup) <- list(name.group[num.group.sup], paste("Dim", c(1:ncp.tmp), sep = "."))
      if (is.null(ind.sup)) tab <- (cov.wt(cbind.data.frame(res.globale$ind$coord,data.group.sup),wt=row.w/sum(row.w),method="ML")$cov)^2
        else tab <- (cov.wt(cbind.data.frame(res.globale$ind$coord,data.group.sup[-ind.sup, ]),wt=row.w/sum(row.w),method="ML")$cov)^2
#      if (is.null(ind.sup)) tab <- (cov.wt(cbind.data.frame(res.globale$ind$coord,data.group.sup),wt=row.w,method="ML")$cov)^2
#        else tab <- (cov.wt(cbind.data.frame(res.globale$ind$coord,data.group.sup[-ind.sup, ]),wt=row.w,method="ML")$cov)^2
#tab <- cov(cbind.data.frame(res.globale$ind$coord,data.group.sup))^2*((nb.actif-1)/nb.actif)^2
#      else tab <- cov(cbind.data.frame(res.globale$ind$coord,data.group.sup[-ind.sup,]))^2*((nb.actif-1)/nb.actif)^2
      tab <- sweep(tab, 2, c(1/res.globale$eig[,1],ponderation.group.sup), "*")
      tab <- sweep(tab, 1, c(1/res.globale$eig[,1],ponderation.group.sup), "*")
      ind.gc <- 0
      for (gc in 1:length(num.group.sup)) {
        if (group.mod[num.group.sup[gc]]!=1) coord.group.sup[gc,] <- apply(tab[1:ncp.tmp,  (ncp.tmp+ind.gc+1):(ncp.tmp+ind.gc+group.mod[num.group.sup[gc]])],1,sum)
        else coord.group.sup[gc,] <- tab[1:ncp.tmp,  ncp.tmp+ind.gc+1]
        ind.gc <- ind.gc + group.mod[num.group.sup[gc]]
      }
    }
    if (is.null(num.group.sup)){
      if (is.null(ind.sup)) tab <- (cov.wt(data,wt=row.w/sum(row.w),method="ML")$cov)^2
        else tab <- (cov.wt(data[-ind.sup, ],wt=row.w/sum(row.w),method="ML")$cov)^2
#      if (is.null(ind.sup)) tab <- (cov.wt(data,wt=row.w,method="ML")$cov)^2
#        else tab <- (cov.wt(data[-ind.sup, ],wt=row.w,method="ML")$cov)^2
#tab <- cov(data)^2*((nb.actif-1)/nb.actif)^2
#      else tab <- cov(data[-ind.sup,])^2*((nb.actif-1)/nb.actif)^2
      tab <- sweep(tab, 2, ponderation, "*")
      tab <- sweep(tab, 1, ponderation, "*")
    }
    else{
      if (is.null(ind.sup)) tab <- (cov.wt(cbind.data.frame(data, data.group.sup),wt=row.w/sum(row.w),method="ML")$cov)^2
        else tab <- (cov.wt(cbind.data.frame(data[-ind.sup, ], data.group.sup[-ind.sup,]),wt=row.w/sum(row.w),method="ML")$cov)^2
#      if (is.null(ind.sup)) tab <- (cov.wt(cbind.data.frame(data, data.group.sup),wt=row.w,method="ML")$cov)^2
#        else tab <- (cov.wt(cbind.data.frame(data[-ind.sup, ], data.group.sup[-ind.sup,]),wt=row.w,method="ML")$cov)^2
#tab <- cov(cbind.data.frame(data,data.group.sup))^2*((nb.actif-1)/nb.actif)^2
#      else tab <- cov(cbind.data.frame(data[-ind.sup,],data.group.sup[-ind.sup,]))^2*((nb.actif-1)/nb.actif)^2
      tab <- sweep(tab, 2, c(ponderation,ponderation.group.sup), "*")
      tab <- sweep(tab, 1, c(ponderation,ponderation.group.sup), "*")
    }
    Lg <- matrix(0, nbre.group+1, nbre.group+1)
    ind.gl <- 0
    for (gl in c(group.actif,num.group.sup)) {
        ind.gc <- 0
        for (gc in c(group.actif,num.group.sup)) {
            Lg[gl, gc] <- Lg[gc, gl] <- sum(tab[(ind.gl + 1):(ind.gl + group.mod[gl]),  (ind.gc + 1):(ind.gc + group.mod[gc])])
            ind.gc <- ind.gc + group.mod[gc]
        }
        Lg[nbre.group+1,gl] <- Lg[gl,nbre.group+1] <- sum(tab[(ind.gl + 1):(ind.gl + group.mod[gl]), 1:ncol(data)])/res.globale$eig[1,1]
        ind.gl <- ind.gl + group.mod[gl]
    }
    Lg[nbre.group+1,nbre.group+1] <- sum(tab[1:ncol(data),1:ncol(data)])/(res.globale$eig[1,1]^2)
    dist2.group <- diag(Lg)
    if (!is.null(num.group.sup)){
      dist2.group.sup <- dist2.group[num.group.sup]
      dist2.group <- dist2.group[-num.group.sup]
    }
    RV <- sweep(Lg, 2, sqrt(diag(Lg)), "/")
    RV <- sweep(RV, 1, sqrt(diag(Lg)), "/")
    rownames(Lg) <- colnames(Lg) <- rownames(RV) <- colnames(RV) <- c(name.group,"MFA")
    data.partiel <- vector(mode = "list", length = nbre.group)
    names(data.partiel) <- name.group
    ind.col <- 0
    for (g in 1:nbre.group) {
      if (g%in%group.actif){
        data.partiel[[g]] <- as.data.frame(matrix(res.globale$call$centre, nrow(data), ncol(data), byrow = TRUE, dimnames = dimnames(data)))
        data.partiel[[g]][, (ind.col + 1):(ind.col + group.mod[g])] <- data[, (ind.col + 1):(ind.col + group.mod[g])]
        ind.col <- ind.col + group.mod[g]
      }
    }
    res.ind.partiel <- vector(mode = "list", length = nbre.group)
    names(res.ind.partiel) <- name.group

    for (g in group.actif){
      Xis <- as.matrix(sweep(data.partiel[[g]], 2, res.globale$call$centre, FUN = "-"))
      Xis <- as.matrix(sweep(Xis, 2, res.globale$call$ecart.type, FUN = "/"))
##      coord.ind.sup <- length(group.actif) * as.matrix(Xis)%*%diag((res.globale$call$col.w))%*%res.globale$svd$V
      coord.ind.sup <- length(group.actif) * as.matrix(Xis)
      coord.ind.sup <- sweep(coord.ind.sup,2,res.globale$call$col.w,FUN="*")
      coord.ind.sup <- coord.ind.sup %*%res.globale$svd$V
      res.ind.partiel[[g]]$coord.sup <- coord.ind.sup
    }
    cor.grpe.fact <- as.data.frame(matrix(NA, length(group.actif), ncp.tmp))
    colnames(cor.grpe.fact) <- paste("Dim", c(1:ncp.tmp), sep = ".")
    rownames(cor.grpe.fact) <- name.group[group.actif]
    for (f in 1:ncp.tmp) {
        for (g in 1:length(group.actif)) cor.grpe.fact[g, f] <- cor(res.ind.partiel[[group.actif[g]]]$coord.sup[1:nb.actif, f], res.globale$ind$coord[, f])
    }
    It <- vector(length = ncp.tmp)
    for (g in group.actif)  It <- It + apply(res.ind.partiel[[g]]$coord.sup[1:nb.actif,]^2,2,sum)
    rap.inertie <- apply(res.globale$ind$coord^2,2,sum) * length(group.actif) / It 

    res.groupes <- list(Lg = Lg, RV = RV, coord = coord.group[, 1:ncp], contrib = contrib.group[, 1:ncp] * 100,  cos2 = cos2.group[, 1:ncp], dist2 = dist2.group[-length(dist2.group)], correlation = cor.grpe.fact[, 1:ncp])
    if (!is.null(num.group.sup)){
      res.groupes$coord.sup <- as.data.frame(coord.group.sup)[,1:ncp]
      res.groupes$contrib.sup <- sweep(as.data.frame(coord.group.sup)[,1:ncp], 2, res.globale$eig[1:ncp,1], "/")*100
      res.groupes$cos2.sup <- sweep(as.data.frame(coord.group.sup)[,1:ncp]^2, 1, dist2.group.sup, "/")
      res.groupes$dist2.sup <- dist2.group.sup
    }
    inertie.intra.ind.partiel <- as.data.frame(matrix(NA, (nb.actif * length(group.actif) ), ncp.tmp))
    nom.ligne <- NULL
    for (i in 1:nb.actif) nom.ligne <- c(nom.ligne, paste(rownames(base)[i], name.group[group.actif], sep = "."))
    tmp <- array(0,dim=c(dim(res.globale$ind$coord),length(group.actif)))
    for (g in 1:length(group.actif)) tmp[,,g] <- (res.ind.partiel[[group.actif[g]]]$coord.sup[1:nb.actif,]-res.globale$ind$coord)^2 / length(group.actif)
    variab.auxil <- apply(tmp,2,sum)
    tmp <- sweep(tmp,2,variab.auxil,FUN="/") * 100
    inertie.intra.ind <- apply(tmp,c(1,2),sum)
    for (i in 1:nb.actif) inertie.intra.ind.partiel[((i - 1) * length(group.actif)  + 1):(i * length(group.actif)), ] <- t(tmp[i,1:ncp.tmp,])
    rownames(inertie.intra.ind) <- rownames(res.globale$ind$coord)
    rownames(inertie.intra.ind.partiel) <- nom.ligne
    colnames(inertie.intra.ind) <- colnames(inertie.intra.ind.partiel) <- paste("Dim", c(1:ncp.tmp), sep = ".")
    tab.partial.axes <- as.data.frame(matrix(NA, nb.actif, ncp * nbre.group))
    rownames(tab.partial.axes) <- rownames(data)[1:nb.actif]
    nom.axes <- paste("Dim", c(1:ncp), sep = "")
    nom.col <- NULL
    debut <- 0
    for (g in 1:nbre.group) {
        nom.col <- c(nom.col, paste(nom.axes, name.group[g],sep="."))
        nbcol <- min(ncp, ncol(res.separe[[g]]$ind$coord))
        tab.partial.axes[, (debut + 1):(debut + nbcol)] <- res.separe[[g]]$ind$coord
        debut <- debut + ncp
    }
    colnames(tab.partial.axes) <- nom.col
    indice.col.NA <- which(!is.na(tab.partial.axes[1, ]))
    tab.partial.axes <- tab.partial.axes[, indice.col.NA]
    centre <- apply(tab.partial.axes, 2, moy.p, res.globale$call$row.w)
    tab.partial.axes <- as.matrix(sweep(tab.partial.axes, 2, centre, FUN = "-"))
    ecart.type <- apply(tab.partial.axes, 2, ec, res.globale$call$row.w)
    ecart.type[ecart.type <= 1e-08] <- 1
    tab.partial.axes <- as.matrix(sweep(tab.partial.axes, 2, ecart.type, FUN = "/"))
##    coord.res.partial.axes <- t(tab.partial.axes) %*% diag(res.globale$call$row.w) %*% res.globale$svd$U
    coord.res.partial.axes <- sweep( t(tab.partial.axes) ,2, res.globale$call$row.w,FUN="*")
    coord.res.partial.axes <- coord.res.partial.axes %*% res.globale$svd$U
    contrib.res.partial.axes <- sweep(coord.res.partial.axes^2,2,res.globale$eig[,1],FUN="/") *100
    sigma <- apply(tab.partial.axes, 2, ec, res.globale$call$row.w)
    cor.res.partial.axes <- sweep(coord.res.partial.axes,1,sigma,FUN="/")
    colnames(coord.res.partial.axes) <- paste("Dim", c(1:ncol(coord.res.partial.axes)), sep = ".")
    dimnames(contrib.res.partial.axes) <- dimnames(cor.res.partial.axes) <- dimnames (coord.res.partial.axes)
    summary.n <- as.data.frame(matrix(NA, 0, 4))
    colnames(summary.n) <- c("group", "variable", "modalite", "effectif")
    summary.c <- as.data.frame(matrix(NA, 0, 6))
    colnames(summary.c) <- c("group", "variable", "moyenne", "ecart.type", "minimum", "maximum")
    for (g in 1:nbre.group) {
        if (type[g] == "c") {
            statg <- as.data.frame(matrix(NA, ncol(res.separe[[g]]$call$X), 6))
            colnames(statg) <- c("group", "variable", "moyenne", "ecart.type", "minimum", "maximum")
            statg[, "group"] <- rep(g, nrow(statg))
            statg[, "variable"] <- colnames(res.separe[[g]]$call$X)
            statg[, "moyenne"] <- res.separe[[g]]$call$centre
            if (!is.null(res.separe[[g]]$call$ecart.type)) statg[, "ecart.type"] <- res.separe[[g]]$call$ecart.type
            statg[, "minimum"] <- apply(res.separe[[g]]$call$X, 2, min)
            statg[, "maximum"] <- apply(res.separe[[g]]$call$X, 2, max)
            if (!is.null(res.separe[[g]]$call$ecart.type)) statg[, -c(1, 2)] <- round(statg[, -c(1, 2)], digits = 2)
            else statg[, -c(1, 2,4)] <- round(statg[, -c(1, 2,4)], digits = 2)
            summary.c <- rbind(summary.c, statg)
        }
        else {
            statg <- as.data.frame(matrix(NA, length(res.separe[[g]]$call$marge.col), 4))
            colnames(statg) <- c("group", "variable", "modalite", "effectif")
            statg[, "group"] <- rep(g, nrow(statg))
            res.separe[[g]]$call$X <- as.data.frame(res.separe[[g]]$call$X)
            nb.var <- ncol(res.separe[[g]]$call$X)
            nb.mod <- NULL
            nom.mod <- NULL
            for (v in 1:nb.var) {
                nb.mod <- c(nb.mod, nlevels(res.separe[[g]]$call$X[, v]))
                nom.mod <- c(nom.mod, levels(res.separe[[g]]$call$X[, v]))
            }
            statg[, "variable"] <- rep(colnames(res.separe[[g]]$call$X), nb.mod)
            statg[, "modalite"] <- nom.mod
            statg[, "effectif"] <- res.separe[[g]]$call$marge.col * nbre.ind * nb.var
            summary.n <- rbind(summary.n, statg)
        }
    }
    eig <- res.globale$eig
    nom.ligne <- NULL
    for (i in 1:nbre.ind) {
        ind.tmp <- rownames(base)[i]
        nom.ligne <- c(nom.ligne, paste(ind.tmp, name.group[group.actif], sep = "."))
    }
    coord.ind.partiel <- as.data.frame(matrix(NA, (nbre.ind * length(group.actif)), ncp))
    rownames(coord.ind.partiel) <- nom.ligne
    colnames(coord.ind.partiel) <- paste("Dim", c(1:ncp), sep = ".")
    coord.ind <- rbind(res.globale$ind$coord[, 1:ncp],res.globale$ind.sup$coord[, 1:ncp])
    cos2.ind <- rbind(res.globale$ind$cos2[, 1:ncp],res.globale$ind.sup$cos2[, 1:ncp])
    contrib.ind <- res.globale$ind$contrib[, 1:ncp]
    liste.ligne <- seq(1, nbre.ind * length(group.actif), by = length(group.actif))
    for (g in 1:length(group.actif)) coord.ind.partiel[liste.ligne+g-1, ] <- res.ind.partiel[[group.actif[g]]]$coord.sup[, 1:ncp]
    if (!is.null(ind.sup)) {
      res.ind.sup <- list(coord = coord.ind[(nb.actif+1):nrow(coord.ind),], cos2 = cos2.ind[(nb.actif+1):nrow(coord.ind),], coord.partiel = coord.ind.partiel[(length(group.actif)*nb.actif+1):nrow(coord.ind.partiel),])
      res.ind <- list(coord = coord.ind[1:nb.actif,], contrib = contrib.ind, cos2 = cos2.ind[1:nb.actif,], within.inertia = inertie.intra.ind[1:nb.actif,1:ncp], coord.partiel = coord.ind.partiel[1:(length(group.actif)*nb.actif),], within.partial.inertia = inertie.intra.ind.partiel[1:(length(group.actif)*nb.actif),1:ncp] )
    }
    else res.ind <- list(coord = coord.ind, contrib = contrib.ind, cos2 = cos2.ind, within.inertia = inertie.intra.ind[,1:ncp], coord.partiel = coord.ind.partiel, within.partial.inertia = inertie.intra.ind.partiel[,1:ncp])

    res.quali.var <- res.quali.var.sup <- NULL
    bool.act <- FALSE
    bool.sup <- FALSE
    if (!is.null(ind.quali)) {
      coord.quali <- res.globale$quali.sup$coord[, 1:ncp,drop=FALSE]
      cos2.quali <- res.globale$quali.sup$cos2[, 1:ncp,drop=FALSE]
      val.test.quali <- res.globale$quali.sup$v.test[, 1:ncp,drop=FALSE]
      contrib.quali <- sweep(res.globale$quali.sup$coord^2, 2, res.globale$eig[,1], "/")
      poids.bary <- res.globale$call$quali.sup$nombre * res.globale$call$row.w[1]
      contrib.quali <- 100 * sweep(contrib.quali, 1, poids.bary, "*")[,1:ncp,drop=FALSE]
      barycentre <- res.globale$call$quali.sup$barycentre
      coord.quali.partiel <- as.data.frame(matrix(NA, (nrow(barycentre) * length(group.actif)), ncp))
      nom.ligne.bary <- NULL
      for (q in 1:nrow(barycentre)) {
        ind.tmp <- rownames(barycentre)[q]
        nom.ligne.bary <- c(nom.ligne.bary, paste(ind.tmp, name.group[group.actif], sep = "."))
      }
      rownames(coord.quali.partiel) <- nom.ligne.bary
      liste.ligne <- seq(1, (nrow(barycentre) * length(group.actif) ), by = length(group.actif))
      inertie.intra.cg.partiel <- as.data.frame(matrix(NA, (nrow(barycentre) * length(group.actif) ), ncp.tmp))
      tmp <- array(0,dim=c(dim(res.globale$quali.sup$coord),length(group.actif)))
      ind.col <- 0
      for (g in 1:length(group.actif)) {
        cg.partiel <- as.data.frame(matrix(res.globale$call$centre, nrow(barycentre), ncol(barycentre), byrow = TRUE, dimnames = dimnames(barycentre)))
        cg.partiel[, (ind.col + 1):(ind.col + group.mod[group.actif[g]])] <- barycentre[, (ind.col + 1):(ind.col + group.mod[group.actif[g]])]
        ind.col <- ind.col + group.mod[group.actif[g]]
        Xis <- as.matrix(sweep(cg.partiel, 2, res.globale$call$centre, FUN = "-"))
        Xis <- as.matrix(sweep(Xis, 2, res.globale$call$ecart.type, FUN = "/"))
##        coord.quali.sup <- length(group.actif) * as.matrix(Xis)%*%diag((res.globale$call$col.w))%*%res.globale$svd$V
        coord.quali.sup <- length(group.actif) * as.matrix(Xis)
        coord.quali.sup <- sweep(coord.quali.sup ,2,res.globale$call$col.w,FUN="*")
        coord.quali.sup <- coord.quali.sup %*%res.globale$svd$V
        coord.quali.partiel[liste.ligne + g - 1, ] <- coord.quali.sup[,1:ncp]
        tmp[,,g] <- (coord.quali.sup - res.globale$quali.sup$coord)^2 / length(group.actif) 
      }
      colnames(coord.quali.partiel) <-  paste("Dim", 1:ncp, sep = ".")
      tmp <- sweep(tmp,2,variab.auxil,FUN="/") * 100
      tmp <- sweep(tmp,1,res.globale$call$quali.sup$nombre,FUN="*")
      inertie.intra.cg <- apply(tmp,c(1,2),sum)
      for (i in 1:nrow(barycentre)) inertie.intra.cg.partiel[((i - 1) * length(group.actif)  + 1):(i * length(group.actif)), ] <- t(tmp[i,1:ncp.tmp,])
      rownames(inertie.intra.cg) <- rownames(res.globale$quali.sup$coord)
      rownames(inertie.intra.cg.partiel) <- nom.ligne.bary
      colnames(inertie.intra.cg) <- colnames(inertie.intra.cg.partiel) <- paste("Dim", c(1:ncp.tmp), sep = ".")

      ind.col <- 0
      ind.col.act <- NULL
      ind.col.sup <- NULL
      for (g in 1:nbre.group) {
        if (type[g] =="n"){
          if (g%in%num.group.sup) ind.col.sup <- c(ind.col.sup, (ind.col+1):(ind.col+group.mod[g]))
          else ind.col.act <- c(ind.col.act, (ind.col+1):(ind.col+group.mod[g]))
          ind.col = ind.col + group.mod[g]
        }
      }
      for (g in 1:nbre.group) {
        if (type[g] =="n"){
          if (g%in%num.group.sup) {
            coord.quali.sup <- coord.quali[ind.col.sup,,drop=FALSE]
            contrib.quali.sup <- contrib.quali[ind.col.sup,,drop=FALSE]
            cos2.quali.sup <- cos2.quali[ind.col.sup,,drop=FALSE]
            val.test.quali.sup <- val.test.quali[ind.col.sup,,drop=FALSE]
            coord.quali.partiel.sup <- coord.quali.partiel[unlist(lapply(ind.col.sup, function(k) seq(length(group.actif)*(k-1)+1,length=length(group.actif)))),]
            inertie.intra.cg.sup <- inertie.intra.cg[ind.col.sup,1:ncp]
            inertie.intra.cg.partiel.sup <- inertie.intra.cg.partiel[unlist(lapply(ind.col.sup, function(k) seq(length(group.actif)*(k-1)+1,length=length(group.actif)))),1:ncp]
            bool.sup <- TRUE
          }
          else {
            coord.quali.act <- coord.quali[ind.col.act,,drop=FALSE]
            contrib.quali.act <- contrib.quali[ind.col.act,,drop=FALSE]
            cos2.quali.act <- cos2.quali[ind.col.act,,drop=FALSE]
            val.test.quali.act <- val.test.quali[ind.col.act,,drop=FALSE]
            coord.quali.partiel.act <- coord.quali.partiel[unlist(lapply(ind.col.act, function(k) seq(length(group.actif)*(k-1)+1,length=length(group.actif)))),]
            inertie.intra.cg.act <- inertie.intra.cg[ind.col.act,1:ncp]
            inertie.intra.cg.partiel.act <- inertie.intra.cg.partiel[unlist(lapply(ind.col.act, function(k) seq(length(group.actif)*(k-1)+1,length=length(group.actif)))),1:ncp]
            bool.act <- TRUE
          }
        }
      }
      if (bool.act) res.quali.var <- list(coord = coord.quali.act, contrib = contrib.quali.act, cos2 = cos2.quali.act, v.test = val.test.quali.act, coord.partiel = coord.quali.partiel.act, within.inertia = inertie.intra.cg.act, within.partial.inertia = inertie.intra.cg.partiel.act)
      if (bool.sup) res.quali.var.sup <- list(coord = coord.quali.sup, contrib = contrib.quali.sup, cos2 = cos2.quali.sup, v.test = val.test.quali.sup, coord.partiel = coord.quali.partiel.sup, within.inertia = inertie.intra.cg.sup, within.partial.inertia = inertie.intra.cg.partiel.sup)
    }
    indice.quanti <- NULL
    num.tmp <- 0
    for (g in group.actif) {
        if (type[g] == "c")  indice.quanti <- c(indice.quanti, c((num.tmp + 1):(num.tmp + group.mod[g])))
        num.tmp <- num.tmp + group.mod[g]
    }
    res.quanti.var <- NULL
    if (!is.null(indice.quanti)){
      coord.quanti.var <- res.globale$var$coord[indice.quanti,1:ncp,drop=FALSE]
      coord.quanti.var <- as.data.frame(res.globale$var$coord[indice.quanti,1:ncp,drop=FALSE])
      cos2.quanti.var <- res.globale$var$cos2[indice.quanti, 1:ncp,drop=FALSE]
      contrib.quanti.var <- res.globale$var$contrib[indice.quanti, 1:ncp,drop=FALSE]
      cor.quanti.var <- res.globale$var$cor[indice.quanti, 1:ncp,drop=FALSE]
      res.quanti.var <- list(coord = coord.quanti.var, contrib = contrib.quanti.var, cos2 = cos2.quanti.var, cor = cor.quanti.var)
    }

    res.quanti.var.sup <- NULL
    if (!is.null(num.group.sup)){
      num.tmp <- 0
      indice.quanti <- NULL
      eig.aux <- NULL
      for (g in num.group.sup) {
        if (type[g] == "c") {
          indice.quanti <- c(indice.quanti, c((num.tmp + 1):(num.tmp + group.mod[g])))
          eig.aux <- c(eig.aux, rep(res.separe[[g]]$eig[1,1],group.mod[g]*ncp))
        }
        num.tmp <- num.tmp + group.mod[g]
      }
      if (!is.null(indice.quanti)){
        coord.quanti.var.sup <- res.globale$quanti.sup$coord[indice.quanti,1:ncp,drop=FALSE]
        cos2.quanti.var.sup <- res.globale$quanti.sup$cos2[indice.quanti, 1:ncp,drop=FALSE]
#        contrib.quanti.var.sup <- res.globale$quanti.sup$contrib[indice.quanti, 1:ncp]
#        contrib.quanti.var.sup <- sweep(as.data.frame(contrib.quanti.var.sup)[,1:ncp], 2, eig.aux, "/")
        cor.quanti.var.sup <- res.globale$quanti.sup$cor[indice.quanti, 1:ncp,drop=FALSE]
#        res.quanti.var.sup <- list(coord = coord.quanti.var.sup, contrib = contrib.quanti.var.sup, cos2 = cos2.quanti.var.sup, cor = cor.quanti.var.sup)
        res.quanti.var.sup <- list(coord = coord.quanti.var.sup, cos2 = cos2.quanti.var.sup, cor = cor.quanti.var.sup)
      }
    }

    aux <- res.separe[[1]]$ind$coord
    name.aux <- paste(colnames(res.separe[[1]]$ind$coord),name.group[1],sep=".")
    for (g in 2:nbre.group) {
      aux <- cbind(aux,res.separe[[g]]$ind$coord)
      name.aux = c(name.aux,paste(colnames(res.separe[[g]]$ind$coord),name.group[g],sep="."))
    }
    cor.partial.axes <- cor(aux)
    dimnames(cor.partial.axes) <- list(name.aux,name.aux)
    res.partial.axes <- list(coord = coord.res.partial.axes[, 1:ncp], cor = cor.res.partial.axes[, 1:ncp], contrib = contrib.res.partial.axes[, 1:ncp], cor.between = cor.partial.axes)
    resultats <- list(separate.analyses = res.separe, eig = eig, group = res.groupes, 
        inertia.ratio = rap.inertie[1:ncp], ind = res.ind)
    if (!is.null(ind.sup)) resultats$ind.sup <- res.ind.sup
    if (!is.null(c(res.quanti.var,res.quanti.var.sup))) resultats$summary.quanti = summary.c
    if (!is.null(c(bool.act,bool.sup))) resultats$summary.quali = summary.n
    if (!is.null(res.quanti.var)) resultats$quanti.var = res.quanti.var
    if (!is.null(res.quanti.var.sup)) resultats$quanti.var.sup = res.quanti.var.sup
    if (bool.act) resultats$quali.var = res.quali.var
    if (bool.sup) resultats$quali.var.sup = res.quali.var.sup
    resultats$partial.axes = res.partial.axes
    resultats$call = call
    resultats$global.pca = res.globale
    class(resultats) <- c("MFA", "list")

    if (graph){
      if (bool.act | bool.sup){
        cg.plot.partial <- NULL
        if (!is.null(resultats["quali.var"]$quali.var)){
          max.inertia <- order(apply(resultats$quali.var$within.inertia[,1:2],1,sum))
          cg.plot.partial <- rownames(resultats$quali.var$coord)[max.inertia[1:length(max.inertia)]]
        }
        if (!is.null(resultats$quali.var.sup)){
          max.inertia <- order(apply(resultats$quali.var.sup$within.inertia[,1:2],1,sum))
          cg.plot.partial <- c(cg.plot.partial,rownames(resultats$quali.var.sup$coord)[max.inertia[1:length(max.inertia)]])
        }
        plot.MFA(resultats,choix="ind",invisible="ind",partial=cg.plot.partial,habillage="group",axes=axes)
      }
      max.inertia <- order(apply(resultats$ind$within.inertia[,1:2],1,sum))
      plot.MFA(resultats,choix="ind",invisible="quali",partial=rownames(resultats$ind$coord)[max.inertia[c(1:2,nrow(resultats$ind$coord)-1,nrow(resultats$ind$coord))]],habillage="group",axes=axes)
      if (!is.null(c(res.quanti.var,res.quanti.var.sup))) plot.MFA(resultats,choix="var",habillage="group",axes=axes)
      plot.MFA(resultats,choix="ind",invisible="quali",habillage = "none",axes=axes)
      plot.MFA(resultats,choix="axes",habillage="group",axes=axes)
      plot.MFA(resultats,choix="group",axes=axes)
    }
    return(resultats)
}
