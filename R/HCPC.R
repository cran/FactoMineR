HCPC <- function (res, nb.clust = 0, consol = TRUE, iter.max = 10, min = 3, 
    max = NULL, metric = "euclidean", method = "ward", order = TRUE, 
    graph.scale = "inertia", nb.par = 5, graph = TRUE, proba = 0.05,cluster.CA="rows", 
    ...) 
{
    auto.cut.tree = function(res, min, max, metric, method, ...) {
        if (order) {
            sss = cbind.data.frame(res$ind$coord, res$call$X, res$call$row.w)
            sss = sss[order(sss[, 1], decreasing = FALSE), ]
            res$ind$coord = sss[, 1:ncol(res$ind$coord)]
            res$call$X = sss[, (ncol(res$ind$coord) + 1):(ncol(sss)-1)]
            res$call$row.w = sss[,ncol(sss)]
        }
        X = as.data.frame(res$ind$coord)
        intra = NULL
        inert.gain = NULL
        ag = agnes(X, diss = FALSE, metric = metric, method = method, 
            stand = FALSE, ...)
        hc = as.hclust(ag)
        i = sum(scale(X, scale = FALSE)^2)/nrow(X)
        intra[1] = i
        for (j in 1:(nrow(X) - 1)) {
            inert.gain[j] = hc$height[nrow(X) - j]^2/(2 * nrow(X))
            intra[j + 1] = intra[j] - inert.gain[j]
        }
        quot = intra[min:(max)]/intra[(min - 1):(max - 1)]
        nb.clust = which.min(quot) + min - 1
        return(list(res = res, tree = hc, nb.clust = nb.clust, 
            within = intra, inert.gain = inert.gain, quot = quot, 
            i = i))
    }
    consolidation = function(X, clust, iter.max = 10, ...) {
        centers = NULL
        centers = by(X, clust, mean)
        centers = matrix(unlist(centers), ncol = ncol(X), byrow = TRUE)
        km = kmeans(X, centers = centers, iter.max = iter.max, 
            ...)
        return(km)
    }
    coord.construction = function(coord.centers, coord.ind, clust) {
        coord.centers = as.data.frame(coord.centers)
        for (i in 1:nrow(coord.centers)) rownames(coord.centers)[i] = paste("center", 
            i)
        coord.ind = cbind(coord.ind, clust)
        return(list(coord.ind = coord.ind, coord.centers = coord.centers))
    }
    select = function(Y, default.size, method, coord.centers) {
        clust = Y[1, ncol(Y)]
        Y = Y[, -ncol(Y)]
        Z = rbind(Y, coord.centers)
        if (nrow(Y) == 1) {
            distance = data.frame(0, row.names = "")
            colnames(distance) = rownames(Z[1, ])
        }
        else {
            distance = as.matrix(dist(Z, method = method))
            distance = distance[(nrow(Y) + 1):nrow(distance), 
                -((nrow(Y) + 1):ncol(distance))]
            distance = sort(distance[clust, ], decreasing = FALSE)
        }
        if (length(distance) > default.size) 
            distance = distance[1:default.size]
        else distance = distance
    }
    distinctivness = function(Y, default.size, method, coord.centers) {
        clust = as.numeric(Y[1, ncol(Y)])
        Y = Y[, -ncol(Y)]
        Z = rbind(Y, coord.centers)
        if (nrow(Y) == 1) {
            distance = as.matrix(dist(Z, method = method))
            ind.car = vector(length = 1, mode = "numeric")
            ind.car = min(distance[-c(1, (clust + 1)), 1])
            names(ind.car) = rownames(Z[1, ])
        }
        else {
            distance = as.matrix(dist(Z, method = method))
            distance = distance[(nrow(Y) + 1):nrow(distance), 
                -((nrow(Y) + 1):ncol(distance))]
            if (nrow(distance) == 2) 
                center.min = distance[-clust, ]
            else center.min = apply(distance[-clust, ], 2, min)
            ind.car = sort(center.min, decreasing = TRUE)
        }
        if (length(ind.car) > default.size) 
            ind.car = ind.car[1:default.size]
        else ind.car = ind.car
    }
    if (is.vector(res)) {
        res = cbind.data.frame(res, res)
        res = PCA(res, scale.unit = FALSE, ncp = Inf, graph = FALSE)
        vec = TRUE
    }
    else vec = FALSE
    if(inherits(res,"CA")){
	  if(cluster.CA=="rows") res=as.data.frame(res$row$coord)
	  if(cluster.CA=="columns") res=as.data.frame(res$col$coord)
    }
    if (is.data.frame(res)){
	  num=c()
	  for (i in 1:ncol(res)) if(is.numeric(res[,i])) num=c(num,i)
	  res=res[,num]
        res = PCA(res, scale.unit = FALSE, ncp = Inf, graph = FALSE)
    }
    if (is.null(max)) 
        max = min(10, round(nrow(res$ind$coord)/2))
    max = min(max, nrow(res$ind$coord) - 1)
    if (inherits(res, "PCA") | inherits(res, "MCA") | inherits(res, 
        "MFA") | inherits(res, "HMFA")) {
        if (!is.null(res$call$ind.sup)) 
            res$call$X = res$call$X[-res$call$ind.sup, ]
        t = auto.cut.tree(res, min = min, max = max, metric = metric, 
            method = method, ...)
    }
    else stop("res should be from PCA, MCA, or MFA class")
    if (inherits(t$tree, "agnes")) 
        t$tree <- as.hclust(t$tree)
    if (inherits(t$tree, "hclust")) {
        if (graph.scale == "inertia") {
            nb.ind = nrow(t$res$ind$coord)
            inertia.height = rep(0, nb.ind - 1)
            for (i in 1:(nb.ind - 1)) inertia.height[i] = t$inert.gain[(nb.ind - 
                i)]
            inertia.height = sort(inertia.height, decreasing = FALSE)
            t$tree$height = inertia.height
        }
        auto.haut = ((t$tree$height[length(t$tree$height) - t$nb.clust + 
            2]) + (t$tree$height[length(t$tree$height) - t$nb.clust + 
            1]))/2
        if (graph) {
            dev.new()
            par(mar = c(0.5, 2, 0.75, 0))
            lay = matrix(ncol = 5, nrow = 5, c(2, 4, 4, 4, 4, 
                2, 4, 4, 4, 4, 2, 4, 4, 4, 4, 2, 4, 4, 4, 4, 
                1, 3, 3, 3, 3))
            layout(lay, respect = TRUE)
            layout.show(n = 4)
            barplot(t$inert.gain[1:max(15, max)], col = c(rep("black", 
                t$nb.clust - 1), rep("grey", max(max, 15) - t$nb.clust + 
                1)), rep(0.1, max(max, 15)), space = 0.9)
            plot(x = 1, xlab = "", ylab = "", main = "", col = "white", 
                axes = FALSE)
            text(1, 1, "Hierarchical Clustering", cex = 2)
            plot(x = 1, xlab = "", ylab = "", main = "", col = "white", 
                axes = FALSE)
            legend("top", "inertia gain  ", box.lty = NULL, cex = 1)
        }
        else {
            if (nb.clust == 0 | nb.clust == 1) 
                nb.clust = -1
        }
        if ((nb.clust == 0) | (nb.clust == 1)) {
            plot(t$tree, hang = -1, main = "Click to cut the tree", 
                xlab = "", sub = "")
            abline(h = auto.haut, col = "black", lwd = 3)
            coupe = locator(n = 1)
            while (coupe$y < min(t$tree$height)) {
                cat("No class \n")
                coupe = locator(n = 1)
            }
            y = coupe$y
        }
        else {
            if (graph) 
                plot(t$tree, hang = -1, main = "Hierarchical Classification", 
                  xlab = "", sub = "")
            if (nb.clust < 0) 
                y = auto.haut
            else y = (t$tree$height[length(t$tree$height) - nb.clust + 
                2] + t$tree$height[length(t$tree$height) - nb.clust + 
                1])/2
        }
    }
    else stop("The tree should be from 'hclust' or 'agnes' class.")
    clust = cutree(as.hclust(t$tree), h = y)
    nb.clust = max(clust)
    X = as.data.frame(t$res$ind$coord)
    if (graph) {
        rect = rect.hclust(t$tree, h = y, border = seq(1, nb.clust, 
            1))
        clust = NULL
        for (j in 1:nb.clust) clust = c(clust, rep(j, length(rect[[j]])))
        clust = as.factor(clust)
        belong = cbind.data.frame(t$tree$order, clust)
        belong = belong[do.call("order", belong), ]
        clust = belong$clust
        clust = as.factor(clust)
    }
    if (consol) {
        res.consol = consolidation(X, clust = clust, iter.max = iter.max)
        clust = res.consol$cluster
        coordon = coord.construction(res.consol$centers, X, clust = clust)
    }
    if (!consol) {
        list.centers = by(X, clust, mean)
        centers = matrix(unlist(list.centers), ncol = ncol(X), 
            byrow = TRUE)
        colnames(centers) = colnames(X)
        coordon = coord.construction(centers, X, clust)
    }
    cluster = coordon$coord.ind$clust
    para = by(coordon$coord.ind, cluster, simplify = FALSE, select, 
        default.size = nb.par, method = metric, coord.centers = coordon$coord.centers)
    dist = by(coordon$coord.ind, cluster, simplify = FALSE, distinctivness, 
        default.size = nb.par, method = metric, coord.centers = coordon$coord.centers)
    desc.ind = list(para = para, dist = dist)
    clust = as.factor(clust)
    X = cbind.data.frame(X, clust)
    data.clust = cbind.data.frame(t$res$call$X, clust)
    if (vec) 
        data.clust = as.data.frame(data.clust[, -2])
    desc.var = catdes(data.clust, ncol(data.clust), proba = proba)
    desc.axe = catdes(X, ncol(X), proba = proba)
    call = list(t = t, min = min, max = max, X = X, vec = vec)
    res.HCPC = list(data.clust = data.clust, desc.var = desc.var, 
        desc.axes = desc.axe, call = call, desc.ind = desc.ind)
    if (graph) {
        if (vec) 
            plot.HCPC(res.HCPC, choice = "3D.map", t.level = "all", 
                angle = 0, ind.names = FALSE)
        else {
            plot.HCPC(res.HCPC, choice = "3D.map", t.level = "all", 
                ind.names = TRUE)
            plot.HCPC(res.HCPC, choice = "map", draw.tree = FALSE, 
                label = "ind")
        }
    }
    class(res.HCPC) = "HCPC"
    return(res.HCPC)
}