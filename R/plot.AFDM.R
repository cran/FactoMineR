plot.AFDM <- function (x, choix = "group", axes = c(1, 2), lab.grpe = TRUE,
    lab.var = TRUE, lab.ind = TRUE, habillage = "none", col.lab = FALSE,
    col.hab = NULL, invisible = NULL, lim.cos2.var = 0.1, xlim = NULL,
    ylim = NULL, ...) {
    res.afdm <- x
    if (!inherits(res.afdm, "AFDM")) stop("non convenient data")
    contribA1 <- round((res.afdm$eig[axes[1]]/sum(res.afdm$eig)) * 100, digits = 2)
    contribA2 <- round((res.afdm$eig[axes[2]]/sum(res.afdm$eig)) * 100, digits = 2)
    sous.titre <- paste("plan factoriel", axes[1], "-", axes[2], ":", contribA1, "+", contribA2, "=", sum(contribA1, contribA2), "%")
    groupe <- res.afdm$call$groupe
    nbre.grpe <- length(groupe)
    type <- res.afdm$call$type
    if (choix == "group") {
        coord.actif <- res.afdm$group$coord[, axes]
        plot(coord.actif, xlab = "", ylab = "", xlim = c(0, 1),
            ylim = c(0, 1), pch = 17, col = "darkred", cex = 1.4,
            main = "Représentation simultannée des variables qualitatives et quantitatives",
            cex.main = 1, sub = sous.titre, asp=1)
        if (lab.grpe) {
            if (col.lab) {
                text(coord.actif[, 1], y = coord.actif[, 2], labels = rownames(coord.actif), pos = 3, col = "darkred")
            }
            else {
                text(coord.actif[, 1], y = coord.actif[, 2], labels = rownames(coord.actif), pos = 3)
            }
        }
    }
    if (choix == "var") {
        coord.var <- res.afdm$quanti.var$coord[, axes]
        col <- collab <- NULL
        X11(width = 8, height = 8)
        par(mar = c(2, 2, 2, 2))
        plot(0, 0, main = "Représentation des variables quantitatives",
            cex.main = 1, xlab = "",
            ylab = "", xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2),
            col = "white", bty = "n", asp=1)
        x.cercle <- seq(-1, 1, by = 0.01)
        y.cercle <- sqrt(1 - x.cercle^2)
        lines(x.cercle, y = y.cercle)
        lines(x.cercle, y = -y.cercle)
        abline(h=0,lty=2)
        abline(v=0,lty=2)
        title(sub = sous.titre, line = 0)
        for (v in 1:nrow(coord.var)) {
            if (sum(res.afdm$quanti.var$cos2[v,
                axes], na.rm = TRUE) >= lim.cos2.var && !is.na(sum(res.afdm$quanti.var$cos2[v,
                axes], na.rm = TRUE))) {
                arrows(0, 0, coord.var[v, 1], coord.var[v, 2],
                  length = 0.1, angle = 15, code = 2, col = col[v])
                if (lab.var) {
                  if (coord.var[v, 2] >= 0)
                    pos <- 3
                  else pos <- 1
                  text(coord.var[v, 1], y = coord.var[v, 2],
                    labels = rownames(coord.var)[v], pos = pos,
                    col = collab[v])
                }
            }
        }
        par(mar = c(5, 4, 4, 2) + 0.1)
    }
    if (choix == "ind") {
        nbre.ind.actif <- nrow(res.afdm$call$X)
        indice.ind.actif <- seq(1, ((nbre.grpe + 1) * nbre.ind.actif),
            by = (nbre.grpe + 1))
        coord.ind <- res.afdm$ind$coord[, axes]
        coord.quali <- res.afdm$quali.var$coord[, axes]
        if (is.null(xlim)) {
            xmin <- min(coord.ind[, 1], coord.quali[, 1])
            xmax <- max(coord.ind[, 1], coord.quali[, 1])
            xlim <- c(xmin, xmax) * 1.2
        }
        if (is.null(ylim)) {
            ymin <- min(coord.ind[, 2], coord.quali[, 2])
            ymax <- max(coord.ind[, 2], coord.quali[, 2])
            ylim <- c(ymin, ymax) * 1.2
        }
        if (habillage == "quali") {
            liste.quali <- colnames(res.afdm$call$X[type == "n"])
            if (length(liste.quali) >= 2) {
                texte <- liste.quali[1]
                for (i in 2:length(liste.quali)) texte <- paste(texte,
                  liste.quali[i], sep = ", ")
                cat(paste("\n", texte, "\n\nParmi la liste précédente quelle variable qualitative \ndésirez vous utilisez pour l'habillage de vos points",
                  sep = ""))
                nom.quali <- readLines(n = 1)
            }
            else {
                nom.quali <- liste.quali
            }
            modalite <- levels(as.factor(res.afdm$call$X[, nom.quali]))
            col.ind <- factor(as.numeric(as.factor(res.afdm$call$X[,
                nom.quali])))
            if (is.null(col.hab) | length(col.hab) != length(modalite)) {
                col.hab <- c(1:length(modalite))
            }
            levels(col.ind) <- col.hab
            col.ind <- as.character(col.ind)
            indice.inf <- sum(res.afdm$call$groupe.mod[type ==
                "n"][0:(match(nom.quali, liste.quali) - 1)]) +
                1
            indice.sup <- indice.inf + length(modalite) - 1
            col.quali <- rep(1, sum(res.afdm$call$groupe.mod[type ==
                "n"]))
            col.quali[indice.inf:indice.sup] <- col.hab
        }
        if (habillage == "none") {
            col.ind <- "black"
            col.quali <- "darkred"
        }
        test.invisible <- vector(length = 2)
        if (!is.null(invisible)) {
            test.invisible[1] <- match("ind", invisible)
            test.invisible[2] <- match("quali", invisible)
        }
        else {
            test.invisible <- rep(NA, 2)
        }
        plot(0, 0, main = "Représentation des individus", cex.main = 1,
            xlab = "", ylab = "", xlim = xlim,
            ylim = ylim, col = "white",asp=1)
        abline(v=0,lty=2)
        abline(h=0,lty=2)
        if (is.na(test.invisible[1])) {
            points(coord.ind, pch = 16, col = col.ind)
            if (lab.ind) {
                if (col.lab) {
                  text(coord.ind[, 1], y = coord.ind[, 2], labels = rownames(coord.ind),
                    pos = 3, col = col.ind)
                }
                else {
                  text(coord.ind[, 1], y = coord.ind[, 2], labels = rownames(coord.ind),
                    pos = 3)
                }
            }
        }
        if (is.na(test.invisible[2])) {
            points(coord.quali, pch = 15, col = col.quali)
            if (lab.var) {
                if (col.lab) {
                  text(coord.quali[, 1], y = coord.quali[, 2],
                    labels = rownames(coord.quali), pos = 3,
                    col = col.quali)
                }
                else {
                  text(coord.quali[, 1], y = coord.quali[, 2],
                    labels = rownames(coord.quali), pos = 3)
                }
            }
        }
    }
}
