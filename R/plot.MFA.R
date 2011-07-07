plot.MFA=function (x, axes = c(1, 2), choix = "ind", ellipse = NULL, ellipse.par = NULL, 
    lab.grpe = TRUE, lab.var = TRUE, lab.ind = TRUE, lab.par = FALSE, lab.col = TRUE,
    habillage = "ind", col.hab = NULL, invisible = NULL, partial = NULL, 
    lim.cos2.var = 0., chrono = FALSE, xlim = NULL, ylim = NULL, 
    cex = 1, title = NULL, palette = NULL, new.plot = TRUE, ...) 
{
    res.mfa <- x
    if (!inherits(res.mfa, "MFA")) 
        stop("non convenient data")
    if (is.null(palette)) 
        palette(c("black", "red", "green3", "blue", "cyan", "magenta", 
            "darkgray", "darkgoldenrod", "darkgreen", "violet", 
            "turquoise", "orange", "lightpink", "lavender", "yellow", 
            "lightgreen", "lightgrey", "lightblue", "darkkhaki", 
            "darkmagenta", "darkolivegreen", "lightcyan", "darkorange", 
            "darkorchid", "darkred", "darksalmon", "darkseagreen", 
            "darkslateblue", "darkslategray", "darkslategrey", 
            "darkturquoise", "darkviolet", "lightgray", "lightsalmon", 
            "lightyellow", "maroon"))
    lab.x <- paste("Dim ", axes[1], " (", signif(res.mfa$eig[axes[1], 
        2], 4), " %)", sep = "")
    lab.y <- paste("Dim ", axes[2], " (", signif(res.mfa$eig[axes[2], 
        2], 4), " %)", sep = "")
    group <- res.mfa$call$group
    nbre.grpe <- length(group)
    type <- res.mfa$call$type
    num.group.sup = NULL
    if (!is.null(res.mfa$group$coord.sup)) {
        num.group.sup <- res.mfa$call$num.group.sup
        nbre.grpe.sup <- length(num.group.sup)
        type.sup <- res.mfa$call$type[num.group.sup]
        type.act <- type[-num.group.sup]
        nbre.grpe <- nbre.grpe - length(num.group.sup)
    }
    if (choix == "axes") {
        if (new.plot) 
            dev.new()
        if (is.null(title)) 
            title <- "Partial axes"
        coord.axes <- res.mfa$partial.axes$coord[, axes, drop = FALSE]
        plot(0, 0, xlab = lab.x, ylab = lab.y, xlim = c(-1.1, 
            1.1), ylim = c(-1.1, 1.1), col = "white", asp = 1, 
            cex = cex, main = title)
        x.cercle <- seq(-1, 1, by = 0.01)
        y.cercle <- sqrt(1 - x.cercle^2)
        lines(x.cercle, y = y.cercle)
        lines(x.cercle, y = -y.cercle)
        abline(v = 0, lty = 2, cex = cex)
        abline(h = 0, lty = 2, cex = cex)
        if (habillage == "group") {
            if (is.null(col.hab) | length(col.hab) < length(group)) {
                col.hab <- 2:(length(group) + 1)
            }
            i = 1
            couleur.axes <- col.hab[i]
            auxil = strsplit(rownames(res.mfa$partial.axes$coord)[1], 
                ".", fixed = TRUE)[[1]]
            auxil2 = auxil[length(auxil)]
            for (j in 2:nrow(res.mfa$partial.axes$coord)) {
                auxil = strsplit(rownames(res.mfa$partial.axes$coord)[j], 
                  ".", fixed = TRUE)[[1]]
                if (auxil2 != auxil[length(auxil)]) {
                  i = i + 1
                  auxil2 = auxil[length(auxil)]
                }
                couleur.axes <- c(couleur.axes, col.hab[i])
            }
        }
        else {
            couleur.axes <- NULL
            for (i in 1:length(group)) couleur.axes <- c(couleur.axes, 
                rep("black", ncol(res.mfa$partial.axes$coord)))
        }
        for (v in 1:nrow(coord.axes)) {
            arrows(0, 0, coord.axes[v, 1], coord.axes[v, 2], 
                length = 0.1, angle = 15, code = 2, col = couleur.axes[v])
            if (abs(coord.axes[v, 1]) > abs(coord.axes[v, 2])) {
                if (coord.axes[v, 1] >= 0) 
                  pos <- 4
                else pos <- 2
            }
            else {
                if (coord.axes[v, 2] >= 0) 
                  pos <- 3
                else pos <- 1
            }
            text(coord.axes[v, 1], y = coord.axes[v, 2], labels = rownames(coord.axes)[v], 
                pos = pos, col = couleur.axes[v])
        }
        if (habillage == "group") {
            legend("topleft", legend = rownames(res.mfa$group$Lg)[-length(rownames(res.mfa$group$Lg))], 
                text.col = unique(couleur.axes), cex = 0.8)
        }
    }
    if (choix == "group") {
        if (new.plot) 
            dev.new()
        if (is.null(title)) 
            title <- "Groups representation"
        coord.actif <- res.mfa$group$coord[, axes, drop = FALSE]
        if (!is.null(res.mfa$group$coord.sup)) 
            coord.illu <- res.mfa$group$coord.sup[, axes, drop = FALSE]
        if (is.null(col.hab)) {
            col.hab = rep("darkred", nrow(coord.actif))
            if (!is.null(res.mfa$group$coord.sup)) 
                col.hab = c(col.hab, rep("darkolivegreen", nrow(coord.illu)))
        }
        if (habillage == "group") 
            col.hab <- (2:(length(group) + 1))
        plot(coord.actif, xlab = lab.x, ylab = lab.y, xlim = c(0, 
            1), ylim = c(0, 1), pch = 17, col = col.hab[1:nrow(coord.actif)], 
            cex = cex, main = title, cex.main = cex * 1.2, asp = 1)
        if (lab.grpe) 
            text(coord.actif[, 1], y = coord.actif[, 2], labels = rownames(coord.actif), 
                pos = 3, col = col.hab[1:nrow(coord.actif)])
        if (!is.null(res.mfa$group$coord.sup)) {
            points(coord.illu, pch = 17, col = col.hab[(nrow(coord.actif) + 
                1):(nrow(coord.actif) + nrow(coord.illu))])
            if (lab.grpe) 
                text(coord.illu[, 1], y = coord.illu[, 2], labels = rownames(coord.illu), 
                  pos = 3, col = col.hab[(nrow(coord.actif) + 
                    1):(nrow(coord.actif) + nrow(coord.illu))])
        }
    }
    if (choix == "var") {
        if (new.plot) dev.new()
        test.invisible <- vector(length = 2)
        if (!is.null(invisible)) {
            test.invisible[1] <- match("actif", invisible)
            test.invisible[2] <- match("sup", invisible)
        }
        else test.invisible <- rep(NA, 2)
        col <- NULL
        if (habillage == "group") {
            if (is.null(col.hab) | length(col.hab) < length(group[type == "c"])) col.hab <- 2:(length(group[type == "c"]) + 1)
            for (i in 1:length(group[type == "c"])) col <- c(col, rep(col.hab[i], group[type == "c"][i]))
        } else {
            if (is.null(col.hab) | length(col.hab) < sum(group[type == "c"])) col <- rep(1, sum(group[type == "c"]))
            else col <- col.hab
        }
        if (is.null(title))  title <- "Correlation circle"
        plot(0, 0, main = title, xlab = lab.x, ylab = lab.y, 
            xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1), col = "white", 
            asp = 1, cex = cex)
        x.cercle <- seq(-1, 1, by = 0.01)
        y.cercle <- sqrt(1 - x.cercle^2)
        lines(x.cercle, y = y.cercle)
        lines(x.cercle, y = -y.cercle)
        abline(v = 0, lty = 2, cex = cex)
        abline(h = 0, lty = 2, cex = cex)
        if (habillage == "group" & is.na(test.invisible[1]) & is.na(test.invisible[2])) 
            legend("topleft", legend = rownames(res.mfa$group$Lg[-nrow(res.mfa$group$Lg), ])[type == "c"], text.col = col.hab, cex = 0.8)
        if (habillage == "group" & is.na(test.invisible[1]) & !is.na(test.invisible[2])) 
            legend("topleft", legend = rownames(res.mfa$group$Lg[-c(num.group.sup, nrow(res.mfa$group$Lg)), ])[type.act == "c"], 
                text.col = col.hab, cex = 0.8)
        if (habillage == "group" & !is.na(test.invisible[1]) & is.na(test.invisible[2])) 
            legend("topleft", legend = rownames(res.mfa$group$Lg[num.group.sup, ])[type.sup == "c"], text.col = col.hab, cex = 0.8)
        nrow.coord.var <- 0
        if (!is.null(res.mfa["quanti.var"]$quanti.var$coord) & is.na(test.invisible[1])) {
            coord.var <- res.mfa$quanti.var$cor[, axes, drop = FALSE]
            nrow.coord.var <- nrow(coord.var)
            for (v in 1:nrow(coord.var)) {
                if (sum(res.mfa$quanti.var$cos2[v, axes], na.rm = TRUE) >= lim.cos2.var && !is.na(sum(res.mfa$quanti.var$cos2[v, axes], na.rm = TRUE))) {
                  arrows(0, 0, coord.var[v, 1], coord.var[v, 2], length = 0.1, angle = 15, code = 2, col = col[v])
                  if (lab.var) {
                    if (abs(coord.var[v, 1]) > abs(coord.var[v,  2])) {
                      if (coord.var[v, 1] >= 0)  pos <- 4
                      else pos <- 2
                    }
                    else {
                      if (coord.var[v, 2] >= 0) pos <- 3
                      else pos <- 1
                    }
                    text(coord.var[v, 1], y = coord.var[v, 2], labels = rownames(coord.var)[v], pos = pos, col = col[v])
                  }
                }
            }
        }
        if (!is.null(res.mfa$quanti.var.sup$coord) & is.na(test.invisible[2])) {
            coord.var.sup <- res.mfa$quanti.var.sup$cor[, axes, drop = FALSE]
            for (q in 1:nrow(coord.var.sup)) {
                if (sum(res.mfa$quanti.var.sup$cos2[q, axes], na.rm = TRUE) >= 
                  lim.cos2.var && !is.na(sum(res.mfa$quanti.var.sup$cos2[q, 
                  axes], na.rm = TRUE))) {
                arrows(0, 0, coord.var.sup[q, 1], coord.var.sup[q, 
                  2], length = 0.1, angle = 15, code = 2, lty = 2, 
                  col = col[nrow.coord.var + q])
                if (lab.var) {
                  if (abs(coord.var.sup[q, 1]) > abs(coord.var.sup[q, 
                    2])) {
                    if (coord.var.sup[q, 1] >= 0) 
                      pos <- 4
                    else pos <- 2
                  }
                  else {
                    if (coord.var.sup[q, 2] >= 0) 
                      pos <- 3
                    else pos <- 1
                  }
                  text(coord.var.sup[q, 1], y = coord.var.sup[q, 
                    2], labels = rownames(coord.var.sup)[q], 
                    pos = pos, col = col[nrow.coord.var + q])
                }
		  }
            }
        }
        par(mar = c(5, 4, 4, 2) + 0.1)
    }

	if (choix=="freq"){
      if (new.plot) dev.new()
      col.row = "black"
	  col.row.sup = "grey60"
      coord.col <- res.mfa$freq$coord[, axes]
      coord.row <- res.mfa$ind$coord[, axes]
      coord.row.sup <- coord.col.sup <- NULL
      if (!is.null(res.mfa$ind.sup)) coord.row.sup <- res.mfa$ind.sup$coord[, axes]
      if (!is.null(res.mfa$freq.sup)) coord.col.sup <- res.mfa$freq.sup$coord[, axes]

      test.invisible <- vector(length = 4)
      if (!is.null(invisible)) {
          test.invisible[1] <- match("row", invisible)
		  if (is.na(test.invisible[1])) test.invisible[1] <- match("ind", invisible)
          test.invisible[2] <- match("col", invisible)
		  if (is.na(test.invisible[2])) test.invisible[2] <- match("var", invisible)
          test.invisible[3] <- match("row.sup", invisible)
		  if (is.na(test.invisible[3])) test.invisible[3] <- match("ind.sup", invisible)
          test.invisible[4] <- match("col.sup", invisible)
		  if (is.na(test.invisible[4])) test.invisible[4] <- match("var.sup", invisible)
      }
      else  test.invisible <- rep(NA, 4)
      if (is.null(xlim)) {
        xmin <- xmax <- 0
        if(is.na(test.invisible[1])) xmin <- min(xmin, coord.row[,1])
        if(is.na(test.invisible[1])) xmax <- max(xmax, coord.row[,1])
        if(is.na(test.invisible[3])) xmin <- min(xmin, coord.row.sup[, 1])
        if(is.na(test.invisible[3])) xmax <- max(xmax, coord.row.sup[, 1])
        if(is.na(test.invisible[2])) xmin <- min(xmin, coord.col[,1])
        if(is.na(test.invisible[2])) xmax <- max(xmax, coord.col[,1])
        if(is.na(test.invisible[4])) xmin <- min(xmin, coord.col.sup[, 1])
        if(is.na(test.invisible[4])) xmax <- max(xmax, coord.col.sup[, 1])
          xlim <- c(xmin, xmax) * 1.2
      }
      else {
        xmin = xlim[1]
        xmax = xlim[2]
      }
      if (is.null(ylim)) {
        ymin <- ymax <- 0
        if(is.na(test.invisible[1])) ymin <- min(ymin, coord.row[,2])
        if(is.na(test.invisible[1])) ymax <- max(ymax, coord.row[,2])
        if(is.na(test.invisible[3])) ymin <- min(ymin, coord.row.sup[,2])
        if(is.na(test.invisible[3])) ymax <- max(ymax, coord.row.sup[,2])
        if(is.na(test.invisible[2])) ymin <- min(ymin, coord.col[,2])
        if(is.na(test.invisible[2])) ymax <- max(ymax, coord.col[,2])
        if(is.na(test.invisible[4])) ymin <- min(ymin, coord.col.sup[,2])
        if(is.na(test.invisible[4])) ymax <- max(ymax, coord.col.sup[,2])
        ylim <- c(ymin, ymax) * 1.2
      }
      else {
        ymin = ylim[1]
        ymax = ylim[2]
      }

      col <- NULL
      if (habillage == "group") {
          if (is.null(col.hab) | length(col.hab) < length(group[type == "f"])) col.hab <- 2:(length(group[type == "f"]) + 1)
          for (i in 1:length(group[type == "f"])) col <- c(col, rep(col.hab[i], group[type == "f"][i]))
      } else {
          if (is.null(col.hab) | length(col.hab) < sum(group[type == "f"])) col <- rep(1, sum(group[type == "f"]))
          else col <- col.hab
      }

      if (is.null(title)) titre <- "Factor map for the contingency table(s)"
      else titre <- title
      plot(0, 0, main = titre, xlab = paste("Dim ",axes[1]," (",signif(res.mfa$eig[axes[1],2],4),"%)",sep=""), ylab = paste("Dim ",axes[2]," (",signif(res.mfa$eig[axes[2],2],4),"%)",sep=""), xlim = xlim, ylim = ylim, col = "white", asp=1, cex=cex)
      abline(h=0,lty=2)
      abline(v=0,lty=2)
      if (is.na(test.invisible[1])) {
        points(coord.row, pch = 20, col = col.row, cex = cex)
        if (lab.ind)  text(coord.row[, 1], y = coord.row[, 2], labels = rownames(coord.row), pos = 3, col = col.row, cex = cex)
      }
      if (is.na(test.invisible[2])) {
        for (v in 1:nrow(coord.col)){
		  points(coord.col[v, 1], y = coord.col[v, 2], pch = 17, col = col[v], cex = cex)
          if (lab.col) text(coord.col[v, 1], y = coord.col[v, 2], labels = rownames(coord.col)[v], pos = 3, col = col[v], cex = cex)
		 }
      }
      if (!is.null(res.mfa$quanti.sup) & is.na(test.invisible[4])) {
        for (v in 1:nrow(coord.col)){
		  points(coord.col.sup[v, 1], y = coord.col.sup[v, 2], pch = 17, col = col[v], cex = cex)
          if (lab.col) text(coord.col.sup[v, 1], y = coord.col.sup[v, 2], labels = rownames(coord.col.sup)[v], pos = 3, col = col[v], cex = cex)
		 }
      }
      if (!is.null(res.mfa$ind.sup) & is.na(test.invisible[3])) {
        points(coord.row.sup[, 1], y = coord.row.sup[, 2], pch = 20, col = col.row.sup, cex = cex)
        if (lab.ind) text(coord.row.sup[, 1], y = coord.row.sup[, 2], labels = rownames(coord.row.sup), pos = 3, col = col.row.sup, cex = cex)
      }
        if (habillage == "group") legend("topleft", legend = rownames(res.mfa$group$Lg[-nrow(res.mfa$group$Lg), ])[type == "f"], text.col = col.hab, cex = 0.8)
	}

    if (choix == "ind") {
        test.invisible <- vector(length = 3)
        if (!is.null(invisible)) {
            test.invisible[1] <- match("ind", invisible)
            test.invisible[2] <- match("ind.sup", invisible)
            test.invisible[3] <- match("quali", invisible)
        }
        else test.invisible <- rep(NA, 3)
        nb.ind.actif <- nrow(res.mfa$ind$coord)
        nb.ind.illu <- 0
        if (!is.null(res.mfa$ind.sup)) 
            nb.ind.illu <- nrow(res.mfa$ind.sup$coord)
        nb.ind <- nb.ind.actif + nb.ind.illu
        coord.ind <- res.mfa$ind$coord[, axes, drop = FALSE]
        coord.ind.partiel <- res.mfa$ind$coord.partiel[, axes, 
            drop = FALSE]
        coord.ind.sup <- NULL
        if (!is.null(res.mfa$ind.sup)) {
            coord.ind.sup <- res.mfa$ind.sup$coord[, axes, drop = FALSE]
            coord.ind.partiel.sup <- res.mfa$ind.sup$coord.partiel[, 
                axes, drop = FALSE]
        }
        coord.quali <- coord.quali.sup <- coord.quali.partiel <- coord.quali.sup.partiel <- NULL
        nrow.coord.quali <- 0
        if (!is.null(res.mfa["quali.var"]$quali.var)) {
            coord.quali <- res.mfa$quali.var$coord[, axes, drop = FALSE]
            coord.quali.partiel <- res.mfa$quali.var$coord.partiel[, 
                axes, drop = FALSE]
            nrow.coord.quali <- nrow(coord.quali)
        }
        if (!is.null(res.mfa["quali.var.sup"])) {
            coord.quali.sup <- res.mfa$quali.var.sup$coord[, 
                axes, drop = FALSE]
            coord.quali.partiel.sup <- res.mfa$quali.var.sup$coord.partiel[, 
                axes, drop = FALSE]
        }
        group.ind.actif <- group.ind.sup <- group.quali <- group.quali.sup <- NULL
        if (!is.null(partial)) {
            if (length(partial) == 1) {
                if (partial == "all") {
                  group.ind.actif <- 1:nrow(coord.ind)
                  if (!is.null(res.mfa$ind.sup)) 
                    group.ind.sup <- 1:nrow(coord.ind.sup)
                  if (!is.null(res.mfa["quali.var"]$quali.var)) 
                    group.quali <- 1:nrow(coord.quali)
                  if (!is.null(res.mfa["quali.var.sup"]$quali.var.sup)) 
                    group.quali.sup <- 1:nrow(coord.quali.sup)
                }
                else {
                  for (i in 1:length(partial)) {
                    if (partial[i] %in% rownames(coord.ind)) 
                      group.ind.actif <- c(group.ind.actif, match(partial[i], 
                        rownames(coord.ind)))
                    if (partial[i] %in% rownames(coord.ind.sup)) 
                      group.ind.sup <- c(group.ind.sup, match(partial[i], 
                        rownames(coord.ind.sup)))
                    if (partial[i] %in% rownames(coord.quali)) 
                      group.quali <- c(group.quali, match(partial[i], 
                        rownames(coord.quali)))
                    if (partial[i] %in% rownames(coord.quali.sup)) 
                      group.quali.sup <- c(group.quali.sup, match(partial[i], 
                        rownames(coord.quali.sup)))
                  }
                }
            }
            else {
                for (i in 1:length(partial)) {
                  if (partial[i] %in% rownames(coord.ind)) 
                    group.ind.actif <- c(group.ind.actif, match(partial[i], 
                      rownames(coord.ind)))
                  if (partial[i] %in% rownames(coord.ind.sup)) 
                    group.ind.sup <- c(group.ind.sup, match(partial[i], 
                      rownames(coord.ind.sup)))
                  if (partial[i] %in% rownames(coord.quali)) 
                    group.quali <- c(group.quali, match(partial[i], 
                      rownames(coord.quali)))
                  if (partial[i] %in% rownames(coord.quali.sup)) 
                    group.quali.sup <- c(group.quali.sup, match(partial[i], 
                      rownames(coord.quali.sup)))
                }
            }
        }
        if (!is.null(ellipse)) {
            coord.ellipse <- ellipse$res
            npoint.ellipse <- ellipse$call
        }
        else coord.ellipse <- NULL
        if (!is.null(ellipse.par)) {
            coord.ellipse.par <- ellipse.par$res
            npoint.ellipse.par <- ellipse.par$call
        }
        else coord.ellipse.par <- NULL
        if (is.null(xlim)) {
            xmin <- xmax <- 0
            if (is.na(test.invisible[1])) 
                xmin <- min(xmin, coord.ind[, 1])
            if (is.na(test.invisible[1])) 
                xmax <- max(xmax, coord.ind[, 1])
            if (!is.null(res.mfa$ind.sup) & is.na(test.invisible[2])) 
                xmin <- min(xmin, coord.ind.sup[, 1])
            if (!is.null(res.mfa$ind.sup) & is.na(test.invisible[2])) 
                xmax <- max(xmax, coord.ind.sup[, 1])
            if (is.na(test.invisible[1])) 
                xmin <- min(xmin, coord.ind.partiel[unlist(lapply(group.ind.actif, 
                  function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                  1])
            if (is.na(test.invisible[1])) 
                xmax <- max(xmax, coord.ind.partiel[unlist(lapply(group.ind.actif, 
                  function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                  1])
            if (!is.null(res.mfa$ind.sup) & is.na(test.invisible[2])) 
                xmin <- min(xmin, coord.ind.partiel.sup[unlist(lapply(group.ind.sup, 
                  function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                  1])
            if (!is.null(res.mfa$ind.sup) & is.na(test.invisible[2])) 
                xmax <- max(xmax, coord.ind.partiel.sup[unlist(lapply(group.ind.sup, 
                  function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                  1])
            if (!is.null(res.mfa["quali.var"]$quali.var) & is.na(test.invisible[3])) 
                xmin <- min(xmin, coord.quali[, 1])
            if (!is.null(res.mfa["quali.var"]$quali.var) & is.na(test.invisible[3])) 
                xmax <- max(xmax, coord.quali[, 1])
            if (!is.null(res.mfa["quali.var"]$quali.var) & is.na(test.invisible[3])) 
                xmin <- min(xmin, coord.quali.partiel[unlist(lapply(group.quali, 
                  function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                  1])
            if (!is.null(res.mfa["quali.var"]$quali.var) & is.na(test.invisible[3])) 
                xmax <- max(xmax, coord.quali.partiel[unlist(lapply(group.quali, 
                  function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                  1])
            if (!is.null(res.mfa$quali.var.sup) & is.na(test.invisible[3])) 
                xmin <- min(xmin, coord.quali[, 1], coord.quali.sup[, 
                  1])
            if (!is.null(res.mfa$quali.var.sup) & is.na(test.invisible[3])) 
                xmax <- max(xmax, coord.quali[, 1], coord.quali.sup[, 
                  1])
            if (!is.null(res.mfa$quali.var.sup) & is.na(test.invisible[3])) 
                xmin <- min(xmin, coord.quali.partiel.sup[unlist(lapply(group.quali.sup, 
                  function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                  1])
            if (!is.null(res.mfa$quali.var.sup) & is.na(test.invisible[3])) 
                xmax <- max(xmax, coord.quali.partiel.sup[unlist(lapply(group.quali.sup, 
                  function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                  1])
            xlim <- c(xmin, xmax) * 1.1
        }
        else {
            xmin = xlim[1]
            xmax = xlim[2]
        }
        if (is.null(ylim)) {
            ymin <- ymax <- 0
            if (is.na(test.invisible[1])) 
                ymin <- min(ymin, coord.ind[, 2])
            if (is.na(test.invisible[1])) 
                ymax <- max(ymax, coord.ind[, 2])
            if (!is.null(res.mfa$ind.sup) & is.na(test.invisible[2])) 
                ymin <- min(ymin, coord.ind.sup[, 2])
            if (!is.null(res.mfa$ind.sup) & is.na(test.invisible[2])) 
                ymax <- max(ymax, coord.ind.sup[, 2])
            if (is.na(test.invisible[1])) 
                ymin <- min(ymin, coord.ind.partiel[unlist(lapply(group.ind.actif, 
                  function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                  2])
            if (is.na(test.invisible[1])) 
                ymax <- max(ymax, coord.ind.partiel[unlist(lapply(group.ind.actif, 
                  function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                  2])
            if (!is.null(res.mfa$ind.sup) & is.na(test.invisible[2])) 
                ymin <- min(ymin, coord.ind.partiel.sup[unlist(lapply(group.ind.sup, 
                  function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                  2])
            if (!is.null(res.mfa$ind.sup) & is.na(test.invisible[2])) 
                ymax <- max(ymax, coord.ind.partiel.sup[unlist(lapply(group.ind.sup, 
                  function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                  2])
            if (!is.null(res.mfa["quali.var"]$quali.var) & is.na(test.invisible[3])) 
                ymin <- min(ymin, coord.quali[, 2])
            if (!is.null(res.mfa["quali.var"]$quali.var) & is.na(test.invisible[3])) 
                ymax <- max(ymax, coord.quali[, 2])
            if (!is.null(res.mfa["quali.var"]$quali.var) & is.na(test.invisible[3])) 
                ymin <- min(ymin, coord.quali.partiel[unlist(lapply(group.quali, 
                  function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                  2])
            if (!is.null(res.mfa["quali.var"]$quali.var) & is.na(test.invisible[3])) 
                ymax <- max(ymax, coord.quali.partiel[unlist(lapply(group.quali, 
                  function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                  2])
            if (!is.null(res.mfa$quali.var.sup) & is.na(test.invisible[3])) 
                ymin <- min(ymin, coord.quali[, 1], coord.quali.sup[, 
                  2])
            if (!is.null(res.mfa$quali.var.sup) & is.na(test.invisible[3])) 
                ymax <- max(ymax, coord.quali[, 1], coord.quali.sup[, 
                  2])
            if (!is.null(res.mfa$quali.var.sup) & is.na(test.invisible[3])) 
                ymin <- min(ymin, coord.quali.partiel.sup[unlist(lapply(group.quali.sup, 
                  function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                  2])
            if (!is.null(res.mfa$quali.var.sup) & is.na(test.invisible[3])) 
                ymax <- max(ymax, coord.quali.partiel.sup[unlist(lapply(group.quali.sup, 
                  function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                  2])
            ylim <- c(ymin, ymax) * 1.1
        }
        else {
            ymin = ylim[1]
            ymax = ylim[2]
        }
        if (habillage == "group") {
            if (is.null(col.hab) | length(col.hab) != (nbre.grpe)) 
                col.hab <- 2:(nbre.grpe + 1)
            col.ind <- c(rep(1, nb.ind.actif), rep(col.hab, nb.ind.actif))
            if (!is.null(res.mfa$ind.sup)) 
                col.ind.sup <- c(rep(1, nb.ind - nb.ind.actif), 
                  rep(col.hab, nb.ind - nb.ind.actif))
            if (length(group[type == "n"]) != 0) 
                col.quali <- c(rep(1, sum(res.mfa$call$group.mod[type == 
                  "n"])), rep(col.hab, sum(res.mfa$call$group.mod[type == 
                  "n"])))
            if (!is.null(res.mfa$quali.var.sup)) 
                col.quali.sup <- c(rep(1, sum(res.mfa$call$group.mod[num.group.sup][type.sup == 
                  "n"])), rep(col.hab, sum(res.mfa$call$group.mod[num.group.sup][type.sup == 
                  "n"])))
            if (!is.null(ellipse)) 
                col.ellipse <- rep(1, nb.ind.actif)
            if (!is.null(ellipse.par)) 
                col.ellipse.par <- rep(col.hab, nb.ind.actif)
        }
        if (habillage == "ind") {
            if (is.null(col.hab) | length(col.hab) != nb.ind) {
                col.hab <- 1:nb.ind
            }
            col.ind <- c(col.hab[1:nb.ind.actif], rep(col.hab[1:nb.ind.actif], 
                each = nbre.grpe))
            if (!is.null(res.mfa$ind.sup)) 
                col.ind.sup <- c(col.hab[(nb.ind.actif + 1):nb.ind], 
                  rep(col.hab[(nb.ind.actif + 1):nb.ind], each = nbre.grpe))
            if (length(group[type == "n"]) != 0) 
                col.quali <- col.quali.sup <- rep("black", (1 + 
                  nbre.grpe) * sum(res.mfa$call$group.mod[type == 
                  "n"]))
            if (!is.null(ellipse)) 
                col.ellipse <- col.hab[1:nb.ind.actif]
            if (!is.null(ellipse.par)) 
                col.ellipse.par <- rep(col.hab[1:nb.ind.actif], 
                  each = nbre.grpe)
        }
        if ((habillage != "none") & (habillage != "ind") & (habillage != 
            "group")) {
            group.act <- (1:length(group))
            if (!is.null(num.group.sup)) 
                group.act <- group.act[-num.group.sup]
            nbre.modalite <- NULL
            liste.quali <- NULL
            for (i in group.act) {
                if (type[i] == "n") {
                  for (k in 1:ncol(res.mfa$separate.analyses[[i]]$call$X)) nbre.modalite <- c(nbre.modalite, 
                    nlevels(res.mfa$separate.analyses[[i]]$call$X[, 
                      k]))
                  if (i == 1) 
                    liste.quali <- c(liste.quali, colnames(res.mfa$call$X[1:group[1]]))
                  else liste.quali <- c(liste.quali, colnames(res.mfa$call$X[(sum(group[1:(i - 
                    1)]) + 1):sum(group[1:i])]))
                }
            }
            if (!is.null(num.group.sup)) {
                for (i in num.group.sup) {
                  if (type[i] == "n") {
                    if (i == 1) 
                      liste.quali <- c(liste.quali, colnames(res.mfa$call$X[1:group[1]]))
                    else liste.quali <- c(liste.quali, colnames(res.mfa$call$X[(sum(group[1:(i - 
                      1)]) + 1):sum(group[1:i])]))
                    for (k in 1:ncol(res.mfa$separate.analyses[[i]]$call$X)) nbre.modalite <- c(nbre.modalite, 
                      nlevels(res.mfa$separate.analyses[[i]]$call$X[, 
                        k]))
                  }
                }
            }
            if (is.double(habillage)) 
                nom.quali <- colnames(res.mfa$call$X)[habillage]
            else nom.quali = habillage
            if (!(nom.quali %in% liste.quali)) 
                stop("The variable ", habillage, " is not qualitative")
            modalite <- levels(as.factor(res.mfa$call$X[, nom.quali]))
            col.ind <- as.numeric(as.factor(res.mfa$call$X[, 
                nom.quali]))
            if (is.null(col.hab) | length(col.hab) != length(modalite)) 
                col.hab <- 2:(1 + length(modalite))
            col.ind <- col.hab[col.ind]
            if (!is.null(res.mfa$call$ind.sup)) {
                col.ind.sup <- col.ind[res.mfa$call$ind.sup]
                col.ind <- col.ind[-res.mfa$call$ind.sup]
                col.ind.sup <- c(col.ind.sup, rep(col.ind.sup, 
                  each = nbre.grpe))
            }
            col.ind <- c(col.ind, rep(col.ind, each = nbre.grpe))
            col.ellipse <- col.ind[1:nb.ind.actif]
            col.ellipse.par <- col.ind[-c(1:nb.ind.actif)]
            indice.inf <- sum(nbre.modalite[0:(match(nom.quali, 
                liste.quali) - 1)]) + 1
            indice.sup <- indice.inf + length(modalite) - 1
            col.quali <- rep("black", sum(res.mfa$call$group.mod[type == 
                "n"]))
            if (length(group[type == "n"]) != 0) {
                for (i in 1:length(liste.quali)) {
                  if (liste.quali[i] == nom.quali) 
                    col.quali[indice.inf:indice.sup] <- col.hab
                }
            }
            col.quali <- c(col.quali, rep(col.quali, each = nbre.grpe))
            col.quali.sup <- col.quali
        }
        if (habillage == "none") 
            col.ind <- col.ind.sup <- col.quali.sup <- col.quali <- col.ellipse <- col.ellipse.par <- rep("black", 
                nb.ind * (nbre.grpe + 1))
        if (new.plot) 
            dev.new(width = min(14, max(8, 8 * (xmax - xmin)/(ymax - 
                ymin))), height = 8)
        if (is.null(title)) 
            title <- "Individual factor map"
        plot(0, 0, main = title, xlab = lab.x, ylab = lab.y, 
            xlim = xlim, ylim = ylim, col = "white", asp = 1, 
            cex = cex)
        abline(v = 0, lty = 2, cex = cex)
        abline(h = 0, lty = 2, cex = cex)
        if (is.na(test.invisible[1])) {
            points(coord.ind, pch = 20, col = col.ind[1:nb.ind.actif], 
                cex = cex)
            if (lab.ind) 
                text(coord.ind[, 1], y = coord.ind[, 2], labels = rownames(coord.ind), 
                  pos = 3, col = col.ind[1:nb.ind.actif])
            for (i in group.ind.actif) {
                for (j in 1:nbre.grpe) {
                  points(coord.ind.partiel[(i - 1) * nbre.grpe + 
                    j, ], cex = 0.8 * cex, col = col.ind[nb.ind.actif + 
                    (i - 1) * nbre.grpe + j], pch = 20)
                  if (lab.par) 
                    text(coord.ind.partiel[(i - 1) * nbre.grpe + 
                      j, 1], y = coord.ind.partiel[(i - 1) * 
                      nbre.grpe + j, 2], labels = rownames(coord.ind.partiel)[(i - 
                      1) * nbre.grpe + j], pos = 3, col = col.ind[nb.ind.actif + 
                      (i - 1) * nbre.grpe + j])
                  if (chrono) {
                    if (j > 1) 
                      lines(c(coord.ind.partiel[(i - 1) * nbre.grpe + 
                        (j - 1), 1], coord.ind.partiel[(i - 1) * 
                        nbre.grpe + j, 1]), c(coord.ind.partiel[(i - 
                        1) * nbre.grpe + (j - 1), 2], coord.ind.partiel[(i - 
                        1) * nbre.grpe + j, 2]), col = col.ind[i])
                  }
                  else lines(c(coord.ind[i, 1], coord.ind.partiel[(i - 
                    1) * nbre.grpe + j, 1]), c(coord.ind[i, 2], 
                    coord.ind.partiel[(i - 1) * nbre.grpe + j, 
                      2]), col = col.ind[nb.ind.actif + (i - 
                    1) * nbre.grpe + j], lty = j)
                }
            }
        }
        if (!is.null(res.mfa$ind.sup) & is.na(test.invisible[2])) {
            points(coord.ind.sup, pch = 21, col = col.ind.sup[1:(nb.ind - 
                nb.ind.actif)], cex = cex)
            if (lab.ind) 
                text(coord.ind.sup[, 1], y = coord.ind.sup[, 
                  2], labels = rownames(coord.ind.sup), pos = 3, 
                  col = col.ind.sup[1:(nb.ind - nb.ind.actif)])
            for (i in group.ind.sup) {
                for (j in 1:nbre.grpe) {
                  points(coord.ind.partiel.sup[(i - 1) * nbre.grpe + 
                    j, ], cex = 0.8 * cex, col = col.ind.sup[nb.ind - 
                    nb.ind.actif + (i - 1) * nbre.grpe + j], 
                    pch = 21)
                  if (lab.par) 
                    text(coord.ind.partiel.sup[(i - 1) * nbre.grpe + 
                      j, 1], y = coord.ind.partiel.sup[nb.ind + 
                      (i - 1) * nbre.grpe + j, 2], labels = rownames(coord.ind.partiel.sup)[(i - 
                      1) * nbre.grpe + j], pos = 3, col = col.ind.sup[nb.ind - 
                      nb.ind.actif + (i - 1) * nbre.grpe + j])
                  if (chrono) {
                    if (j > 1) 
                      lines(c(coord.ind.partiel.sup[(i - 1) * 
                        nbre.grpe + (j - 1), 1], coord.ind.partiel.sup[(i - 
                        1) * nbre.grpe + j, 1]), c(coord.ind.partiel.sup[(i - 
                        1) * nbre.grpe + (j - 1), 2], coord.ind.partiel.sup[(i - 
                        1) * nbre.grpe + j, 2]), col = col.ind[nb.ind.actif + 
                        i])
                  }
                  else lines(c(coord.ind.sup[i, 1], coord.ind.partiel.sup[(i - 
                    1) * nbre.grpe + j, 1]), c(coord.ind.sup[i, 
                    2], coord.ind.partiel.sup[(i - 1) * nbre.grpe + 
                    j, 2]), col = col.ind.sup[nb.ind - nb.ind.actif + 
                    (i - 1) * nbre.grpe + j], lty = j)
                }
            }
        }
        if (!is.null(coord.quali) & is.na(test.invisible[3])) {
            points(coord.quali, pch = 15, col = col.quali[1:nrow.coord.quali], 
                cex = cex)
            if (lab.var) 
                text(coord.quali[, 1], y = coord.quali[, 2], 
                  labels = rownames(coord.quali), pos = 3, col = col.quali[1:nrow.coord.quali])
            for (i in group.quali) {
                for (j in 1:nbre.grpe) {
                  points(coord.quali.partiel[(i - 1) * nbre.grpe + 
                    j, ], pch = 15, col = col.quali[nrow.coord.quali + 
                    (i - 1) * nbre.grpe + j], cex = cex * 0.8)
                  if (lab.var & lab.par) 
                    text(coord.quali.partiel[(i - 1) * nbre.grpe + 
                      j, 1], y = coord.quali.partiel[(i - 1) * 
                      nbre.grpe + j, 2], labels = rownames(coord.quali.partiel)[(i - 
                      1) * nbre.grpe + j], pos = 3, col = col.quali[nrow.coord.quali + 
                      (i - 1) * nbre.grpe + j])
                  if (chrono) {
                    if (j > 1) 
                      lines(c(coord.quali.partiel[(i - 1) * nbre.grpe + 
                        (j - 1), 1], coord.quali.partiel[(i - 
                        1) * nbre.grpe + j, 1]), c(coord.quali.partiel[(i - 
                        1) * nbre.grpe + (j - 1), 2], coord.quali.partiel[(i - 
                        1) * nbre.grpe + j, 2]), col = col.quali[i])
                  }
                  else lines(c(coord.quali[i, 1], coord.quali.partiel[(i - 
                    1) * nbre.grpe + j, 1]), c(coord.quali[i, 
                    2], coord.quali.partiel[(i - 1) * nbre.grpe + 
                    j, 2]), col = col.quali[nrow.coord.quali + 
                    (i - 1) * nbre.grpe + j], lty = j)
                }
            }
        }
        if (!is.null(coord.quali.sup) & is.na(test.invisible[3])) {
            points(coord.quali.sup, pch = 22, col = col.quali.sup[1:nrow(coord.quali.sup)], 
                cex = cex)
            if (lab.var) 
                text(coord.quali.sup[, 1], y = coord.quali.sup[, 
                  2], labels = rownames(coord.quali.sup), pos = 3, 
                  col = col.quali.sup[1:nrow(coord.quali.sup)])
            for (i in group.quali.sup) {
                for (j in 1:nbre.grpe) {
                  points(coord.quali.partiel.sup[(i - 1) * nbre.grpe + 
                    j, ], pch = 22, col = col.quali.sup[nrow(coord.quali.sup) + 
                    (i - 1) * nbre.grpe + j], cex = cex * 0.8)
                  if (lab.var & lab.par) 
                    text(coord.quali.partiel.sup[(i - 1) * nbre.grpe + 
                      j, 1], y = coord.quali.partiel.sup[(i - 
                      1) * nbre.grpe + j, 2], labels = rownames(coord.quali.partiel.sup)[(i - 
                      1) * nbre.grpe + j], pos = 3, col = col.quali.sup[nrow(coord.quali.sup) + 
                      (i - 1) * nbre.grpe + j])
                  if (chrono) {
                    if (j > 1) 
                      lines(c(coord.quali.partiel.sup[(i - 1) * 
                        nbre.grpe + (j - 1), 1], coord.quali.partiel.sup[(i - 
                        1) * nbre.grpe + j, 1]), c(coord.quali.partiel.sup[(i - 
                        1) * nbre.grpe + (j - 1), 2], coord.quali.partiel.sup[(i - 
                        1) * nbre.grpe + j, 2]), col = col.quali[nrow.coord.quali + 
                        i])
                  }
                  else lines(c(coord.quali.sup[i, 1], coord.quali.partiel.sup[(i - 
                    1) * nbre.grpe + j, 1]), c(coord.quali.sup[i, 
                    2], coord.quali.partiel.sup[(i - 1) * nbre.grpe + 
                    j, 2]), col = col.quali.sup[nrow(coord.quali.sup) + 
                    (i - 1) * nbre.grpe + j], lty = j)
                }
            }
        }
        if ((!is.null(partial)) & (habillage == "group")) 
            legend("topleft", legend = rownames(res.mfa$group$Lg)[-c(num.group.sup, 
                length(rownames(res.mfa$group$Lg)))], lty = 1:length(rownames(res.mfa$group$Lg)[-c(num.group.sup, 
                length(rownames(res.mfa$group$Lg)))]), text.col = col.hab, 
                col = col.hab, cex = 0.8)
        if ((!is.null(partial)) & (habillage != "group")) 
            legend("topleft", legend = rownames(res.mfa$group$Lg)[-c(num.group.sup, 
                length(rownames(res.mfa$group$Lg)))], lty = 1:length(rownames(res.mfa$group$Lg)[-c(num.group.sup, 
                length(rownames(res.mfa$group$Lg)))]), cex = 0.8)
        if ((habillage != "none") & (habillage != "ind") & (habillage != 
            "group")) 
            legend("topleft", legend = levels(res.mfa$call$X[, 
                habillage]), text.col = col.hab, cex = 0.8)
        if (!is.null(coord.ellipse) & is.na(test.invisible[2])) {
            for (e in 1:nb.ind.actif) {
                debut <- ((nb.ind.actif - 1) * npoint.ellipse) + 
                  1
                fin <- debut + npoint.ellipse - 1
                data.elli <- coord.ellipse[debut:fin, -1]
                lines(data.elli[, 1], y = data.elli[, 2], col = col.ellipse[e])
            }
        }
        if (!is.null(coord.ellipse)) {
            for (e in 1:nlevels(coord.ellipse[, 1])) {
                data.elli <- coord.ellipse[(npoint.ellipse * 
                  (e - 1) + 1):(npoint.ellipse * e), -1]
                lines(data.elli[, 1], y = data.elli[, 2], col = col.ellipse[e])
            }
        }
        if (!is.null(coord.ellipse.par)) {
            for (i in group.ind.actif) {
                for (j in 1:nbre.grpe) {
                  ind.e <- (i - 1) * nbre.grpe + j
                  data.elli <- coord.ellipse.par[(npoint.ellipse.par * 
                    (ind.e - 1) + 1):(npoint.ellipse.par * ind.e), 
                    -1]
                  lines(data.elli[, 1], y = data.elli[, 2], col = col.ellipse.par[ind.e], 
                    lty = 2)
                }
            }
        }
    }
}