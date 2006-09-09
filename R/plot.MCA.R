plot.MCA <- function (x, axes = c(1, 2), 
    xlim = NULL, ylim = NULL, invisible = NULL,
    col.ind = "blue", col.var = "red", col.quali.sup = "darkred",
    col.ind.sup = "darkblue", label="all", cex = 1, title = NULL, ...){
    
    res.mca <- x
    if (!inherits(res.mca, "MCA")) stop("non convenient data")

    lab.ind <- lab.var <- lab.quali.sup <- lab.ind.sup <- FALSE
    if(length(label)==1 && label=="all") lab.ind <- lab.var <- lab.quali.sup <- lab.ind.sup <-TRUE
    if("ind" %in% label) lab.ind<-TRUE
    if("var" %in% label) lab.var<-TRUE
    if("quali.sup" %in% label) lab.quali.sup<-TRUE
    if("ind.sup" %in% label) lab.ind.sup<-TRUE

    test.invisible <- vector(length = 3)
    if (!is.null(invisible)) {
        test.invisible[1] <- match("ind", invisible)
        test.invisible[2] <- match("var", invisible)
        test.invisible[3] <- match("quanti.sup", invisible)
    }
    else  test.invisible <- rep(NA, 3)
    coord.var <- res.mca$var$coord[, axes]
    coord.ind <- res.mca$ind$coord[, axes]
    coord.ind.sup <- coord.quali.sup <- NULL
    if (!is.null(res.mca$ind.sup)) coord.ind.sup <- res.mca$ind.sup$coord[, axes]
    if (!is.null(res.mca$quali.sup)) coord.quali.sup <- res.mca$quali.sup$coord[, axes]
    if (is.null(xlim)) {
      xmin <- xmax <- 0
      if(is.na(test.invisible[1])) xmin <- min(xmin, coord.ind[,1], coord.ind.sup[, 1])
      if(is.na(test.invisible[1])) xmax <- max(xmax, coord.ind[,1], coord.ind.sup[, 1])
      if(is.na(test.invisible[2])) xmin <- min(xmin, coord.var[,1], coord.quali.sup[, 1])
      if(is.na(test.invisible[2])) xmax <- max(xmax, coord.var[,1], coord.quali.sup[, 1])
        xlim <- c(xmin, xmax) * 1.2
    }
    else {
      xmin = xlim[1]
      xmax = xlim[2]
    }
    if (is.null(ylim)) {
      ymin <- ymax <- 0
      if(is.na(test.invisible[1])) ymin <- min(ymin, coord.ind[,2], coord.ind.sup[, 2])
      if(is.na(test.invisible[1])) ymax <- max(ymax, coord.ind[,2], coord.ind.sup[, 2])
      if(is.na(test.invisible[2])) ymin <- min(ymin, coord.var[,2], coord.quali.sup[, 2])
      if(is.na(test.invisible[2])) ymax <- max(ymax, coord.var[,2], coord.quali.sup[, 2])
      ylim <- c(ymin, ymax) * 1.2
    }
    else {
      ymin = ylim[1]
      ymax = ylim[2]
    }
    sub.titre <- NULL
    if (is.null(title)) titre <- "MCA factor map"
    else sub.titre <- "MCA factor map"
    plot(0, 0, main = titre, xlab = paste("Dim ",axes[1]," (",signif(res.mca$eig[axes[1],2],4),"%)",sep=""), ylab = paste("Dim ",axes[2]," (",signif(res.mca$eig[axes[2],2],4),"%)",sep=""), xlim = xlim, ylim = ylim, col = "white", asp=1, cex=cex)
    if (!is.null(sub.titre)) title(sub = sub.titre, cex.sub = cex, font.sub = 2, col.sub = "steelblue4", adj = 0, line = 3.8)
    abline(v=0,lty=2, cex=cex)
    abline(h=0,lty=2, cex=cex)
    if (is.na(test.invisible[1])) {
        points(coord.ind, pch = 16, col = col.ind)
        if (lab.ind) text(coord.ind[, 1], y = coord.ind[, 2], labels = rownames(coord.ind), pos = 3, col = col.ind)
    }
    if (is.na(test.invisible[2])) {
        points(coord.var[, 1], y = coord.var[, 2], pch = 17, col = col.var)
        if (lab.var) text(coord.var[, 1], y = coord.var[, 2], labels = rownames(coord.var), pos = 3, col = col.var)
    }
    if (!is.null(res.mca$quali.sup) & is.na(test.invisible[2])) {
        points(coord.quali.sup[, 1], y = coord.quali.sup[, 2], pch = 17, col = col.quali.sup)
        if (lab.quali.sup) text(coord.quali.sup[, 1], y = coord.quali.sup[, 2], labels = rownames(coord.quali.sup), pos = 3, col = col.quali.sup)
    }
    if (!is.null(res.mca$ind.sup) & is.na(test.invisible[1])) {
        points(coord.ind.sup[, 1], y = coord.ind.sup[, 2], pch = 16, col = col.ind.sup)
        if (lab.ind.sup) text(coord.ind.sup[, 1], y = coord.ind.sup[, 2], labels = rownames(coord.ind.sup), pos = 3, col = col.ind.sup)
    }
    if (!is.null(res.mca$quanti.sup)&is.na(test.invisible[3])) {
      get(getOption("device"))(width=8,height=8)
      plot(0, 0, main = "Supplementary variables on the MCA factor map", xlab = paste("Dim ",axes[1]," (",signif(res.mca$eig[axes[1],2],4),"%)",sep=""), ylab = paste("Dim ",axes[2]," (",signif(res.mca$eig[axes[2],2],4),"%)",sep=""), xlim = c(-1.1,1.1), ylim = c(-1.1,1.1), col = "white", asp=1, cex=cex)
      abline(v=0,lty=2, cex=cex)
      abline(h=0,lty=2, cex=cex)
      x.cercle <- seq(-1, 1, by = 0.01)
      y.cercle <- sqrt(1 - x.cercle^2)
      lines(x.cercle, y = y.cercle)
      lines(x.cercle, y = -y.cercle)
      for (v in 1:nrow(res.mca$quanti.sup$coord)) {
        arrows(0, 0, res.mca$quanti.sup$coord[v, 1], res.mca$quanti.sup$coord[v, 2], length = 0.1, angle = 15, code = 2)
        if (abs(res.mca$quanti.sup$coord[v,1])>abs(res.mca$quanti.sup$coord[v,2])){
          if (res.mca$quanti.sup$coord[v,1]>=0) pos<-4
          else pos<-2
        }
        else {
          if (res.mca$quanti.sup$coord[v,2]>=0) pos<-3
          else pos<-1
        }
        text(res.mca$quanti.sup$coord[v, 1], y = res.mca$quanti.sup$coord[v, 2], labels = rownames(res.mca$quanti.sup$coord)[v], pos = pos, cex=cex)
      }
    }
}
