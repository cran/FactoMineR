plot.CA <- function (x, axes = c(1, 2),
    xlim = NULL, ylim = NULL, invisible = NULL, col.row = "blue",
    col.col = "red", col.row.sup = "darkblue", col.col.sup = "darkred",
    label = "all", cex = 1, title = NULL, ...) {
    res.ca <- x
    if (!inherits(res.ca, "CA")) stop("non convenient data")
    lab.row <- lab.col <- lab.row.sup <- lab.col.sup <- FALSE
    if(length(label)==1 && label=="all") lab.row <- lab.col <- lab.row.sup <- lab.col.sup <- TRUE
    if("row" %in% label) lab.row<-TRUE
    if("col" %in% label) lab.col<-TRUE
    if("row.sup" %in% label) lab.row.sup<-TRUE
    if("col.sup" %in% label) lab.col.sup<-TRUE

    coord.col <- res.ca$col$coord[, axes]
    coord.row <- res.ca$row$coord[, axes]
    coord.row.sup <- coord.col.sup <- NULL
    if (!is.null(res.ca$row.sup)) coord.row.sup <- res.ca$row.sup$coord[, axes]
    if (!is.null(res.ca$col.sup)) coord.col.sup <- res.ca$col.sup$coord[, axes]

    test.invisible <- vector(length = 2)
    if (!is.null(invisible)) {
        test.invisible[1] <- match("row", invisible)
        test.invisible[2] <- match("col", invisible)
    }
    else  test.invisible <- rep(NA, 2)
    if (is.null(xlim)) {
      xmin <- xmax <- 0
      if(is.na(test.invisible[1])) xmin <- min(xmin, coord.row[,1], coord.row.sup[, 1])
      if(is.na(test.invisible[1])) xmax <- max(xmax, coord.row[,1], coord.row.sup[, 1])
      if(is.na(test.invisible[2])) xmin <- min(xmin, coord.col[,1], coord.col.sup[, 1])
      if(is.na(test.invisible[2])) xmax <- max(xmax, coord.col[,1], coord.col.sup[, 1])
        xlim <- c(xmin, xmax) * 1.2
    }
    else {
      xmin = xlim[1]
      xmax = xlim[2]
    }
    if (is.null(ylim)) {
      ymin <- ymax <- 0
      if(is.na(test.invisible[1])) ymin <- min(ymin, coord.row[,2], coord.row.sup[, 2])
      if(is.na(test.invisible[1])) ymax <- max(ymax, coord.row[,2], coord.row.sup[, 2])
      if(is.na(test.invisible[2])) ymin <- min(ymin, coord.col[,2], coord.col.sup[, 2])
      if(is.na(test.invisible[2])) ymax <- max(ymax, coord.col[,2], coord.col.sup[, 2])
        ylim <- c(ymin, ymax) * 1.2
    }
    else {
      ymin = ylim[1]
      ymax = ylim[2]
    }
    sub.titre <- NULL
    if (is.null(title)) titre <- "CA factor map"
    else {
      sub.titre <- "CA factor map"
      titre <- title
    }
    plot(0, 0, main = titre, xlab = paste("Dim ",axes[1]," (",signif(res.ca$eig[axes[1],2],4),"%)",sep=""), ylab = paste("Dim ",axes[2]," (",signif(res.ca$eig[axes[2],2],4),"%)",sep=""), xlim = xlim, ylim = ylim, col = "white", asp=1, cex=cex)
    if (!is.null(sub.titre)) title(sub = sub.titre, cex.sub = cex, font.sub = 2, col.sub = "steelblue4", adj = 0, line = 3.8)
    abline(h=0,lty=2)
    abline(v=0,lty=2)
    if (is.na(test.invisible[1])) {
      points(coord.row, pch = 20, col = col.row, cex = cex)
      if (lab.row)  text(coord.row[, 1], y = coord.row[, 2], labels = rownames(coord.row), pos = 3, col = col.row, cex = cex)
    }
    if (is.na(test.invisible[2])) {
      points(coord.col[, 1], y = coord.col[, 2], pch = 17, col = col.col, cex = cex)
      if (lab.col) text(coord.col[, 1], y = coord.col[, 2], labels = rownames(coord.col), pos = 3, col = col.col, cex = cex)
    }
    if (!is.null(res.ca$col.sup) & is.na(test.invisible[2])) {
      points(coord.col.sup[, 1], y = coord.col.sup[, 2], pch = 17, col = col.col.sup, cex = cex)
      if (lab.col.sup) text(coord.col.sup[, 1], y = coord.col.sup[, 2], labels = rownames(coord.col.sup), pos = 3, col = col.col.sup, cex = cex)
    }
    if (!is.null(res.ca$row.sup) & is.na(test.invisible[1])) {
      points(coord.row.sup[, 1], y = coord.row.sup[, 2], pch = 20, col = col.row.sup, cex = cex)
      if (lab.row.sup) text(coord.row.sup[, 1], y = coord.row.sup[, 2], labels = rownames(coord.row.sup), pos = 3, col = col.row.sup, cex = cex)
    }
}
