plot.CA <- function (x, axes = c(1, 2),
    xlim = NULL, ylim = NULL, invisible = NULL, choix = "CA", col.row = "blue",
    col.col = "red", col.row.sup = "darkblue", col.col.sup = "darkred",col.quali.sup ="magenta",
    col.quanti.sup="blue",label = "all", cex = 1, title = NULL, palette=NULL, new.plot=FALSE, ...) {
    res.ca <- x
    if (is.null(palette)) palette(c("black","red","green3","blue","cyan","magenta","darkgray","darkgoldenrod","darkgreen","violet","turquoise","orange","lightpink","lavender","yellow","lightgreen","lightgrey","lightblue","darkkhaki", "darkmagenta","darkolivegreen","lightcyan", "darkorange", "darkorchid","darkred","darksalmon","darkseagreen","darkslateblue","darkslategray","darkslategrey","darkturquoise","darkviolet", "lightgray","lightsalmon","lightyellow", "maroon"))
    if (!inherits(res.ca, "CA")) stop("non convenient data")
choix <- tolower(choix)
if (choix=="ca"){
    lab.row <- lab.col <- lab.row.sup <- lab.col.sup <- FALSE
    if(length(label)==1 && label=="all") lab.row <- lab.col <- lab.row.sup <- lab.col.sup <- lab.quali.sup <- TRUE
    if("row" %in% label) lab.row<-TRUE
    if("col" %in% label) lab.col<-TRUE
    if("row.sup" %in% label) lab.row.sup<-TRUE
    if("col.sup" %in% label) lab.col.sup<-TRUE

    coord.col <- res.ca$col$coord[, axes]
    coord.row <- res.ca$row$coord[, axes]
    coord.row.sup <- coord.col.sup <- coord.quali.sup <- NULL
    if (!is.null(res.ca$row.sup)) coord.row.sup <- res.ca$row.sup$coord[, axes,drop=FALSE]
    if (!is.null(res.ca$col.sup)) coord.col.sup <- res.ca$col.sup$coord[, axes,drop=FALSE]
    if (!is.null(res.ca$quali.sup)) coord.quali.sup <- res.ca$quali.sup$coord[, axes,drop=FALSE]

    test.invisible <- vector(length = 4)
    if (!is.null(invisible)) {
        test.invisible[1] <- match("row", invisible)
        test.invisible[2] <- match("col", invisible)
        test.invisible[3] <- match("row.sup", invisible)
        test.invisible[4] <- match("col.sup", invisible)
        test.invisible[5] <- match("quali.sup", invisible)
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
      if(is.na(test.invisible[5])) xmin <- min(xmin, coord.quali.sup[, 1])
      if(is.na(test.invisible[5])) xmax <- max(xmax, coord.quali.sup[, 1])
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
      if(is.na(test.invisible[5])) ymin <- min(ymin, coord.quali.sup[,2])
      if(is.na(test.invisible[5])) ymax <- max(ymax, coord.quali.sup[,2])
        ylim <- c(ymin, ymax) * 1.2
    }
    else {
      ymin = ylim[1]
      ymax = ylim[2]
    }
    if (is.null(title)) titre <- "CA factor map"
    else titre <- title
    plot(0, 0, main = titre, xlab = paste("Dim ",axes[1]," (",signif(res.ca$eig[axes[1],2],4),"%)",sep=""), ylab = paste("Dim ",axes[2]," (",signif(res.ca$eig[axes[2],2],4),"%)",sep=""), xlim = xlim, ylim = ylim, col = "white", asp=1, cex=cex)
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
    if (!is.null(res.ca$col.sup) & is.na(test.invisible[4])) {
      points(coord.col.sup[, 1], y = coord.col.sup[, 2], pch = 17, col = col.col.sup, cex = cex)
      if (lab.col.sup) text(coord.col.sup[, 1], y = coord.col.sup[, 2], labels = rownames(coord.col.sup), pos = 3, col = col.col.sup, cex = cex)
    }
    if (!is.null(res.ca$row.sup) & is.na(test.invisible[3])) {
      points(coord.row.sup[, 1], y = coord.row.sup[, 2], pch = 20, col = col.row.sup, cex = cex)
      if (lab.row.sup) text(coord.row.sup[, 1], y = coord.row.sup[, 2], labels = rownames(coord.row.sup), pos = 3, col = col.row.sup, cex = cex)
    }
    if (!is.null(res.ca$quali.sup) & is.na(test.invisible[5])) {
      points(coord.quali.sup[, 1], y = coord.quali.sup[, 2], pch = 22, col = col.quali.sup, cex = cex)
      if (lab.quali.sup) text(coord.quali.sup[, 1], y = coord.quali.sup[, 2], labels = rownames(coord.quali.sup), pos = 3, col = col.quali.sup, cex = cex)
    }
}
	
	if (choix == "quanti.sup") {
     if (!is.null(res.ca$quanti.sup)) {
      if (new.plot) dev.new()
	  if (is.null(title)) title <- "Supplementary variables on the CA factor map"
      plot(0, 0, main = title, xlab = paste("Dim ",axes[1]," (",signif(res.ca$eig[axes[1],2],4),"%)",sep=""), ylab = paste("Dim ",axes[2]," (",signif(res.ca$eig[axes[2],2],4),"%)",sep=""), xlim = c(-1.1,1.1), ylim = c(-1.1,1.1), col = "white", asp=1, cex=cex)
      abline(v=0,lty=2, cex=cex)
      abline(h=0,lty=2, cex=cex)
      x.cercle <- seq(-1, 1, by = 0.01)
      y.cercle <- sqrt(1 - x.cercle^2)
      lines(x.cercle, y = y.cercle)
      lines(x.cercle, y = -y.cercle)
      for (v in 1:nrow(res.ca$quanti.sup$coord)) {
        arrows(0, 0, res.ca$quanti.sup$coord[v, axes[1]], res.ca$quanti.sup$coord[v, axes[2]], length = 0.1, angle = 15, code = 2, col = col.quanti.sup)
        if (abs(res.ca$quanti.sup$coord[v,axes[1]])>abs(res.ca$quanti.sup$coord[v,axes[2]])){
          if (res.ca$quanti.sup$coord[v,axes[1]]>=0) pos<-4
          else pos<-2
        }
        else {
          if (res.ca$quanti.sup$coord[v,axes[2]]>=0) pos<-3
          else pos<-1
        }
		if((!is.null(label)) && (label=="all" | "quanti.sup" %in% label)){
        	text(res.ca$quanti.sup$coord[v, axes[1]], y = res.ca$quanti.sup$coord[v, axes[2]], labels = rownames(res.ca$quanti.sup$coord)[v], pos = pos, cex=cex, col = col.quanti.sup)
		}
      }
    }
    }

}
