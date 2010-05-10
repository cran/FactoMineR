plot.MCA <- function (x, axes = c(1, 2), choix="ind",
    xlim = NULL, ylim = NULL, invisible = NULL, 
    col.ind = "blue", col.var = "red", col.quali.sup = "darkgreen",
    col.ind.sup = "darkblue", col.quanti.sup = "black",
    label="all", cex = 1, title = NULL, habillage = "none", palette=NULL, new.plot=TRUE, ...){
    
    res.mca <- x
    if (!inherits(res.mca, "MCA")) stop("non convenient data")

    if (is.null(palette)) palette(c("black","red","green3","blue","cyan","magenta","darkgray","darkgoldenrod","darkgreen","violet","turquoise","orange","lightpink","lavender","yellow","lightgreen","lightgrey","lightblue","darkkhaki", "darkmagenta","darkolivegreen","lightcyan", "darkorange", "darkorchid","darkred","darksalmon","darkseagreen","darkslateblue","darkslategray","darkslategrey","darkturquoise","darkviolet", "lightgray","lightsalmon","lightyellow", "maroon"))

   lab.x = paste("Dim ",axes[1]," (",signif(res.mca$eig[axes[1],2],4),"%)",sep="")
   lab.y = paste("Dim ",axes[2]," (",signif(res.mca$eig[axes[2],2],4),"%)",sep="")

   if (choix =="ind"){
    lab.ind <- lab.var <- lab.quali.sup <- lab.ind.sup <- FALSE
    if(length(label)==1 && label=="all") lab.ind <- lab.var <- lab.quali.sup <- lab.ind.sup <-TRUE
    if("ind" %in% label) lab.ind<-TRUE
    if("var" %in% label) lab.var<-TRUE
    if("quali.sup" %in% label) lab.quali.sup<-TRUE
    if("ind.sup" %in% label) lab.ind.sup<-TRUE

    test.invisible <- vector(length = 5)
    if (!is.null(invisible)) {
        test.invisible[1] <- match("ind", invisible)
        test.invisible[2] <- match("var", invisible)
        test.invisible[3] <- match("quanti.sup", invisible)
        test.invisible[4] <- match("ind.sup", invisible)
        test.invisible[5] <- match("quali.sup", invisible)
    }
    else  test.invisible <- rep(NA, 5)
    coord.var <- res.mca$var$coord[, axes]
    coord.ind <- res.mca$ind$coord[, axes]
    coord.ind.sup <- coord.quali.sup <- NULL
    if (!is.null(res.mca$ind.sup)) coord.ind.sup <- res.mca$ind.sup$coord[, axes]
    if (!is.null(res.mca$quali.sup)) coord.quali.sup <- res.mca$quali.sup$coord[, axes]
    if (is.null(xlim)) {
      xmin <- xmax <- 0
      if(is.na(test.invisible[1])) xmin <- min(xmin, coord.ind[,1])
      if(is.na(test.invisible[1])) xmax <- max(xmax, coord.ind[,1])
      if(is.na(test.invisible[4])) xmin <- min(xmin, coord.ind.sup[, 1])
      if(is.na(test.invisible[4])) xmax <- max(xmax, coord.ind.sup[, 1])
      if(is.na(test.invisible[2])) xmin <- min(xmin, coord.var[,1])
      if(is.na(test.invisible[2])) xmax <- max(xmax, coord.var[,1])
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
      if(is.na(test.invisible[1])) ymin <- min(ymin, coord.ind[,2])
      if(is.na(test.invisible[1])) ymax <- max(ymax, coord.ind[,2])
      if(is.na(test.invisible[4])) ymin <- min(ymin, coord.ind.sup[, 2])
      if(is.na(test.invisible[4])) ymax <- max(ymax, coord.ind.sup[, 2])
      if(is.na(test.invisible[2])) ymin <- min(ymin, coord.var[,2])
      if(is.na(test.invisible[2])) ymax <- max(ymax, coord.var[,2])
      if(is.na(test.invisible[5])) ymin <- min(ymin, coord.quali.sup[,2])
      if(is.na(test.invisible[5])) ymax <- max(ymax, coord.quali.sup[,2])
      ylim <- c(ymin, ymax) * 1.2
    }
    else {
      ymin = ylim[1]
      ymax = ylim[2]
    }

        if (habillage == "quali") {
          aux = 1
          col.var = NULL
          for (j in res.mca$call$quali) {
            col.var <- c(col.var,rep(aux,nlevels(res.mca$call$X[,j])))
            aux = aux + 1
          }
          if (!is.null(res.mca$call$quali.sup)){
            col.quali.sup = NULL
            for (j in res.mca$call$quali.sup) {
              col.quali.sup <- c(col.quali.sup,rep(aux,nlevels(res.mca$call$X[,j])))
              aux = aux + 1
            }
          }
        }
        if ((habillage != "none")&(habillage != "quali")) {
          if (!is.factor(res.mca$call$X[,habillage])) stop("The variable ", habillage, " is not qualitative")
          col.ind <- as.numeric(as.factor(res.mca$call$X[, habillage]))
          n.mod <- nlevels(as.factor(res.mca$call$X[, habillage]))
          col.ind.sup <- col.ind[res.mca$call$ind.sup]
          if (!is.null(res.mca$call$ind.sup)) col.ind <- col.ind[-res.mca$call$ind.sup]
        }

    
    titre = title
    if (is.null(title)) titre <- "MCA factor map"
    if (is.na(test.invisible[1])|is.na(test.invisible[2])|is.na(test.invisible[4])|is.na(test.invisible[5])) {
      if (new.plot) dev.new(width=min(14,max(8,8*(xmax-xmin)/(ymax-ymin))),height=8)
      plot(0, 0, main = titre, xlab = lab.x, ylab = lab.y, xlim = xlim, ylim = ylim, col = "white", asp=1, cex=cex)
      abline(v=0,lty=2, cex=cex)
      abline(h=0,lty=2, cex=cex)
      if (is.na(test.invisible[1])) {
          points(coord.ind, pch = 16, col = col.ind, cex=cex)
          if (lab.ind) text(coord.ind[, 1], y = coord.ind[, 2], labels = rownames(coord.ind), pos = 3, col = col.ind, cex=cex)
      }
      if (is.na(test.invisible[2])) {
          points(coord.var[, 1], y = coord.var[, 2], pch = 17, col = col.var, cex=cex)
          if (lab.var) text(coord.var[, 1], y = coord.var[, 2], labels = rownames(coord.var), pos = 3, col = col.var, cex=cex)
      }
      if (!is.null(res.mca$quali.sup) & is.na(test.invisible[5])) {
          points(coord.quali.sup[, 1], y = coord.quali.sup[, 2], pch = 17, col = col.quali.sup, cex=cex)
          if (lab.quali.sup) text(coord.quali.sup[, 1], y = coord.quali.sup[, 2], labels = rownames(coord.quali.sup), pos = 3, col = col.quali.sup, cex=cex)
      }
      if (!is.null(res.mca$ind.sup) & is.na(test.invisible[4])) {
          points(coord.ind.sup[, 1], y = coord.ind.sup[, 2], pch = 16, col = col.ind.sup, cex=cex)
          if (lab.ind.sup) text(coord.ind.sup[, 1], y = coord.ind.sup[, 2], labels = rownames(coord.ind.sup), pos = 3, col = col.ind.sup, cex=cex)
      }
      if ((habillage != "none")&(habillage != "quali")&(is.na(test.invisible[1])|is.na(test.invisible[2])))  legend("topleft",legend= levels(res.mca$call$X[,habillage]),text.col= 1:n.mod,cex=0.8)
    }
   }
    
    if (choix == "quanti.sup") {
     if (!is.null(res.mca$quanti.sup)) {
      if (new.plot) dev.new()
	  if (is.null(title)) title <- "Supplementary variables on the MCA factor map"
      plot(0, 0, main = title, xlab = paste("Dim ",axes[1]," (",signif(res.mca$eig[axes[1],2],4),"%)",sep=""), ylab = paste("Dim ",axes[2]," (",signif(res.mca$eig[axes[2],2],4),"%)",sep=""), xlim = c(-1.1,1.1), ylim = c(-1.1,1.1), col = "white", asp=1, cex=cex)
      abline(v=0,lty=2, cex=cex)
      abline(h=0,lty=2, cex=cex)
      x.cercle <- seq(-1, 1, by = 0.01)
      y.cercle <- sqrt(1 - x.cercle^2)
      lines(x.cercle, y = y.cercle)
      lines(x.cercle, y = -y.cercle)
      for (v in 1:nrow(res.mca$quanti.sup$coord)) {
        arrows(0, 0, res.mca$quanti.sup$coord[v, axes[1]], res.mca$quanti.sup$coord[v, axes[2]], length = 0.1, angle = 15, code = 2, col = col.quanti.sup)
        if (abs(res.mca$quanti.sup$coord[v,axes[1]])>abs(res.mca$quanti.sup$coord[v,axes[2]])){
          if (res.mca$quanti.sup$coord[v,axes[1]]>=0) pos<-4
          else pos<-2
        }
        else {
          if (res.mca$quanti.sup$coord[v,axes[2]]>=0) pos<-3
          else pos<-1
        }
		if((!is.null(label)) && (label=="all" | "quanti.sup" %in% label)){
        	text(res.mca$quanti.sup$coord[v, axes[1]], y = res.mca$quanti.sup$coord[v, axes[2]], labels = rownames(res.mca$quanti.sup$coord)[v], pos = pos, cex=cex, col = col.quanti.sup)
		}
      }
    }
    }

    
    if (choix == "var") {
      lab.var <- lab.quali.sup <- FALSE
      if(length(label)==1 && label=="all") lab.var <- lab.quali.sup <-TRUE
      if("var" %in% label) lab.var<-TRUE
      if("quali.sup" %in% label) lab.quali.sup<-TRUE

      test.invisible <- vector(length = 2)
      if (!is.null(invisible)) {
          test.invisible[1] <- match("var", invisible)
          test.invisible[2] <- match("quali.sup", invisible)
      }
      else  test.invisible <- rep(NA, 2)

      if (new.plot) dev.new()
      if (is.null(title)) title <- "Variables representation"
      coord.actif <- res.mca$var$eta2[, axes]
      if (!is.null(res.mca$quali.sup$eta2)) coord.illu <- res.mca$quali.sup$eta2[,axes,drop=FALSE]
      if (is.na(test.invisible[1])){
        plot(coord.actif, xlab = lab.x, ylab = lab.y, xlim = c(0, 1), ylim = c(0, 1), pch = 20, col = col.var, cex = cex, main = title, cex.main = cex*1.2, asp = 1)
        if (lab.var) text(coord.actif[, 1], y = coord.actif[, 2], labels = rownames(coord.actif), pos = 3, col = col.var)
      }
      if ((!is.null(res.mca$quali.sup$eta2))&&(is.na(test.invisible[2]))){
        if (!is.na(test.invisible[1])) plot(coord.illu, xlab = lab.x, ylab = lab.y, xlim = c(0, 1), ylim = c(0, 1), pch = 20, col = col.quali.sup, cex = cex, main = title, cex.main = cex*1.2, asp = 1)
        else points(coord.illu, pch = 20, col = col.quali.sup)
        if (lab.quali.sup) text(coord.illu[, 1], y = coord.illu[, 2], labels = rownames(coord.illu), pos = 3, col = col.quali.sup)
      }
    }
    
}
