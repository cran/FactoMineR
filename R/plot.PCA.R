plot.PCA <- function (x, axes = c(1, 2), choix = "ind",
    ellipse = NULL, xlim = NULL, ylim = NULL, habillage = "none", 
    col.hab = NULL, col.ind = "black", col.ind.sup = "blue", 
    col.quali = "magenta", col.quanti.sup = "blue", 
    col.var = "black", label="all", invisible = NULL, lim.cos2.var = 0.,
    cex = 1, title = NULL, palette=NULL, new.plot=TRUE, ...){
    
    res.pca <- x
    if (!inherits(res.pca, "PCA")) stop("non convenient data")
    if (is.null(palette)) palette(c("black","red","green3","blue",      "cyan","magenta","darkgray","darkgoldenrod","darkgreen","violet","turquoise","orange","lightpink","lavender","yellow","lightgreen","lightgrey","lightblue","darkkhaki", "darkmagenta","darkolivegreen","lightcyan", "darkorange", "darkorchid","darkred","darksalmon","darkseagreen","darkslateblue","darkslategray","darkslategrey","darkturquoise","darkviolet", "lightgray","lightsalmon","lightyellow", "maroon"))
    lab.ind <- lab.quali <- lab.var <- lab.quanti <- lab.ind.sup <- FALSE
    if(length(label)==1 && label=="all") lab.ind <- lab.quali <- lab.var <- lab.quanti <- lab.ind.sup <-TRUE
    if("ind" %in% label) lab.ind<-TRUE
    if("quali" %in% label) lab.quali<-TRUE
    if("var" %in% label) lab.var<-TRUE
    if("quanti.sup" %in% label) lab.quanti<-TRUE
    if("ind.sup" %in% label) lab.ind.sup<-TRUE
    cp1 <- round((res.pca$eig[axes[1],2]), digit = 2)
    cp2 <- round((res.pca$eig[axes[2],2]), digit = 2)
    lab.x <- paste("Dim ",axes[1]," (",cp1,"%)",sep="")
    lab.y <- paste("Dim ",axes[2]," (",cp2,"%)",sep="")
    plan <- cp1 + cp2
#    sub.titre <- NULL
    if (choix == "ind") {
        if (is.null(title)) titre <- "Individuals factor map (PCA)"
        else titre <- title
        coord.actif <- res.pca$ind$coord[, axes]
        coord.illu <- coord.quali <- coord.ellipse <- NULL
        if (!is.null(res.pca$ind.sup)) coord.illu <- res.pca$ind.sup$coord[, axes]
        if (!is.null(res.pca$quali.sup))  coord.quali <- res.pca$quali.sup$coord[, axes]
        if (!is.null(ellipse))  coord.ellipse <- ellipse$res

        test.invisible <- vector(length = 2)
        if (!is.null(invisible)) {
            test.invisible[1] <- match("ind", invisible)
            test.invisible[2] <- match("ind.sup", invisible)
            test.invisible[3] <- match("quali", invisible)
        }
        else  test.invisible <- rep(NA, 3)
        if (is.null(xlim)) {
          xmin <- xmax <- 0
          if(is.na(test.invisible[1])) xmin <- min(xmin, coord.actif[,1])
          if(is.na(test.invisible[1])) xmax <- max(xmax, coord.actif[,1])
          if(!is.null(coord.illu)&is.na(test.invisible[2])) xmin <- min(xmin, coord.illu[, 1])
          if(!is.null(coord.illu)&is.na(test.invisible[2])) xmax <- max(xmax, coord.illu[, 1])
          if(!is.null(coord.quali)&is.na(test.invisible[3])) xmin <- min(xmin, coord.quali[, 1])
          if(!is.null(coord.quali)&is.na(test.invisible[3])) xmax <- max(xmax, coord.quali[, 1])
          if(!is.null(coord.ellipse)&is.na(test.invisible[1])) xmin <- min(xmin, coord.ellipse[, 2])
          if(!is.null(coord.ellipse)&is.na(test.invisible[1])) xmax <- max(xmax, coord.ellipse[, 2])
          xlim <- c(xmin, xmax) * 1.2
        }
        else {
          xmin = xlim[1]
          xmax = xlim[2]
        }
        if (is.null(ylim)) {
          ymin <- ymax <- 0
          if(is.na(test.invisible[1])) ymin <- min(ymin, coord.actif[,2])
          if(is.na(test.invisible[1])) ymax <- max(ymax, coord.actif[,2])
          if(!is.null(coord.illu)&is.na(test.invisible[2])) ymin <- min(ymin, coord.illu[, 2])
          if(!is.null(coord.illu)&is.na(test.invisible[2])) ymax <- max(ymax, coord.illu[, 2])
          if(!is.null(coord.quali)&is.na(test.invisible[3])) ymin <- min(ymin, coord.quali[, 2])
          if(!is.null(coord.quali)&is.na(test.invisible[3])) ymax <- max(ymax, coord.quali[, 2])
          if(!is.null(coord.ellipse)&is.na(test.invisible[1])) ymin <- min(ymin, coord.ellipse[, 3])
          if(!is.null(coord.ellipse)&is.na(test.invisible[1])) ymax <- max(ymax, coord.ellipse[, 3])
          ylim <- c(ymin, ymax) * 1.2
        }
        else {
          ymin = ylim[1]
          ymax = ylim[2]
        }
       if (new.plot) dev.new(width=min(14,8*(xmax-xmin)/(ymax-ymin)),height=8)
        if (habillage == "ind") {
            nb.prod <- nrow(coord.actif)
            if (length(col.hab) != nb.prod) color.ind <- c(1:nb.prod)
            else  color.ind <- col.hab
            if (!is.null(coord.illu)) color.ind.sup <- c((nb.prod+1):(nb.prod+nrow(coord.illu)))
            color.mod <- "darkred"
        }
        if ((habillage != "none")&(habillage != "ind")) {
            liste.quali <- colnames(res.pca$call$quali.sup$quali.sup)
            nom.quali <- colnames(res.pca$call$X)[habillage]
            if (!(nom.quali %in% liste.quali)) stop("The variable", habillage, "is not qualitative")
            n.mod <- res.pca$call$quali.sup$modalite[liste.quali == nom.quali]
            if (length(col.hab) != n.mod) {
                color.mod <- c(1:n.mod)
                color.ind <- as.numeric(as.factor(res.pca$call$X[, nom.quali]))
                col.ind.sup <- color.ind[res.pca$call$ind.sup]
                if (!is.null(res.pca$call$ind.sup)) color.ind <- color.ind[-res.pca$call$ind.sup]
            }
            else {
                color.mod <- col.hab
                color.ind <- as.factor(res.pca$call$X[, nom.quali])
                levels(color.ind) <- col.hab
                col.ind.sup <- color.ind[res.pca$call$ind.sup]
                if (!is.null(res.pca$call$ind.sup)) color.ind <- color.ind[-res.pca$call$ind.sup]
                color.ind <- as.character(color.ind)
            }
        }
        if (habillage == "none") {
            color.ind <- col.ind
            color.mod <- col.quali
        }
        color.sup <- col.ind.sup

        plot(0, 0, main = titre, xlab = lab.x, ylab = lab.y, xlim = xlim, ylim = ylim, col = "white", asp=1, cex=cex)
        abline(v=0,lty=2, cex=cex)
        abline(h=0,lty=2, cex=cex)
        if (is.na(test.invisible[1])) {
            points(coord.actif, pch = 20, col = color.ind, cex=cex)
            if (lab.ind) text(coord.actif[, 1], y = coord.actif[, 2], labels = rownames(coord.actif), pos = 3, col = color.ind, cex=cex)
        }
        if (!is.null(res.pca$ind.sup) & is.na(test.invisible[2])) {
            points(res.pca$ind.sup$coord[, axes[1]],res.pca$ind.sup$coord[, axes[2]], col = as.character(color.sup), cex=cex)
            if (lab.ind.sup)  text(res.pca$ind.sup$coord[, axes[1]], y = res.pca$ind.sup$coord[, axes[2]], labels = rownames(res.pca$ind.sup$coord), pos = 3, col = as.character(color.sup), cex=cex)
        }
        if (!is.null(coord.quali) & is.na(test.invisible[3])) {
            num.li <- 0
            modalite <- res.pca$call$quali.sup$modalite
            col.quali<-rep(col.quali, length(modalite))
            for (q in 1:length(modalite)) {
                if ((habillage != "none")&(habillage != "ind")) {
                  if (q == match(nom.quali, liste.quali)) {
                    points(coord.quali[(num.li + 1):(num.li + modalite[q]), ], pch = 22, col = color.mod, cex=cex)
                    if (lab.quali) {
                      text(coord.quali[(num.li + 1):(num.li + modalite[q]), 1], y = coord.quali[(num.li +
                        1):(num.li + modalite[q]), 2], labels = rownames(coord.quali[(num.li +
                        1):(num.li + modalite[q]), ]), pos = 3, col = color.mod, cex=cex)
                    }
                  }
                }
                else {
                  points(coord.quali[(num.li + 1):(num.li + modalite[q]), ], pch = 22, col = col.quali[q])
                  if (lab.quali) {
                    text(coord.quali[(num.li + 1):(num.li + modalite[q]), 1],
                      y = coord.quali[(num.li + 1):(num.li +
                      modalite[q]), 2], labels = rownames(coord.quali[(num.li +
                      1):(num.li + modalite[q]), ]), pos = 3, col = col.quali[q], cex=cex)
                  }
                }
                num.li <- num.li + modalite[q]
            }
        }
        if (!is.null(ellipse)) {
          nbre.ellipse <- nlevels(coord.ellipse[, 1])
          for (e in 1:nbre.ellipse) {
            data.elli <- coord.ellipse[ellipse$res[, 1] == levels(coord.ellipse[, 1])[e], -1]
            if ((habillage != "none")&(habillage != "ind")) lines(x=data.elli[, 1], y = data.elli[, 2], col = color.mod[e])
        else lines(x=data.elli[, 1], y = data.elli[, 2], col = col.quali)
          }
        }
        if ((habillage != "none")&(habillage != "ind")) legend("topleft",legend= levels(res.pca$call$X[,habillage]),text.col= color.mod,cex=0.8)
    }
    if (choix == "var") {
        if (is.null(title)) titre <- "Variables factor map (PCA)"
        else titre <- title
        test.invisible <- vector(length = 2)
        if (!is.null(invisible)) {
            test.invisible[1] <- match("var", invisible)
            test.invisible[2] <- match("quanti.sup", invisible)
        }
        else  test.invisible <- rep(NA, 2)
        scale.unit <- res.pca$call$scale.unit
        coord.var <- res.pca$var$coord[, axes]
        if (!is.null(res.pca$quanti.sup))  coord.quanti <- res.pca$quanti.sup$coord[, axes]
        else coord.quanti <- NULL
        if (scale.unit)  xlim <- ylim <- c(-1, 1)
        else {
            xmin <- min(0,coord.var[, 1], coord.quanti[, 1])
            xmax <- max(0,coord.var[, 1], coord.quanti[, 1])
            ymin <- min(0,coord.var[, 2], coord.quanti[, 2])
            ymax <- max(0,coord.var[, 2], coord.quanti[, 2])
            xlim <- c(xmin, xmax) * 1.2
            ylim <- c(ymin, ymax) * 1.2
        }
        if (new.plot) dev.new()
        if (scale.unit) {
            plot(0, 0, xlab = lab.x, ylab = lab.y, xlim = xlim, ylim = ylim, col = "white", asp=1, cex=cex, main=titre)
            x.cercle <- seq(-1, 1, by = 0.01)
            y.cercle <- sqrt(1 - x.cercle^2)
            lines(x.cercle, y = y.cercle)
            lines(x.cercle, y = -y.cercle)
             abline(v=0,lty=2, cex=cex)
             abline(h=0,lty=2, cex=cex)
        }
        else {
            plot(0, 0, main = titre, xlab = lab.x, ylab = lab.y, xlim = xlim, ylim = ylim, col = "white", asp=1, cex=cex)
             abline(v=0,lty=2)
             abline(h=0,lty=2)
        }
        col.var<-rep(col.var, length=nrow(coord.var))
        if (is.na(test.invisible[1])){
          for (v in 1:nrow(coord.var)) {
            if (sum(res.pca$var$cos2[v, axes], na.rm = TRUE) >= lim.cos2.var && !is.na(sum(res.pca$var$cos2[v, axes], na.rm = TRUE))) {
                arrows(0, 0, coord.var[v, 1], coord.var[v, 2], length = 0.1, angle = 15, code = 2, col = col.var[v])
                if (lab.var) {
                if (abs(coord.var[v,1])>abs(coord.var[v,2])){
                 if (coord.var[v,1]>=0) pos<-4
                 else pos<-2
                }
                else {
                 if (coord.var[v,2]>=0) pos<-3
                 else pos<-1
                }
                  text(coord.var[v, 1], y = coord.var[v, 2], labels = rownames(coord.var)[v], pos = pos, cex=cex, col = col.var[v])
                }
            }
          }
        }
        if (!is.null(coord.quanti)& is.na(test.invisible[2])) {
            col.quanti.sup<-rep(col.quanti.sup, length=nrow(coord.quanti))
            for (q in 1:nrow(coord.quanti)) {
                arrows(0, 0, coord.quanti[q, 1], coord.quanti[q, 2], length = 0.1, angle = 15, code = 2, lty = 2, col=col.quanti.sup[q])
                if (lab.quanti) {
                if (abs(coord.quanti[q,1])>abs(coord.quanti[q,2])){
                 if (coord.quanti[q,1]>=0) pos<-4
                 else pos<-2
                }
                else {
                 if (coord.quanti[q,2]>=0) pos<-3
                 else pos<-1
                }
                  text(coord.quanti[q, 1], y = coord.quanti[q, 2], labels = rownames(coord.quanti)[q], pos = pos, cex=cex,col=col.quanti.sup[q])
                }
            }
        }
        par(mar = c(5, 4, 4, 2) + 0.1)
    }
}
