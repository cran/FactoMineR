#! plot.FDA


plot.FDA<-function(x, choix="ind", axes=c(1,2), invisible=NULL, xlim=NULL, ylim=NULL, col.grpe=NULL, 
lab.ind=FALSE, lab.var=TRUE, lab.cg=TRUE, lab.ind.sup=FALSE, conf.elli=0.95, ...){

    res.fda <- x
    if(!inherits(res.fda, "FDA"))    stop("non convinient data")

    # titre du graphique
    if (axes[2]!=axes[1])
    {
        titre<-paste("Plan factoriel", axes[1], "-", axes[2])
    ylab<-""
    }
    else
    {
        titre<-paste("Fonction discriminante", axes[1])
        ylab<-"dispersion aleatoire"
    }

    # graphique des individus
    if (choix=="ind")
    {
    
      # detection des éléments à ne pas représenter
        test.invisible<-vector(length=3)
        if(!is.null(invisible))
        {
            test.invisible[1]<-match("cg", invisible)
            test.invisible[2]<-match("ind", invisible)
            test.invisible[3]<-match("elli", invisible)
            test.invisible[4]<-match("ind.sup", invisible)
        }
        else
        {
            test.invisible<-rep(NA,4)
        }

    
        #récupérations de toutes les coordonnées
        if (axes[2]!=axes[1])
      {
            coord.cg<-res.fda$res.cg$coord[, axes]
            coord.ind<-res.fda$res.ind$coord[,axes]
      tab.elli<-data.frame(res.fda$call$fact, coord.ind)
            coord.elli<-coord.ellipse(tab.elli, conf=conf.elli)$res
        coord.ind.sup<-res.fda$coord.ind.sup[,axes]
        }

        else
        {
      coord.cg<-data.frame(res.fda$res.cg$coord.cg[, axes[1]], rnorm(nrow(res.fda$res.cg$coord.cg), mean=0, sd=1))
            coord.ind<-data.frame(res.fda$res.ind$coord.ind[,axes[1]], rnorm(nrow(res.fda$res.ind$coord.ind), mean=0, sd=1))
      coord.elli<-NULL
      if (!is.null(res.fda$coord.ind.sup))
            {
                coord.ind.sup<-data.frame(res.fda$coord.ind.sup[,axes[1]], rnorm(nrow(res.fda$coord.ind.sup), mean=0, sd=1))
            }
            else
            {
                coord.ind.sup<-NULL
            }
    }

        # determination des limites
        if(is.null(xlim))
        {
      xmin<-xmax<-0
      if(is.na(test.invisible[1]))
      {
        xmin<-min(xmin, coord.cg[,1])
        xmax<-max(xmax, coord.cg[,1])
      }
      if(is.na(test.invisible[2]))
      {
        xmin<-min(xmin, coord.ind[,1])
        xmax<-max(xmax, coord.ind[,1])
      }
      if(is.na(test.invisible[3]))
      {
        xmin<-min(xmin, coord.elli[,2])
        xmax<-max(xmax, coord.elli[,2])
      }
      if(is.na(test.invisible[4]))
      {
        xmin<-min(xmin, coord.ind.sup[,1])
        xmax<-max(xmax, coord.ind.sup[,1])
      }
        xlim<-c(xmin, xmax)*1.2
        }
        
        if(is.null(ylim))
        {
      ymin<-ymax<-0
      if(is.na(test.invisible[1]))
      {
        ymin<-min(ymin, coord.cg[,2])
        ymax<-max(ymax, coord.cg[,2])
      }
      if(is.na(test.invisible[2]))
      {
        ymin<-min(ymin, coord.ind[,2])
        ymax<-max(ymax, coord.ind[,2])
      }
      if(is.na(test.invisible[3]))
      {
        ymin<-min(ymin, coord.elli[,3])
        ymax<-max(ymax, coord.elli[,3])
      }
      if(is.na(test.invisible[4]))
      {
        ymin<-min(ymin, coord.ind.sup[,2])
        ymax<-max(ymax, coord.ind.sup[,2])
      }
        ylim<-c(ymin, ymax)*1.2
        }

        # determination de l'habillage des points

        n.grpe<-nlevels(res.fda$call$fact)
        if (is.null(col.grpe))
        {
            col.grpe<-c(1:n.grpe)
        }
        col.ind<-res.fda$call$fact
        levels(col.ind)<-col.grpe
        col.ind<-as.character(col.ind)
        if (!is.null(coord.ind.sup))
        {
            col.ind.sup<-col.ind.sup.tmp<-res.fda$predict
            for (i in 1: nlevels(res.fda$call$fact))
            {
                col.ind.sup[col.ind.sup.tmp==levels(res.fda$call$fact)[i]]<-col.grpe[i]
            }
        }


        # construction graphique
        plot(0,0,main="Représentation des individus", sub=titre, cex.main=1, xaxt="n", xlab="", yaxt="n", ylab=ylab , xlim=xlim, ylim=ylim, col="white")
        axis(1,at=axTicks(1)[axTicks(1)!=0], pos=0)
        axis(2, at=axTicks(2)[axTicks(2)!=0], pos=0)
        segments(xlim[1]*2, 0, xlim[2]*2, 0)
        segments(0, ylim[1]*2, 0, ylim[2]*2)

        if(is.na(test.invisible[1]))
        {
            points(coord.cg, cex=1.5, pch=17, col=col.grpe)
            if (lab.cg)
            {
                text(coord.cg[,1], y=coord.cg[,2],labels=rownames(coord.cg), pos=3, col=col.grpe, cex=1.3)
            }
        }

        if(is.na(test.invisible[2]))
        {
            points(coord.ind, pch=16, col=col.ind)
            if (lab.ind)
            {
        text(coord.ind[,1], y=coord.ind[,2],labels=rownames(coord.ind), pos=3, col=col.ind)
        }
        }

        if (!is.null(coord.elli) & is.na(test.invisible[3]))
        {
            for ( i in 1: n.grpe)
            {
                ii<-levels(res.fda$call$fact)[i]
                tmp<-coord.elli[coord.elli[,1]==ii,-1]
                lines(tmp[,1], y=tmp[,2], col=col.grpe[i])
            }
        }

        if (!is.null(coord.ind.sup) & is.na(test.invisible[4]))
        {
            points(coord.ind.sup, pch=1, col=col.ind.sup)
            if (lab.ind.sup)
            {
            text(coord.ind.sup[,1], y=coord.ind.sup[,2],labels=rownames(coord.ind.sup), pos=3, col=col.ind.sup)
            }
        }
    }
    ############## fin graphique des individus #################


    # graphique des variables
    if (choix=="var")
    {
        #récupérations des coordonnées
        coord.var<-res.fda$res.var$coord[,axes]

        #construction graphique
    get(getOption("device"))(8, 8)
        par(mar=c(2,2,2,2))
        plot(0,0,main=titre, xaxt="n", xlab="", yaxt="n", ylab="" , xlim=c(-1.3, 1.3), ylim=c(-1.3, 1.3), col="white", bty="n")
        x.cercle<-seq(-1,1,by=0.01)
        y.cercle<-sqrt(1-x.cercle^2)
        lines(x.cercle, y=y.cercle)
        lines(x.cercle, y=-y.cercle)
        segments(-1, 0, 1, 0)
        segments(0, -1, 0, 1)

        for ( v in 1: nrow(coord.var))
        {
            arrows(0,0,coord.var[v,1], coord.var[v,2], length=0.1, angle=15, code=2)
            if(lab.var)
            {
                if (coord.var[v,2]>=0) pos<-3
                else pos<-1
                text(coord.var[v,1], y=coord.var[v,2], labels=rownames(coord.var)[v],pos=pos)
            }
        }

        par(mar=c(5, 4, 4, 2) +0.1)
    }
}
