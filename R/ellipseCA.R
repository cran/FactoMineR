ellipseCA <- function(x, ellipse=c("col","row"), method="multinomial", nbsample=100,
                      axes=c(1,2), xlim=NULL, ylim=NULL, col.row="blue", col.col="red",
                      col.row.ell=col.row, col.col.ell=col.col, graph.type = c("ggplot","classic"), ggoptions = NULL, ...){
  
  X <- x$call$X
  argument <- list(...)
  if (!is.null(argument[["cex"]]) & is.null(ggoptions["size"]))  ggoptions["size"] <- 4*argument$cex
  ggoptions_default <- list(size = 4, point.shape = 19, line.lty = 2, line.lwd = 0.5, line.color = "black", segment.lty = 1, segment.lwd = 0.5, circle.lty = 1, circle.lwd = 0.5, circle.color = "black", low.col.quanti = "blue", high.col.quanti = "red3")
  if (!is.null(ggoptions[1])) ggoptions_default[names(ggoptions)] <- ggoptions[names(ggoptions)]
  concCol<-concRow<-X
  graph.type <- match.arg(graph.type[1],c("ggplot","classic"))
  if (method=="multinomial"){
    proba <- unlist(c(X))
    N<-sum(proba)
    proba <- proba/N
    aa<-rmultinom(nbsample, size = N, prob = proba)
    for (i in 1:nbsample) {
      aux <- matrix(aa[,i,drop=FALSE],nrow=nrow(X))
      dimnames(aux)<-dimnames(X)
      if ("col"%in%ellipse) concCol <- cbind.data.frame(concCol,aux)
      if ("row"%in%ellipse) concRow <- rbind.data.frame(concRow,aux)
    }
  }
  if (method=="boot"){   ## botstrap  of the values by rows
    aux <- X
    ni <- apply(X,1,sum)
    for (i in 1:nbsample) {
      for (ri in 1:nrow(X)) aux[ri,] <- summary(cut(sample(1:ni[ri],ni[ri],replace=TRUE),c(0,cumsum(t(X[ri,])))+0.1/ni[ri]*(0:ncol(X))),maxsum=Inf)
      if ("col"%in%ellipse) concCol <- cbind.data.frame(concCol,aux)
      if ("row"%in%ellipse) concRow <- rbind.data.frame(concRow,aux)
    } 
  }
  if (length(col.row)==1) col.row <- rep(col.row,nrow(X))
  if (length(col.col)==1) col.col <- rep(col.col,ncol(X))
  if (length(col.col.ell)==1) col.col.ell <- rep(col.col.ell,ncol(X))
  if (length(col.row.ell)==1) col.row.ell <- rep(col.row.ell,ncol(X))
  if ("col"%in%ellipse){
    colCA <- CA(concCol,col.sup=(ncol(X)+1):ncol(concCol),graph=FALSE)
    aux3 <- colCA$col.sup$coord[,axes]
    rownames(aux3) <- paste("r",1:nrow(aux3),sep="")
    aux1 <- cbind.data.frame(paste("col",1:ncol(X),sep=""),aux3)
	aux1[,1] <- as.factor(aux1[,1])
    ellCol <- coord.ellipse(aux1,level.conf = 0.95)$res
  }
  if ("row"%in%ellipse){
    rowCA <- CA(concRow,row.sup=(nrow(X)+1):nrow(concRow),graph=FALSE)
    aux2 <- cbind.data.frame(paste("row",1:nrow(X),sep=""),rowCA$row.sup$coord[,axes])
	aux2[,1] <- as.factor(aux2[,1])
    ellRow <- coord.ellipse(aux2,level.conf = 0.95)$res
  }
  nullxlimylim <- (is.null(xlim) & is.null(ylim))
  if (is.null(xlim)){
    if (("col"%in%ellipse)&("row"%in%ellipse)) xlim <- c(min(ellCol[,2],ellRow[,2]),max(ellCol[,2],ellRow[,2]))
    else {
      if ("col"%in%ellipse) xlim <- c(min(ellCol[,2]),max(ellCol[,2]))
      if ("row"%in%ellipse) xlim <- c(min(ellRow[,2]),max(ellRow[,2]))
    }
  }
  if (is.null(ylim)){
    if (("col"%in%ellipse)&("row"%in%ellipse)) ylim <- c(min(ellCol[,3],ellRow[,3]),max(ellCol[,3],ellRow[,3]))
    else {
      if ("col"%in%ellipse) ylim <- c(min(ellCol[,3]),max(ellCol[,3]))
      if ("row"%in%ellipse) ylim <- c(min(ellRow[,3]),max(ellRow[,3]))
    }
  }
  if(!is.null(x$row.sup)){
    xlim <- c(min(xlim[1],min(x$row.sup$coord[,axes[1]])), max(xlim[2],max(x$row.sup$coord[,axes[1]])))
    ylim <- c(min(ylim[1],min(x$row.sup$coord[,axes[2]])), max(ylim[2],max(x$row.sup$coord[,axes[2]])))
  }
  if(!is.null(x$col.sup)){
    xlim <- c(min(xlim[1],min(x$col.sup$coord[,axes[1]])), max(xlim[2],max(x$col.sup$coord[,axes[1]])))
    ylim <- c(min(ylim[1],min(x$col.sup$coord[,axes[2]])), max(ylim[2],max(x$col.sup$coord[,axes[2]])))
  }
  if (nullxlimylim & diff(xlim)/diff(ylim)>3) ylim <- (ylim-mean(ylim))*diff(xlim)/diff(ylim)/3 + mean(ylim)
  if (nullxlimylim & diff(xlim)/diff(ylim)<1/2) xlim <- (xlim-mean(xlim))*diff(ylim)/diff(xlim)/2 + mean(xlim)
  graph <- plot(x,axes=axes,xlim=xlim,ylim=ylim,col.col=col.col,col.row=col.row, graph.type = graph.type, ggoptions = ggoptions, ...)

  if ("row"%in%ellipse){
    lev<-paste("row",1:nlevels(ellRow[, 1]),sep="")
    for (e in 1:nlevels(ellRow[, 1])) {
      data.elli <- ellRow[ellRow[, 1] == lev[e], -1]
      if(graph.type == "classic"){
        lines(x=data.elli[, 1], y = data.elli[, 2], col = col.row.ell[e])
      }
      if(graph.type == "ggplot"){
        graph <- graph + geom_path(aes_string(x=data.elli[,1],y=data.elli[,2]), color = col.row.ell[e])
      }
    }   
  }
  if ("col"%in%ellipse){
    lev<-paste("col",1:nlevels(ellCol[, 1]),sep="")
    for (e in 1:nlevels(ellCol[, 1])) {
      data.elli <- ellCol[ellCol[, 1] == lev[e], -1]
      if(graph.type == "classic"){
        lines(x=data.elli[, 1], y = data.elli[, 2], col = col.col.ell[e])
      }
      if(graph.type == "ggplot"){
        graph <- graph + geom_path(aes_string(x=data.elli[,1],y=data.elli[,2]), color = col.col.ell[e])
      }
    }   
  }
  if(graph.type == "ggplot") print(graph)
}
