predict.FAMD <- function(object, newdata, ...){
    if (!inherits(object, "FAMD")) print("object should be a FAMD object")
    if (!is.null(colnames(newdata))) {
	  if (any(!(rownames(object$var$coord)%in%colnames(newdata)))) warning("The names of the variables is not the same as the ones in the active variables of the FAMD result")
	}
	object <- object$call$object   ## ne pas utiliser les noms des variables comme dans les autres fonctions car les variables sont reordonnees

    ecart.type <- object$call$ecart.type
    centre <- object$call$centre
    prop <- object$call$prop
	ncp <- ncol(object$var$coord)
	illu <- object$call$sup.var
    if (length(illu)>0) object <- object[,-illu]
	newdata <- newdata[,colnames(object)]
	pb = NULL
	for (i in 1:ncol(newdata)) {
	  if (sum(!levels(newdata[,i])%in%levels(object[,i]))>0) pb <- c(pb, levels(newdata[,i])[which(!levels(newdata[,i])%in%levels(object[,i]))])
	}
	if (!is.null(pb)) stop("The following categories are not in the active dataset: ",pb)
	newdata <- rbind.data.frame(object,newdata)[-(1:nrow(object)),,drop=FALSE]
	if (!is.null(object$call$sup.var)) {
	  numAct <- which((object$call$type=="s")[-object$call$sup.var])
	  facAct <- which((object$call$type=="n")[-object$call$sup.var])
	}
	else {
	  numAct <- which(object$call$type=="s")
	  facAct <- which(object$call$type=="n")
	}
    if (is.null(ecart.type)) ecart.type <- rep(1, length(centre))
    QuantiAct <- as.matrix(newdata[,numAct,drop=FALSE])
	QuantiAct <- t(t(QuantiAct)-centre[1:length(numAct)])
	QuantiAct <- t(t(QuantiAct)/ecart.type[1:length(numAct)])
	
	QualiAct <- tab.disjonctif(newdata[,facAct,drop=FALSE])
	QualiAct <- t(t(QualiAct)- prop)
	QualiAct <- t(t(QualiAct)/sqrt(prop))

	tab.newdata=cbind(QuantiAct,QualiAct)
    marge.col <- object$call$marge.col
	
    coord <- crossprod(t(as.matrix(tab.newdata)),object$svd$V)
    dist2 <- rowSums(tab.newdata^2)
    cos2 <- coord^2/dist2
    coord <- coord[, 1:ncp,drop=FALSE]
    cos2 <- cos2[, 1:ncp,drop=FALSE]
    colnames(coord) <- colnames(cos2) <- paste("Dim", 1:ncp)
    rownames(coord) <- rownames(cos2) <- rownames(newdata)
    result <- list(coord = coord, cos2 = cos2, dist2 = sqrt(dist2))
	return(result)
}
