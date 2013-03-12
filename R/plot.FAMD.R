plot.FAMD <- function (x, choix = c("ind","var","quanti","quali"), axes = c(1, 2), 
    lab.var = TRUE, lab.ind = TRUE, habillage = "none", col.lab = FALSE,
    col.hab = NULL, invisible = NULL, lim.cos2.var = 0., xlim = NULL,
    ylim = NULL, title = NULL, palette=NULL, autoLab = c("auto","yes","no"), new.plot = FALSE, 
	select = NULL, unselect = 0.7, shadowtext=FALSE,...) {

    autoLab <- match.arg(autoLab,c("auto","yes","no"))
	if (autoLab=="yes") autoLab=TRUE
	if (autoLab=="no") autoLab=FALSE
    choix <- match.arg(choix,c("ind","var","quanti","quali"))
if (choix=="var") {
  choix="group"
  x$group$coord <- x$var$coord
}
if (choix=="quanti")  choix="var"
if (choix=="quali"){
  choix="ind"
  invisible=c(invisible,"ind","ind.sup")
}
class(x) <- c("MFA", "list")
if ((choix=="ind") & (is.numeric(habillage))) {
  x$separate.analyses=vector(mode = "list", length = ncol(x$call$X))
  for (i in 1:ncol(x$call$X)) x$separate.analyses[[i]]$call$X <- matrix(x$call$X[,i],ncol=1)
 }
plot.MFA (x, axes = axes, choix = choix, lab.var = lab.var,
    lab.ind = lab.ind, lab.par = FALSE, habillage = habillage,
    col.hab = col.hab, invisible = invisible, lim.cos2.var = lim.cos2.var, 
    xlim = xlim, ylim = ylim, title = title, palette=palette, new.plot=new.plot,
	select=select,unselect=unselect,autoLab=autoLab,shadowtext=shadowtext,...)
class(x) <- c("FAMD", "list")
}
