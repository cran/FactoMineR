plot.spMCA <- function (x, axes = c(1, 2), choix=c("ind","var","quanti.sup"),
    xlim = NULL, ylim = NULL, invisible = NULL, 
    col.ind = "blue", col.var = "red", col.quali.sup = "darkgreen",
    col.ind.sup = "darkblue", col.quanti.sup = "blue",
    label="all", cex = 1, title = NULL, habillage = "none", palette=NULL,
	autoLab = c("auto","yes","no"),new.plot=FALSE,select=NULL,selectMod=NULL, unselect=0.7, ...){
    
    autoLab <- match.arg(autoLab,c("auto","yes","no"))
	if (autoLab=="yes") autoLab=TRUE
	if (autoLab=="no") autoLab=FALSE
    choix <- match.arg(choix,c("ind","var","quanti.sup"))
    xx <- x
	class(xx) <- c("MCA", "list ")
	plot.MCA(xx, axes = axes, choix=choix,xlim = xlim, ylim = ylim, invisible = invisible, 
    col.ind = col.ind, col.var = col.var, col.quali.sup = col.quali.sup,col.ind.sup = col.ind.sup, col.quanti.sup = col.quanti.sup,
    label=label, cex = cex, title = title, habillage = habillage, palette=palette, new.plot=new.plot,
    autoLab = autoLab,select=select, selectMod=selectMod,unselect=unselect,	...)
}
