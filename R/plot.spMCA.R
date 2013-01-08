plot.spMCA <- function (x, axes = c(1, 2), choix="ind",
    xlim = NULL, ylim = NULL, invisible = NULL, 
    col.ind = "blue", col.var = "red", col.quali.sup = "darkgreen",
    col.ind.sup = "darkblue", col.quanti.sup = "blue",
    label="all", cex = 1, title = NULL, habillage = "none", palette=NULL, new.plot=FALSE, ...){
    
    xx <- x
	class(xx) <- c("MCA", "list ")
	plot.MCA(xx, axes = axes, choix=choix,xlim = xlim, ylim = ylim, invisible = invisible, 
    col.ind = col.ind, col.var = col.var, col.quali.sup = col.quali.sup,col.ind.sup = col.ind.sup, col.quanti.sup = col.quanti.sup,
    label=label, cex = cex, title = title, habillage = habillage, palette=palette, new.plot=new.plot, ...)
}
