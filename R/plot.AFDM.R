plot.AFDM <- function (x, choix = "var", axes = c(1, 2), 
    lab.var = TRUE, lab.ind = TRUE, habillage = "none", col.lab = FALSE,
    col.hab = NULL, invisible = NULL, lim.cos2.var = 0., xlim = NULL,
    ylim = NULL, cex = 1, title = NULL, palette=NULL, new.plot=FALSE, ...) {

if (choix=="var") choix=="group"
if (choix=="quanti") choix=="var"
class(x) <- c("MFA", "list")
if ((choix=="ind") & (is.numeric(habillage))) {
  x$separate.analyses=vector(mode = "list", length = ncol(x$call$X))
  for (i in 1:ncol(x$call$X)) x$separate.analyses[[i]]$call$X <- matrix(x$call$X[,i],ncol=1)
 }
plot.MFA (x, axes = axes, choix = choix, lab.var = lab.var,
    lab.ind = lab.ind, lab.par = FALSE, habillage = habillage,
    col.hab = col.hab, invisible = invisible, lim.cos2.var = lim.cos2.var, 
    xlim = NULL, ylim = NULL, cex = cex, title = title, palette=palette, new.plot=new.plot,...)
class(x) <- c("AFDM", "list")
}
