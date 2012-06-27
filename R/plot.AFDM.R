plot.AFDM <- function (x, choix = "var", axes = c(1, 2), lab.grpe = TRUE,
    lab.var = TRUE, lab.ind = TRUE, habillage = "none", col.lab = FALSE,
    col.hab = NULL, invisible = NULL, lim.cos2.var = 0., xlim = NULL,
    ylim = NULL, cex = 1, title = NULL, palette=NULL, new.plot=FALSE, ...) {

if (choix=="var") choix=="group"
if (choix=="quanti") choix=="var"
class(x) <- c("MFA", "list")
plot.MFA (x, axes = axes, choix = choix, ellipse = NULL, lab.grpe = lab.grpe, lab.var = lab.var,
    lab.ind = lab.ind, lab.par = FALSE, habillage = habillage,
    col.hab = col.hab, invisible = invisible, partial = NULL, lim.cos2.var = lim.cos2.var, chrono = FALSE,
    xlim = NULL, ylim = NULL, cex = cex, title = title, palette=palette, new.plot=new.plot,...)
class(x) <- c("AFDM", "list")
}
