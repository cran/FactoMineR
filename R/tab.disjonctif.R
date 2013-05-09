tab.disjonctif<-function (tab){
    tab<-as.data.frame(tab)
    #fonction interne permettant la réalisation d'un TDC pour un unique facteur
    modalite.disjonctif <- function(i){
        moda <- tab[, i]
        nom <- names(tab)[i]
        n <- length(moda)
        moda <- as.factor(moda)
        x <- matrix(0, n, length(levels(moda)))
        x[(1:n) + n * (unclass(moda) - 1)] <- 1
        if((ncol(tab)!=1)&(levels(moda)[1]%in%c(1:nlevels(moda),"n","N","y","Y"))) dimnames(x) <- list(row.names(tab), paste(nom, levels(moda),sep = "."))
        else  dimnames(x) <- list(row.names(tab), levels(moda))
#        if(ncol(tab)==1) dimnames(x) <- list(row.names(tab), levels(moda))
#        else  dimnames(x) <- list(row.names(tab), paste(nom, levels(moda),sep = "."))
        return(x)
    }
    # fin fonction interne

    if (ncol(tab)==1) res<-modalite.disjonctif(1)
    else
    {
        res<- lapply(1:ncol(tab), modalite.disjonctif)
        res <- as.matrix(data.frame(res, check.names = FALSE))
    }
    return(res)
}
