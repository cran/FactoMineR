print.AFDM <- function (x, file = NULL, sep = ";", ...){
    res.afdm <- x
    if (!inherits(res.afdm, "AFDM")) stop("non convenient data")
    cat("**Resultat de votre Analyse Factorielle de Données Mixtes (AFDM)**\n\n")
    cat("Votre analyse a été réalisée sur", nrow(res.afdm$call$X),
        "individus, caractérisés par", sum(res.afdm$call$groupe[res.afdm$call$type ==
            "c"]), "variables quantitatives et par", sum(res.afdm$call$groupe[res.afdm$call$type ==
            "n"]), "variables qualitatives\n\n")
    cat("*The results are available in the following objects:\n\n")
    res <- array("", c(6, 2), list(1:6, c("nom", "description")))
    res[1, ] <- c("$eig", "eigenvalues and inertia")
    res[2, ] <- c("$link.grpe", "coefficients Lg de link entre les 'groupes' de variables")
    res[3, ] <- c("$grpe", "Ensemble des résultats relatifs aux 'groupes' de variables")
    res[4, ] <- c("$ind", "Ensemble des résultats relatifs aux individus")
    res[5, ] <- c("$quali.var", "Ensemble des résultats relatifs aux variables qualitatives")
    res[6, ] <- c("$quanti.var", "Ensemble des résultats relatifs aux variables quantitatives")
    print(res)
    if (!is.null(file)) {
      write.infile(res.afdm,file = file, sep=sep)
      print(paste("All the results are in the file",file))
    }
}
