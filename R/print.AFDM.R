print.AFDM <- function (x, file = NULL, sep = ";", ...){
    res.afdm <- x
    if (!inherits(res.afdm, "AFDM")) stop("non convenient data")
    cat("*The results are available in the following objects:\n\n")
    res <- array("", c(6, 2), list(1:6, c("name", "description")))
    res[1, ] <- c("$eig", "eigenvalues and inertia")
    res[2, ] <- c("$link.grpe", "Lg coefficients measuring the links between groups of variables")
    res[3, ] <- c("$grpe", "Results for the groups of variables")
    res[4, ] <- c("$ind", "results for the individuals")
    res[5, ] <- c("$quali.var", "Results for the qualitative variables")
    res[6, ] <- c("$quanti.var", "Results for the quantitative variables")
    print(res)
    if (!is.null(file)) {
      write.infile(res.afdm,file = file, sep=sep)
      print(paste("All the results are in the file",file))
    }
}
