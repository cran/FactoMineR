print.CA <- function (x, file = NULL, sep = ";", ...){
    res.ca <- x
    if (!inherits(res.ca, "CA")) stop("non convenient data")
    cat("**Results of the Correspondance Analysis (CA)**\n\n")
    cat("The variable in rows have", nrow(res.ca$call$X),
        "categories, the variable in column", ncol(res.ca$call$X), "categories\n\n")
    IT <- res.ca$eig[length(res.ca$eig), 3] * sum(res.ca$call$X)
    df <- (nrow(res.ca$call$X) - 1) * (ncol(res.ca$call$X) - 1)
    pc <- pchisq(IT, df = df,lower.tail = FALSE)
    cat("\nThe chi square of independance between the two variables is equal to", IT, 
        ", the p-value associated to this chi square is equal to", pc, ".\n")
    cat("*The results are available in the following objects:\n\n")
    res <- array("", c(16, 2), list(1:16, c("nom", "description")))
    res[1, ] <- c("$eig", "eigenvalues")
    res[2, ] <- c("$col", "results for the columns")
    res[3, ] <- c("$col$coord", "coord. for the columns")
    res[4, ] <- c("$col$cos2", "cos2 for the columns")
    res[5, ] <- c("$col$contrib", "contributions of the columns")
    res[6, ] <- c("$row", "results for the rows")
    res[7, ] <- c("$row$coord", "coord. for the rows")
    res[8, ] <- c("$row$cos2", "cos2 for the rows")
    res[9, ] <- c("$row$contrib", "contributions of the rows")
    indice <- 10
    if (!is.null(res.ca$row.sup)){
      res[indice, ] <- c("$row.sup$coord", "coord. for supplementary rows")
      res[indice + 1, ] <- c("$row.sup$cos2", "cos2 for supplementary rows")
      indice <- indice + 2    
    }
    if (!is.null(res.ca$col.sup)){
      res[indice, ] <- c("$col.sup$coord", "coord. for supplementary columns")
      res[indice + 1, ] <- c("$col.sup$cos2", "cos2 for supplementary columns")
      indice <- indice + 2    
    }
    res[indice, ] <- c("$call", "summary called parameters")
    res[indice + 1, ] <- c("$call$marge.col", "weights of the columns")
    res[indice + 2, ] <- c("$call$marge.row", "weights of the rows")
    indice <- indice + 2
    print(res[1:indice,])
    if (!is.null(file)) {
      write.infile(res.ca,file = file, sep=sep)
      print(paste("All the results are in the file",file))
    }
}
