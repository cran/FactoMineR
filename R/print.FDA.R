print.FDA<-function(x, file = NULL, sep = ";", ...){
   res.fda <- x
  if(!inherits(res.fda, "FDA")) stop("non convenient data")

  cat("**Results for the Factorial Discriminant Analysis (FDA))**\n\n")
  cat("The analysis was done on", nrow(res.fda$call$X), "individuals\ndistributed among",
        nlevels(res.fda$call$fact), "class and described by", ncol(res.fda$call$X), "variables\n\n")
    cat("*The results are available in the following objects:\n\n")

  res<-array("", c(12, 2), list(1:12, c("names", "description")))
  res[1, ] <- c("$eigen.values", "eigen values")
  res[2, ] <- c("$eigen.vectors", "eigen vectors")
  res[3, ] <- c("$res.var", "results for the variables")
  res[4, ] <- c("$res.cg", "results for the centers of gravity")
  res[5, ] <- c("$res.ind", "results for the individuals")
  res[6, ] <- c("$call", "data frame and grouping factor")
  res[7, ] <- c("$df", "discriminant functions")
  res[8, ] <- c("$score", "individuals scores")
  res[9, ] <- c("$predict", "predicted membership of one class")
  res[10, ] <- c("$eval", "affectation model quality")
  res[11, ] <- c("$coord.ind.sup", "coords of the supplementary individuals")
  res[12, ] <- c("$res.cv", "cross validation results")

  print(res)
    if (!is.null(file)) {
      write.infile(res.pca,file = file, sep=sep)
      print(paste("All the results are in the file",file))
    }

}
