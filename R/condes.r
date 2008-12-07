condes = function (donnee, num.var, proba = 0.05) {
    lab.sauv <- lab <- colnames(donnee)
    quali = NULL
    for (i in 1:length(lab)) {
        lab[i] = gsub(" ", ".", lab[i])
        if (is.factor(donnee[, i])) {
            if (any(is.na(donnee[, i]))) {
                levels(donnee[, i]) <- c(levels(donnee[, i]),
                  "NA")
                donnee[, i][is.na(donnee[, i])] <- "NA"
            }
            if (levels(donnee[, i])[1] == "")
                levels(donnee[, i])[1] = "NA"
            if (i != num.var)
                quali = c(quali, i)
        }
    }
    quanti = (1:ncol(donnee))[-c(quali, num.var)]
    if (length(quanti) == 0) quanti = NULL
    colnames(donnee) = lab
    result = list()
    if (!is.null(quanti)) {
      tab.quanti = cor(donnee[,quanti],donnee[,num.var])
      aux = cbind(tab.quanti, pf(tab.quanti^2*(nrow(donnee)-2)/(1-tab.quanti^2),1,nrow(donnee) - 2, lower.tail=FALSE))
      rownames(aux) = colnames(donnee)[quanti]
      resQ = NULL
      if (NROW(aux)>1) aux <- aux[rev(order(aux[, 1])), ]  ## sort by V-test
      resQ <- aux[aux[,2]<proba, ,drop=F]         ## keep significant
#      if (sum(aux[,2]<proba)>1) resQ <- aux[aux[,2]<proba, ,drop=F]         ## keep significant
#      if (sum(aux[,2]<proba)==1) {
#        resQ <- aux[aux[,2]<proba, ,drop=F]     ## keep significant
#        rownames(resQ) <- rownames(aux)[aux[,2]<proba]
#      }
      if (!is.null(resQ)) colnames(resQ) = c("correlation","P-value")
      result$quanti <- resQ
    }
    if (!is.null(quali)) {
      old.contr = options()$contrasts
      options(contrasts = c("contr.sum", "contr.sum"))
      tabF = matrix(NA,length(quali),1)
      tabT = matrix(NA,1,2)
      indice.tabT = 0
      for (v in 1:length(quali)){
        res.aov <- aov(donnee[,num.var]~donnee[,quali[v]], , na.action = na.exclude)
        res <- summary(res.aov)[[1]]
        tabF[v,1] <- pf(res[1, 4], res[1,1], res[dim(res)[1], 1], lower.tail = FALSE)
        resT = summary.lm(res.aov)$coef[,c(1,4)]
        intercept = resT[1,1]
        if (nrow(resT)==2){
          resT[1,1] = resT[2,1]
          resT[1,2]=resT[2,2]
          resT[2,1] = -resT[2,1]
        }
        if (nrow(resT)>2){
          resT = resT[-1,]   ## delete the intercept
          cov.mat = vcov(res.aov)
          dern.mod = c(-sum(resT[, 1]), pt(abs(sum(resT[, 1]))/sqrt(sum(cov.mat[,-1])), res[2,1], lower.tail = FALSE) * 2)
          resT = rbind(resT, dern.mod)
        }
        rownames(resT) = levels(donnee[,quali[v]])
        tabT = rbind(tabT,resT)
      }
      name.tabF = colnames(donnee)[quali]
      colnames(tabF) = c("P-value")
      tabT = tabT[-1,]
      resF = resT = NULL
      if (NROW(tabF)>1){
        name.tabF = name.tabF[order(tabF)]
        tabF <- tabF[order(tabF),]  ## sort by V-test
      }
      if (sum(tabF<proba)>0){
        resF <- matrix(tabF[tabF<proba], ncol=1) 
        rownames(resF) <- name.tabF[tabF<proba]
        colnames(resF) = c("P-value")
      }
      tabT <- tabT[rev(order(sign(tabT[, 1])/tabT[,2])), ]  ## sort by V-test
      if (sum(tabT[,2]<proba)>1){
        resT <- tabT[tabT[,2]<proba, ]         ## keep significant
        colnames(resT) <- c("Estimate","P-value")
      } 
      if (sum(tabT[,2]<proba)==1) {
        resT <- matrix(tabT[tabT[,2]<proba, ], ncol=2)         ## keep significant
        rownames(resT) <- rownames(tabT)[tabT[,2]<proba]
        colnames(resT) <- c("Estimate","P-value")
      }
      result$quali = resF
      result$category = resT
      options(contrasts = old.contr)
    }
    if (is.null(result$quanti)&is.null(result$quali)&is.null(result$category)) result=NULL
    return(result)
}
