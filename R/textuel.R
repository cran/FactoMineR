textuel <- function(exp, maj.in.min = TRUE, accent = TRUE, min.word = 2){
  mots <- list()
  for (i in 1:length(exp)){
    expression[[i]] <- chartr("(),?.;/:'!$§=+\n;{}<>[]@-","                         ",exp[[i]])
    if (accent) expression[[i]] <- chartr("éèêâûôîìàùòç","eeeauoiiauoc",expression[[i]])
    if (maj.in.min) expression[[i]] <- chartr("A-Z","a-z",expression[[i]])
    expression[[i]] <- gsub('  ', ' ', expression[[i]]) 
    expression[[i]] = strsplit(expression[[i]]," ")
  }
  mots.totaux = as.factor(unlist(expression))
  for (i in 1:length(expression)) mots[[i]] = c(levels(mots.totaux),expression[[i]][[1]])
  nbmots = length(levels(mots.totaux))
  table = as.data.frame(summary(mots.totaux,maxsum=nbmots))
  row.names(table)= levels(mots.totaux)
  for (i in 1:length(expression)) table = cbind(table,summary(as.factor(mots[[i]]),maxsum=nbmots)-1)
  table = cbind.data.frame(table, apply(matrix(as.integer(table[,-1]>0),nrow=length(levels(mots.totaux))),1,sum))
  colnames(table)[1] = "words"
  if (!is.null(names(exp))) colnames(table)[2:(length(exp)+1)] = names(exp)
  if (is.null(names(exp))) colnames(table)[2:(length(exp)+1)] = paste("exp",1:length(exp),sep=".")
  colnames(table)[ncol(table)] = "nb.list"
  row.names(table)= levels(mots.totaux)
  table.for.CA = table[table[,1]>min.word,-c(1,ncol(table))]
  res.ca <- CA(table.for.CA, graph = FALSE)
  res = list(res.ca, nb.words = table[rev(order(table[,1])),c(1,ncol(table))])
  return(res)
}
