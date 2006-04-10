#!Factorial Discriminant Analysis

FDA<-function(X, fact, new.data=NULL, new.fact=NULL, prior=NULL, cross.val=FALSE)
{
  fda<-function(X, fact, new.data=NULL, new.fact=NULL, prior=NULL)
  {
    X.dep<-X<-as.data.frame(X)
  	fact<-as.factor(fact)
  	nbre.ind<-nrow(X)
  	nbre.var<-ncol(X)
  	nbre.cla<-nlevels(fact)
  	row.w<-rep(1,nrow(X))
  	col.w<-rep(1,ncol(X))
  	ncp<-min(ncol(X),(nlevels(fact)-1))

  	# fonction interne permettant le calcul d'une moyenne pondérée
    moy.p <- function(V, poids)
     {
       res <- sum(V * poids)/sum(poids)
     }
    #############fin fonction moy.p

  	#fonction interne permettant le calcul d'un ecart type
    ec <- function(V, poids)
    {
      res <- sqrt(sum(V^2 * poids)/sum(poids))
    }
    ##############fin fonction ec

    #fonction evaluation
    evaluation<-function(X.dep, fact, centre, ecart.type, G, S, Sr, V, new.data, new.fact, prior=NULL)
  	{
      if (ncol(new.data)!=ncol(X.dep))
    	stop("non convenient new.data")

    	# centrage et réduction de new.data
    	XX<-new.data
     	XX<-as.matrix(sweep(XX,2,centre,FUN="-"))
     	XX<-as.matrix(sweep(XX,2,ecart.type,FUN="/"))

    	# calcul des fonctions discriminantes
	    fd<-matrix(NA, ncol(XX)+1, nlevels(fact))
    	colnames(fd)<-levels(fact)
    	rownames(fd)<-c("constante", colnames(X))
	    if (is.null(prior))
	    {
		    for(g in 1:nlevels(fact))
			    {
				    fd[1,g]<- t(as.matrix(G[g,]))%*%solve(Sr)%*%as.matrix(G[g,]) *-1
  	  			fd[-1,g]<-2*t(as.matrix(G[g,]))%*%solve(Sr)
	    		}
	    }

	    if (!is.null(prior))
	    {
	  	  for(g in 1:nlevels(fact))
		  	  {
			  	  fd[1,g]<- 2*log(prior[g])-t(as.matrix(G[g,]))%*%solve(Sr)%*%as.matrix(G[g,])
  		    fd[-1,g]<-2*t(as.matrix(G[g,]))%*%solve(Sr)
  		  	}
    	}

    	# calcul des scores
    	score<-matrix(NA, nrow(XX), nlevels(fact))
   	  colnames(score)<-levels(fact)
    	rownames(score)<-rownames(XX)
    	for (i in 1:nrow(XX))
  	  {
  		  score[i,]<-fd[1,] + XX[i,]%*%fd[-1,]
    	}

      # affectation
    	res.affecte<-vector(length=nrow(XX))
    	for (i in 1:nrow(XX))
    	{
    		res.affecte[i]<-names(which(score[i,]==max(score[i,])))
    	}

    	# calcul éventuel des taux de bien classé
    	if (!is.null(new.fact))
	    {
	    	res.compte<-res.taux<-as.data.frame(matrix(NA, nlevels(fact), nlevels(fact)))
	    	dimnames(res.compte)<-dimnames(res.taux)<-list(paste(levels(fact), "fitted"), paste(levels(fact), "predicted"))
	    	for(i in 1: nlevels(fact))
	    	{
	    		test.tmp<-as.character(new.fact[new.fact==levels(fact)[i]])
	    		res.affecte.tmp<-as.character(res.affecte[new.fact==levels(fact)[i]])
	    		for(j in 1: nlevels(fact))
	  	  	{
	  		  	res.compte[i,j]<-length(res.affecte.tmp[res.affecte.tmp==levels(fact)[j]])
	  			  res.taux[i,j]<-length(res.affecte.tmp[res.affecte.tmp==levels(fact)[j]])/length(test.tmp)*100
    			}
	    	}
	    	TBC<-sum(diag(as.matrix(res.compte)))/length(new.fact)
	  	  TMC<-1-TBC
  	  	eval<-list(res.compte=res.compte,res.taux=res.taux, TBC=TBC, TMC=TMC)
  	  }
  	  else eval<-NULL

    	# calcul des coordonnées pour représentation graphique
      coord.ind.sup<-XX%*%solve(S)%*%V

      # mise en forme et édition des résultats
	    res<-list(discri.f=fd, score=score, res.affecte=res.affecte, eval=eval, coord.ind.sup=coord.ind.sup)
	    return(res)
    }
    ##############fin fonction evaluation
    
    # centrage et réduction des données
	  centre<-apply(X,2,moy.p,row.w)
  	X<-as.matrix(sweep(X,2,centre,FUN="-"))
	  ecart.type<-apply(X, 2, ec, row.w)
  	ecart.type[ecart.type<=1e-08]<-1
	  X<-as.matrix(sweep(X,2,ecart.type,FUN="/"))

  	# calcul de la matrice des centres de gravité
	  T<-tab.disjonctif(fact)
  	Plg<-t(T)%*%diag(row.w/sum(row.w))%*%T
	  G<-solve(Plg)%*%t(T)%*%diag(row.w/sum(row.w))%*%X
  	rownames(G)<-paste("classe", levels(fact), sep=".")

	  # calcul de la matrice à diagonaliser
  	Xe<-T%*%G
	  Xr<-X-Xe
  	Sr<-t(Xr)%*%diag(row.w/sum(row.w))%*%Xr
	  Se<-t(G)%*%Plg%*%G
  	S<-Se+Sr

	  # diagonalisation
  	tmp<-eigen(Se%*%solve(S))
	  eig<-Re(tmp$values[1:ncp])
  	V<-Re(tmp$vectors[,1:ncp])
	  U<-G%*%solve(S)%*%V%*%diag(eig^-0.5)

  	#travail sur les variables
    coord.var <- sweep(V, 2, sqrt(eig), FUN = "*")
    contrib.var <- sweep(coord.var^2, 2, eig, "/")
    contrib.var <- sweep(contrib.var, 1, col.w, "*")
    dist2 <- apply(coord.var^2, 1, sum)
    cor.var <- sweep(coord.var, 1, sqrt(dist2), FUN = "/")
    cos2.var <- cor.var^2
	  rownames(coord.var)<-rownames(cos2.var)<-rownames(cor.var)<-rownames(contrib.var)<-colnames(X)
   	colnames(coord.var)<-colnames(cos2.var)<-colnames(cor.var)<-colnames(contrib.var)<-paste("Dim", c(1:ncol(V)))
  	res.var<-list(coord=coord.var[,1:ncp], cor=cor.var[,1:ncp], cos2=cos2.var[,1:ncp], contrib=contrib.var[,1:ncp])

  	#travail sur les cg
    coord.cg <- sweep(U, 2, sqrt(eig), FUN = "*")
    dist2 <- apply(coord.cg^2, 1, sum)
    cos2.cg <- sweep(coord.cg^2, 1, dist2, FUN = "/")
    contrib.cg <- sweep(coord.cg^2, 1, row.w/sum(row.w), FUN = "*")
    contrib.cg <- sweep(contrib.cg, 2, eig, FUN = "/")
  	rownames(coord.cg)<-rownames(cos2.cg)<-rownames(contrib.cg)<-rownames(G)
  	colnames(coord.cg)<-colnames(cos2.cg)<-colnames(contrib.cg)<-paste("Dim", c(1:ncol(U)))
  	res.cg<-list(coord=coord.cg[,1:ncp], cos2=cos2.cg[,1:ncp], contrib=contrib.cg[,1:ncp])

  	#travail sur les individus
  	coord.ind<-X%*%solve(S)%*%V
  	dist2 <- apply(coord.ind^2, 1, sum)
  	cos2.ind <- sweep(coord.ind^2, 1, dist2, FUN = "/")
	  colnames(coord.ind)<-colnames(cos2.ind)<-paste("Dim", c(1:ncol(V)))
  	rownames(coord.ind)<-rownames(cos2.ind)<-rownames(X)
  	res.ind<-list(coord=coord.ind[,1:ncp], cos2=cos2.ind[,1:ncp])

    # evaluation du modèle
    if(is.null(new.data) & !is.null(new.fact))
    stop ("missing new.data")
    ind.sup<-TRUE
    if(is.null(new.data) & is.null(new.fact) )
    {
      new.data<-X.dep
      new.fact<-fact
      ind.sup=FALSE
    }

    res.eval<-evaluation(X.dep=X.dep, fact=fact, centre=centre, ecart.type=ecart.type, G=G, S=S, Sr=Sr, V=V, new.data=new.data, new.fact=new.fact, prior=prior)
    if(!ind.sup) res.eval$coord.ind.sup<-NULL
    
	  # mise en forme et édition des résultats (ancienne AFD)
	  call<-list(X=X.dep, fact=fact)
  	res<-list(eigen.values=eig, eigen.vectors=list(V=V, U=U), res.var=res.var, res.cg=res.cg, res.ind=res.ind, call=call, fd=res.eval$discri.f, score=res.eval$score, predict=res.eval$res.affecte, eval=res.eval$eval, coord.ind.sup=res.eval$coord.ind.sup)
   	return(res)
  }

  res.fda<-fda(X=X, fact=fact, new.data=new.data, new.fact=new.fact, prior=prior)

  if(cross.val)
  {
   	tbc<-vector(length=nrow(X))
  	table.affecte<-vector(mode="list", length= nrow(X))
	  for(i in 1:nrow(X))
	  {
		  res.fda.tmp<-fda(X[-i,], fact[-i], new.data=X[i,], new.fact=fact[i], prior=prior)
		  tbc[i]<-res.fda.tmp$eval$TBC
		  table.affecte[[i]]<-res.fda.tmp$eval$res.taux
	  }
	  TBC<-mean(tbc)
  	TMC<-1-TBC
  	eval<-table.affecte[[1]]
  	for(l in 1:nlevels(fact))
	  {
	  	for(c in 1:nlevels(fact))
	  	{
  			vect.tmp<-NULL
	  		for(i in 1:nrow(X))
	  		{
	  			vect.tmp<-c(vect.tmp, table.affecte[[i]][l,c])
	  		}
	  		eval[l,c]<-mean(vect.tmp, na.rm=T)
	  	}
  	}
    res.cv<-list(TBC=TBC, TMC=TMC, eval=eval)
  }
  else res.cv<-NULL
  
  resultats<-c(res.fda, res.cv=list(res.cv))
  class(resultats)<-c("FDA", "list")
  plot(resultats)
  return(resultats)
}
  













