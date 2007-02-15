\name{FDA}
\alias{FDA}

\title{ Factorial Discriminant Analysis }
\description{
 Performs Factorial Discriminant Analysis (FDA).
}

\usage{
FDA(X, fact, new.data = NULL, new.fact = NULL, prior = NULL, 
    cross.val = FALSE, graph = TRUE, axes = c(1,2))
}

\arguments{
  \item{X         }{a data frame with n rows (individuals) and p columns (without the factor). }
  \item{fact      }{a factor specifying the class for each observation.}
  \item{new.data  }{a data frame of individuals to be classified or, if \code{new.fact} is not null, which formed the test sample.  }
  \item{new.fact  }{if \code{new.data} is not null, a factor specifying the class for each observation of the test sample.   }
  \item{prior     }{a vector. The prior probabilities of class membership.  If unspecified, the class proportions for the training sample (\code{new.dat}) are used.  }
  \item{cross.val }{if TRUE, returns results for leave-one-out cross-validation. }
  \item{graph}{boolean, if TRUE graphs are plotted}
  \item{axes}{a length 2 vector specifying the components to plot}
}

\details{
 If there is no test sample, the evaluation of the affectation model is realised on the test sample itself.
}

\value{
Returns a list including :

  \item{eig }{ a numeric vector containing all the  eigenvalues }
  \item{eigen.vectors }{ a list of matrices containing all the eigenvectors}
  \item{var      }{ a list of matrices containing all the results for the active variables  }
  \item{cg       }{ a list of matrices containing all the results for the centers of gravity }
  \item{ind      }{ a list of matrices containing all the results for the individuals }
  \item{call         }{ a list with the data frame and grouping factor used}
  \item{df           }{ the discriminant functions}
  \item{score        }{ a vector whith the individuals scores.}
  \item{eval         }{ a list with all the results for the evaluation of the affectation model}
  \item{res.cv       }{ a list with all the results for for leave-one-out cross-validation.}

}

\author{ Jeremy Mazet \email{jeremy.mazet@soredab.org} }

\seealso{\code{\link{plot.FDA}}, \code{\link{print.FDA}}, \code{\link{dimdesc}}}

\examples{
data(wine)
res.fda <- FDA(wine[,-(1:28)], fact=wine[,1])
}


\keyword{ multivariate }
