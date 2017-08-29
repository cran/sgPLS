\name{sgPLSda}
\encoding{latin1}
\alias{sgPLSda}

\title{Sparse Group Sparse Partial Least Squares Discriminant Analysis (sPLS-DA)}

\description{Function to perform sparse group Partial Least Squares to classify samples (supervised analysis) and select variables.
}

\usage{
sgPLSda(X, Y, ncomp = 2, keepX = rep(ncol(X), ncomp),
       max.iter = 500, tol = 1e-06, ind.block.x,
     alpha.x, upper.lambda = 10 ^ 5)
}	

\arguments{
  \item{X}{numeric matrix of predictors. \code{NA}s are allowed.}
  \item{Y}{a factor or a class vector for the discrete outcome.}
  \item{ncomp}{the number of components to include in the model (see Details).}
  \item{keepX}{numeric vector of length \code{ncomp}, the number of variables
    to keep in \eqn{X}-loadings. By default all variables are kept in the model.}
  \item{max.iter}{integer, the maximum number of iterations.}   
  \item{tol}{a positive real, the tolerance used in the iterative algorithm.} 
  \item{ind.block.x}{a vector of integers describing the grouping of the \eqn{X}-variables. (see an example in Details section)}
  \item{alpha.x}{The mixing parameter (value between 0 and 1) related to the sparsity within group for the \eqn{X} dataset.}
  \item{upper.lambda}{By default \code{upper.lambda=10 ^ 5}. A large value specifying the upper bound of the intervall of lambda values for searching the value of the tuning parameter (lambda) corresponding to a non-zero group of variables.}

  }

\details{
\code{sgPLSda} function fit sgPLS models with \eqn{1, \ldots ,}\code{ncomp} components
to the factor or class vector \code{Y}. The appropriate indicator (dummy)
matrix is created.

\code{ind.block.x <- c(3,10,15)} means that \eqn{X} is structured into 4 groups: X1 to X3; X4 to X10, X11 to X15 and X16 to X\eqn{p} where \eqn{p} is the number of variables in the \eqn{X} matrix.}


\value{
\code{sPLSda} returns an object of class \code{"sPLSda"}, a list 
that contains the following components:

  \item{X}{the centered and standardized original predictor matrix.}
  \item{Y}{the centered and standardized indicator response vector or matrix.}
  \item{ind.mat}{the indicator matrix.}
  \item{ncomp}{the number of components included in the model.}
  \item{keepX}{number of \eqn{X} variables kept in the model on each component.}
  \item{mat.c}{matrix of coefficients to be used internally by \code{predict}.}
  \item{variates}{list containing the variates.}
  \item{loadings}{list containing the estimated loadings for the \code{X} and 
	\code{Y} variates.}
  \item{names}{list containing the names to be used for individuals and variables.}
   \item{tol}{the tolerance used in the iterative algorithm, used for subsequent S3 methods}
  \item{max.iter}{the maximum number of iterations, used for subsequent S3 methods}
\item{iter}{Number of iterations of the algorthm for each component}
\item{ind.block.x}{a vector of integers describing the grouping of the X variables.}
\item{alpha.x}{The mixing parameter related to the sparsity within group for the \eqn{X} dataset.}
\item{upper.lambda}{The upper bound of the intervall of lambda values for searching the value of the tuning parameter (lambda) corresponding to a non-zero group of variables.}
}

\references{

Liquet Benoit, Lafaye de Micheaux Pierre , Hejblum Boris, Thiebaut Rodolphe (2016). A group and Sparse Group Partial Least Square approach applied in Genomics context. \emph{Bioinformatics}.	
	
	
On sPLS-DA:
Le Cao, K.-A., Boitard, S. and Besse, P. (2011). Sparse PLS Discriminant Analysis: biologically relevant feature selection and graphical displays for multiclass problems. \emph{BMC Bioinformatics} \bold{12}:253.
}

\author{Benoit Liquet and Pierre Lafaye de Micheaux.}

\seealso{\code{\link{sPLS}}, \code{\link{summary}}, 
\code{\link{plotIndiv}}, \code{\link{plotVar}}, 
\code{\link{cim}}, \code{\link{network}}, \code{\link{predict}}, \code{\link{perf}} and http://www.mixOmics.org for more details.}

\examples{
data(simuData)
X <- simuData$X
Y <- simuData$Y
ind.block.x <- seq(100, 900, 100)
ind.block.x[2] <- 250
#To add some noise in the second group
model <- sgPLSda(X, Y, ncomp = 3,ind.block.x=ind.block.x, keepX = c(2, 2, 2)
, alpha.x = c(0.5,0.5,0.99))
result.sgPLSda <- select.sgpls(model)
result.sgPLSda$group.size.X
##perf(model,criterion="all",validation="loo") -> res
##res$error.rate	
}		


\keyword{regression}
\keyword{multivariate}
