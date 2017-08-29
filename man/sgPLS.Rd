\name{sgPLS}
\encoding{latin1}
\alias{sgPLS}

\title{Sparse Group Partial Least Squares (sgPLS)}

\description{Function to perform  sparse group Partial Least Squares (sgPLS) in the conext of datasets are divided into groups of variables. The sgPLS approach enables selection at both groups and single feature levels.
}
\usage{
sgPLS(X, Y, ncomp, mode = "regression",
     max.iter = 500, tol = 1e-06, keepX, 
     keepY = NULL,ind.block.x, ind.block.y = NULL, alpha.x, alpha.y = NULL,
     upper.lambda = 10 ^ 5,scale=TRUE)
}	

\arguments{
  \item{X}{Numeric matrix of predictors.}
  \item{Y}{Numeric vector or matrix of responses (for multi-response models). }
  \item{ncomp}{The number of components to include in the model (see Details). }
  \item{mode}{character string. What type of algorithm to use, (partially) matching 
    one of \code{"regression"} or \code{"canonical"}. See Details.}
  \item{max.iter}{Integer, the maximum number of iterations.}
  \item{tol}{A positive real, the tolerance used in the iterative algorithm.}
  \item{keepX}{Numeric vector of length \code{ncomp}, the number of variables
    to keep in \eqn{X}-loadings. By default all variables are kept in the model.}
  \item{keepY}{Numeric vector of length \code{ncomp}, the number of variables
    to keep in \eqn{Y}-loadings. By default all variables are kept in the model.}
   \item{ind.block.x}{A vector of integers describing the grouping of the \eqn{X} variables. (see an example in Details section).}
   \item{ind.block.y}{A vector of integers describing the grouping of the \eqn{Y} variables (see example in Details section).}
   \item{alpha.x}{The mixing parameter (value between 0 and 1) related to the sparsity within group for the \eqn{X} dataset.}
    \item{alpha.y}{The mixing parameter (value between 0 and 1) related to the sparsity within group for the \eqn{Y} dataset.}
    \item{upper.lambda}{By default \code{upper.lambda=10 ^ 5}. A large value specifying the upper bound of the intervall of lambda values for searching the value of the tuning parameter (lambda) corresponding to a non-zero group of variables.}
    \item{scale}{a logical indicating if the orignal data set need to be scaled. By default \code{scale}=TRUE}

}

\details{
\code{sgPLS} function fit gPLS models with \eqn{1, \ldots ,}\code{ncomp} components. 
Multi-response models are fully supported. 

The type of algorithm to use is specified with the \code{mode} argument. Two gPLS 
algorithms are available: gPLS regression \code{("regression")} and gPLS canonical analysis 
\code{("canonical")} (see References). 

\code{ind.block.x <- c(3, 10, 15)} means that \eqn{X} is structured into 4 groups: X1 to X3; X4 to X10, X11 to X15 and X16 to X\eqn{p} where \eqn{p} is the number of variables in the \eqn{X} matrix.}

\value{
\code{sgPLS} returns an object of class \code{"sgPLS"}, a list 
that contains the following components:

  \item{X}{The centered and standardized original predictor matrix.}
  \item{Y}{The centered and standardized original response vector or matrix.}
  \item{ncomp}{The number of components included in the model.}
  \item{mode}{The algorithm used to fit the model.}
  \item{keepX}{Number of \eqn{X} variables kept in the model on each component.}
  \item{keepY}{Number of \eqn{Y} variables kept in the model on each component.}
  \item{mat.c}{Matrix of coefficients to be used internally by \code{predict}.}
  \item{variates}{List containing the variates.}
  \item{loadings}{List containing the estimated loadings for the \eqn{X} and 
	\eqn{Y} variates.}
  \item{names}{List containing the names to be used for individuals and variables.}
  \item{tol}{The tolerance used in the iterative algorithm, used for subsequent S3 methods.}
  \item{max.iter}{The maximum number of iterations, used for subsequent S3 methods.}
  \item{iter}{Vector containing the number of iterations for convergence in each component.} 
  \item{ind.block.x}{A vector of integers describing the grouping of the \eqn{X} variables.}
  \item{ind.block.y}{A vector of consecutive integers describing the grouping of the \eqn{Y} variables.}
  \item{alpha.x}{The mixing parameter related to the sparsity within group for the \eqn{X} dataset.}
    \item{alpha.y}{The mixing parameter related to the sparsity within group for the \eqn{Y} dataset.}
    \item{upper.lambda}{The upper bound of the intervall of lambda values for searching the value of the tuning parameter (lambda) corresponding to a non-zero group of variables.}
}

\references{
Liquet Benoit, Lafaye de Micheaux, Boris Hejblum, Rodolphe Thiebaut (2016). A group and Sparse Group Partial Least Square approach applied in Genomics context. \emph{Bioinformatics}.

L\^e Cao, K.-A., Martin, P.G.P., Robert-Grani\'e, C. and Besse, P. (2009). Sparse canonical methods for biological data integration: application to a cross-platform study. \emph{BMC Bioinformatics} \bold{10}:34.

L\^e Cao, K.-A., Rossouw, D., Robert-Grani\'e, C. and Besse, P. (2008). A sparse PLS for variable 
selection when integrating Omics data. \emph{Statistical Applications in Genetics and Molecular 
Biology} \bold{7}, article 35.

Shen, H. and Huang, J. Z. (2008). Sparse principal component analysis via regularized 
low rank matrix approximation. \emph{Journal of Multivariate Analysis} \bold{99}, 1015-1034.    

Tenenhaus, M. (1998). \emph{La r\'egression PLS: th\'eorie et pratique}. Paris: Editions Technic.

Wold H. (1966). Estimation of principal components and related models by iterative least squares. 
In: Krishnaiah, P. R. (editors), \emph{Multivariate Analysis}. Academic Press, N.Y., 391-420.
}

\author{Benoit Liquet and Pierre Lafaye de Micheaux.}

\seealso{\code{\link{sPLS}}, \code{\link{sgPLS}}, \code{\link{predict}}, \code{\link{perf}} and functions from \code{mixOmics} package: \code{summary}, \code{plotIndiv}, \code{plotVar}, \code{plot3dIndiv}, \code{plot3dVar}.}

\examples{
	
## Simulation of datasets X and Y with group variables
n <- 100
sigma.gamma <- 1
sigma.e <- 1.5
p <- 400
q <- 500
theta.x1 <- c(rep(1,15),rep(0,5),rep(-1,15),rep(0,5),rep(1.5,15)
             ,rep(0,5),rep(-1.5,15),rep(0,325))
theta.x2 <- c(rep(0,320),rep(1,15),rep(0,5),rep(-1,15),rep(0,5)
             ,rep(1.5,15),rep(0,5),rep(-1.5,15),rep(0,5))

theta.y1 <- c(rep(1,15),rep(0,5),rep(-1,15),rep(0,5),rep(1.5,15)
             ,rep(0,5),rep(-1.5,15),rep(0,425))
theta.y2 <- c(rep(0,420),rep(1,15),rep(0,5),rep(-1,15),rep(0,5),
			rep(1.5,15),rep(0,5),rep(-1.5,15),rep(0,5))                             


Sigmax <- matrix(0, nrow = p, ncol = p)
diag(Sigmax) <- sigma.e ^ 2
Sigmay <- matrix(0, nrow = q, ncol = q)
diag(Sigmay) <- sigma.e ^ 2

set.seed(125)

gam1 <- rnorm(n)
gam2 <- rnorm(n)

X <- matrix(c(gam1, gam2), ncol = 2, byrow = FALSE) \%*\% matrix(c(theta.x1, theta.x2),
     nrow = 2, byrow = TRUE) + rmvnorm(n, mean = rep(0, p), sigma =
     Sigmax, method = "svd")
Y <- matrix(c(gam1, gam2), ncol = 2, byrow = FALSE) \%*\% matrix(c(theta.y1, theta.y2),
     nrow = 2, byrow = TRUE) + rmvnorm(n, mean = rep(0, q), sigma =
     Sigmay, method = "svd")


ind.block.x <- seq(20, 380, 20)
ind.block.y <- seq(20, 480, 20)
##


model.sgPLS <- sgPLS(X, Y, ncomp = 2, mode = "regression", keepX = c(4, 4), 
                   keepY = c(4, 4), ind.block.x = ind.block.x
                   ,ind.block.y = ind.block.y,
                   alpha.x = c(0.95, 0.95), alpha.y = c(0.95, 0.95))

result.sgPLS <- select.sgpls(model.sgPLS)
result.sgPLS$group.size.X
result.sgPLS$group.size.Y
}

\keyword{regression}
\keyword{multivariate}
