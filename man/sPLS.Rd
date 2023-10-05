\name{sPLS}
\encoding{latin1}
\alias{sPLS}

\title{Sparse Partial Least Squares (sPLS)}

\description{Function to perform sparse Partial Least Squares (sPLS). The sPLS approach
combines both integration and variable selection simultaneously on two data sets
in a one-step strategy.
}

\usage{
sPLS(X, Y, ncomp, mode = "regression",
     max.iter = 500, tol = 1e-06, keepX = rep(ncol(X), ncomp), 
     keepY = rep(ncol(Y), ncomp),scale=TRUE)
}	

\arguments{
  \item{X}{Numeric matrix of predictors.}
  \item{Y}{Numeric vector or matrix of responses (for multi-response models).}
  \item{ncomp}{The number of components to include in the model (see Details).}
  \item{mode}{Character string. What type of algorithm to use, (partially) matching 
    one of \code{"regression"} or \code{"canonical"}. See Details.}
  \item{max.iter}{Integer, the maximum number of iterations.}
  \item{tol}{A positive real, the tolerance used in the iterative algorithm.}
  \item{keepX}{Numeric vector of length \code{ncomp}, the number of variables
    to keep in \eqn{X}-loadings. By default all variables are kept in the model.}
  \item{keepY}{Numeric vector of length \code{ncomp}, the number of variables
    to keep in \eqn{Y}-loadings. By default all variables are kept in the model.}
    \item{scale}{a logical indicating if the orignal data set need to be scaled. By default \code{scale}=TRUE}

}

\details{
\code{sPLS} function fit sPLS models with \eqn{1, \ldots ,}\code{ncomp} components. 
Multi-response models are fully supported.

The type of algorithm to use is specified with the \code{mode} argument. Two sPLS 
algorithms are available: sPLS regression \code{("regression")} and sPLS canonical analysis 
\code{("canonical")} (see References). 

}

\value{
\code{sPLS} returns an object of class \code{"sPLS"}, a list 
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
  \item{tol}{The tolerance used in the iterative algorithm, used for subsequent S3 methods}
  \item{max.iter}{The maximum number of iterations, used for subsequent S3 methods}

}

\references{
Liquet Benoit, Lafaye de Micheaux Pierre, Hejblum Boris, Thiebaut Rodolphe. A group and Sparse Group Partial Least Square approach applied in Genomics context. \emph{Submitted}.

Le Cao, K.-A., Martin, P.G.P., Robert-Grani\', C. and Besse, P. (2009). Sparse canonical methods for biological data integration: application to a cross-platform study. \emph{BMC Bioinformatics} \bold{10}:34.

Le Cao, K.-A., Rossouw, D., Robert-Grani\'e, C. and Besse, P. (2008). A sparse PLS for variable 
selection when integrating Omics data. \emph{Statistical Applications in Genetics and Molecular 
Biology} \bold{7}, article 35.

Shen, H. and Huang, J. Z. (2008). Sparse principal component analysis via regularized 
low rank matrix approximation. \emph{Journal of Multivariate Analysis} \bold{99}, 1015-1034.    

Tenenhaus, M. (1998). \emph{La r\'egression PLS: th\'eorie et pratique}. Paris: Editions Technic.

Wold H. (1966). Estimation of principal components and related models by iterative least squares. 
In: Krishnaiah, P. R. (editors), \emph{Multivariate Analysis}. Academic Press, N.Y., 391-420.
}

\author{Benoit Liquet and Pierre Lafaye de Micheaux.}

\seealso{\code{\link{gPLS}}, \code{\link{sgPLS}}, \code{\link{predict}}, \code{\link{perf}} and functions from \code{mixOmics} package: \code{summary}, \code{plotIndiv}, \code{plotVar}, \code{plot3dIndiv}, \code{plot3dVar}.}

\examples{

## Simulation of datasets X and Y with group variables
n <- 100
sigma.gamma <- 1
sigma.e <- 1.5
p <- 400
q <- 500
theta.x1 <- c(rep(1, 15), rep(0, 5), rep(-1, 15), rep(0, 5),
			rep(1.5, 15), rep(0, 5), rep(-1.5, 15), rep(0, 325))
theta.x2 <- c(rep(0, 320), rep(1, 15), rep(0, 5), rep(-1, 15),
			rep(0, 5), rep(1.5, 15), rep(0, 5), rep(-1.5, 15),
			rep(0, 5))

theta.y1 <- c(rep(1, 15), rep(0, 5), rep(-1, 15), rep(0, 5), 
			rep(1.5, 15), rep(0, 5), rep(-1.5, 15), rep(0, 425))
theta.y2 <- c(rep(0, 420), rep(1, 15), rep(0, 5), rep(-1, 15)
			,rep(0, 5), rep(1.5, 15), rep(0, 5), rep(-1.5, 15)
			, rep(0, 5))                            


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


#### sPLS model
model.sPLS <- sPLS(X, Y, ncomp = 2, mode = "regression", keepX = c(60, 60), 
                     keepY = c(60, 60))
result.sPLS <- select.spls(model.sPLS)
result.sPLS$select.X
result.sPLS$select.Y

}

\keyword{regression}
\keyword{multivariate}
