\name{select.spls}
\alias{select.spls}
\title{Output of selected variables from a sPLS model}
\usage{
  select.spls(model)
}
\arguments{
  \item{model}{object of class inheriting from \code{"sPLS"}.}
   
}
\description{
This function outputs the selected variables on each component for the sPLS. 
}

\value{
  \code{select.spls} produces a list with the following components: 
  \item{select.X}{A list containing for each element (corresponding to each component of the sPLS model) the names of the selected variables in the \eqn{X} dataset.}
  \item{select.Y}{A list containing for each element (corresponding to each component of the sPLS model) the names of the selected variables in the \eqn{Y} dataset.}
  \item{select.X.total}{The names of the variables selected from the sPLS model regarding the \eqn{X} matrix.}
  \item{select.Y.total}{The names of the variables selected from the sPLS model regarding the \eqn{Y} matrix.}
}



\author{
Benoit Liquet, \email{b.liquet@uq.edu.au}, \cr Pierre Lafaye de Micheaux \email{lafaye@dms.umontreal.ca}
}


\examples{
\dontrun{	
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

temp <-  matrix(c(theta.y1, theta.y2), nrow = 2, byrow = TRUE)

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
}