\name{predict}
\encoding{latin1}
\alias{predict.sgPLS}
\alias{predict.gPLS}
\alias{predict.sPLS}
\alias{predict.sgPLSda}
\alias{predict.gPLSda}
\alias{predict.sPLSda}

\title{Predict Method for sPLS, gPLS, sgPLS, sPLDda, gPLSda, sgPLSda}

\description{
Predicted values based on sparse PLS, group PLS, sparse group PLS, sparse PLSda, group PLSda, sparse group PLSda models. New responses and 
variates are predicted using a fitted model and a new matrix of observations.
}

\usage{

\method{predict}{sPLS}(object, newdata, ...)

\method{predict}{gPLS}(object, newdata, ...)

\method{predict}{sgPLS}(object, newdata, ...)

\method{predict}{sPLSda}(object, newdata, method = c("all", "max.dist", 
        "centroids.dist", "mahalanobis.dist"), ...)

\method{predict}{gPLSda}(object, newdata, method = c("all", "max.dist", 
        "centroids.dist", "mahalanobis.dist"), ...)

\method{predict}{sgPLSda}(object, newdata, method = c("all", "max.dist", 
        "centroids.dist", "mahalanobis.dist"), ...)




}

\arguments{
  \item{object}{object of class inheriting from \code{"sPLS"}, \code{"gPLS"}, \code{"sgPLS"},  \code{"sPLSda"}, \code{"gPLSda"} or  \code{"sgPLSda"}.}
  \item{newdata}{data matrix in which to look for for explanatory variables to be used for prediction.}
   \item{method}{method to be applied for \code{sPLSda}, \code{gPLSda} or \code{sgPLSda} to predict the class of new data, 
    should be a subset of \code{"centroids.dist"}, \code{"mahalanobis.dist"} or \code{"max.dist"} (see Details). 
	Defaults to \code{"all"}.}
  \item{...}{not used currently.}
}

\details{
The \code{predict} function for pls and spls object has been created by Sebastien Dejean, Ignacio Gonzalez, Amrit Singh and Kim-Anh Le Cao for \code{mixOmics} package. Similar code is used for sPLS, gPLS, sgPLS, sPLSda, gPLSda, sgPLSda models performed by \code{sgPLS} package. 	
	
\code{predict} function produces predicted values, obtained by evaluating the sparse PLS, group PLS or sparse group PLS  
model returned by \code{sPLS}, \code{gPLS} or \code{sgPLS} in the frame \code{newdata}. 
Variates for \code{newdata} are also returned. The prediction values are calculated based on the regression coefficients of \code{object$Y} onto \code{object$variates$X}.


Different class prediction methods are proposed for \code{sPLSda}, \code{gPLSda} or \code{sgPLSda}: \code{"max.dist"} 
is the naive method to predict the class. It is based on the predicted matrix (\code{object$predict}) 
which can be seen as a probability matrix to assign each test data to a class. The class with the largest 
class value is the predicted class. \code{"centroids.dist"} allocates the individual \eqn{x} to the class of 
\eqn{Y} minimizing \eqn{dist(\code{x-variate}, G_l)}, where \eqn{G_l}, \eqn{l = 1,...,L} are the centroids of 
the classes calculated on the \eqn{X}-variates of the model. \code{"mahalanobis.dist"} allocates the individual 
\eqn{x} to the class of \eqn{Y} as in \code{"centroids.dist"} but by using the Mahalanobis metric 
in the calculation of the distance.
}


\value{
\code{predict} produces a list with the following components: 
  \item{predict}{A three dimensional array of predicted response values. The dimensions 
	correspond to the observations, the response variables and the model dimension, respectively.}
  \item{variates}{Matrix of predicted variates.}
  \item{B.hat}{Matrix of regression coefficients (without the intercept).}
  \item{class}{vector or matrix of predicted class by using \eqn{1,...,}\code{ncomp} 
    (sparse)PLS-DA components.}
  \item{centroids}{matrix of coordinates for centroids.}
}

\references{
Tenenhaus, M. (1998). \emph{La r\'egression PLS: th\'eorie et pratique}. Paris: Editions Technic.  
}

\author{Benoit Liquet and Pierre Lafaye de Micheaux}


\seealso{\code{\link{sPLS}}, \code{\link{gPLS}}, \code{\link{sgPLS}}, \code{\link{sPLSda}}, \code{\link{gPLSda}}, \code{\link{sgPLSda}}.}


\keyword{regression}
\keyword{multivariate}
