\name{predict}
\encoding{latin1}
\alias{predict.sgPLS}
\alias{predict.gPLS}
\alias{predict.sPLS}

\title{Predict Method for sPLS, gPLS or sgPLS}

\description{
Predicted values based on sparse PLS, group PLS, sparse group PLS models. New responses and 
variates are predicted using a fitted model and a new matrix of observations.
}

\usage{

\method{predict}{sPLS}(object, newdata, ...)

\method{predict}{gPLS}(object, newdata, ...)

\method{predict}{sgPLS}(object, newdata, ...)


}

\arguments{
  \item{object}{object of class inheriting from \code{"sPLS"}, \code{"gPLS"}or \code{"sgPLS"}.}
  \item{newdata}{data matrix in which to look for for explanatory variables to be used for prediction.}
  \item{...}{not used currently.}
}

\details{
The \code{predict} function for pls and spls object has been created by Sebastien Dejean, Ignacio Gonzalez, Amrit Singh and Kim-Anh Le Cao for \code{mixOmics} package. Similar code is used for sPLS, gPLS and sgPLS models performed by \code{sgPLS} package. 	
	
\code{predict} function produces predicted values, obtained by evaluating the sparse PLS, group PLS or sparse group PLS  
model returned by \code{sPLS}, \code{gPLS} or \code{sgPLS} in the frame \code{newdata}. 
Variates for \code{newdata} are also returned. The prediction values are calculated based on the regression coefficients of \code{object$Y} onto \code{object$variates$X}.
}

\value{
\code{predict} produces a list with the following components: 
  \item{predict}{A three dimensional array of predicted response values. The dimensions 
	correspond to the observations, the response variables and the model dimension, respectively.}
  \item{variates}{Matrix of predicted variates.}
  \item{B.hat}{Matrix of regression coefficients (without the intercept).}
}

\references{
Tenenhaus, M. (1998). \emph{La r\'egression PLS: th\'eorie et pratique}. Paris: Editions Technic.  
}

\author{Benoit Liquet and Pierre Lafaye de Micheaux}


\seealso{\code{\link{sPLS}}, \code{\link{gPLS}}, \code{\link{sgPLS}}.}


\keyword{regression}
\keyword{multivariate}
