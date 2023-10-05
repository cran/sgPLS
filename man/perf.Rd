\name{perf}
\encoding{latin1}
\alias{perf}
\alias{perf.sPLS}
\alias{perf.gPLS}
\alias{perf.sgPLS}
\alias{perf.sPLSda}
\alias{perf.gPLSda}
\alias{perf.sgPLSda}

\title{Compute evaluation criteria for PLS, sPLS, PLS-DA and sPLS-DA}

\description{
Function to evaluate the performance of the fitted sparse PLS, group PLS, sparse group PLS, sparse PLS-DA, group PLS-DA and sparse group PLS-DA models using various criteria.
}

\usage{
           

\method{perf}{sPLS}(object, 
          criterion = c("all", "MSEP", "R2", "Q2"), 
          validation = c("Mfold", "loo"), 
          folds = 10, progressBar = TRUE, setseed = 1,\ldots)
          
\method{perf}{gPLS}(object, 
          criterion = c("all", "MSEP", "R2", "Q2"), 
          validation = c("Mfold", "loo"), 
          folds = 10, progressBar = TRUE, setseed = 1, \ldots)
          
\method{perf}{sgPLS}(object, 
          criterion = c("all", "MSEP", "R2", "Q2"), 
          validation = c("Mfold", "loo"), 
          folds = 10, progressBar = TRUE,setseed = 1, \ldots)
          
          
\method{perf}{sPLSda}(object,
          method.predict = c("all", "max.dist", "centroids.dist", "mahalanobis.dist"),
          validation = c("Mfold", "loo"), 
          folds = 10, progressBar = TRUE, \ldots)	

\method{perf}{gPLSda}(object,
          method.predict = c("all", "max.dist", "centroids.dist", "mahalanobis.dist"),
          validation = c("Mfold", "loo"), 
          folds = 10, progressBar = TRUE, \ldots)
          
\method{perf}{sgPLSda}(object,
          method.predict = c("all", "max.dist", "centroids.dist", "mahalanobis.dist"),
          validation = c("Mfold", "loo"), 
          folds = 10, progressBar = TRUE, \ldots)                  
          
          
  
}

\arguments{
  \item{object}{Object of class inheriting from \code{"sPLS"}, \code{"gPLS"}, \code{"sgPLS"}, \code{"sPLSda"}, \code{"gPLSda"} or \code{"sgPLSda"}. The function will retrieve some key parameters stored in that object.}
    \item{criterion}{The criteria measures to be calculated (see Details). Can be set to either \code{"all"}, \code{"MSEP"}, \code{"R2"}, \code{"Q2"}. By default set to \code{"all"}. Only applies to an object inheriting from \code{"sPLS"}, \code{"gPLS"} or \code{"sgPLS"}}
  \item{method.predict}{only applies to an object inheriting from \code{"PLSda"}, \code{"gPLSda"} or \code{"sgPLSda"} to evaluate the classification performance of the model. Should be a subset of \code{"max.dist"}, \code{"centroids.dist"}, \code{"mahalanobis.dist"}. Default is \code{"all"}. See \code{\link{predict}}.}
  \item{validation}{Character.  What kind of (internal) validation to use, matching one of \code{"Mfold"} or 
    \code{"loo"} (see below). Default is \code{"Mfold"}.}
  \item{folds}{The folds in the Mfold cross-validation. See Details.}
  \item{progressBar}{By default set to \code{TRUE} to output the progress bar of the computation.}
  \item{setseed}{Integer value to specify the random generator state.}
  \item{...}{Not used at the moment.}
}

\details{
The method \code{perf} has been created by Sebastien Dejean, Ignacio Gonzalez, Amrit Singh and Kim-Anh Le Cao for pls and spls models performed by \code{mixOmics} package. Similar code has been adapted for sPLS, gPLS and sgPLS in the package \code{sgPLS}. 	
	
	
\code{perf} estimates the 
mean squared error of prediction (MSEP), \eqn{R^2}, and \eqn{Q^2} to assess the predictive 
performance of the model using M-fold or leave-one-out cross-validation. Note that only the \code{classic}, \code{regression} and  \code{invariant} modes can be applied.

If \code{validation = "Mfold"}, M-fold cross-validation is performed. 
How many folds to generate is selected by specifying the number of folds in \code{folds}.
The folds also can be supplied as a list of vectors containing the indexes defining each 
fold as produced by \code{split}.
If \code{validation = "loo"}, leave-one-out cross-validation is performed.

For fitted sPLS-DA, gPLS-DA and sgPLS-DA models, \code{perf} estimates the classification error rate 
using cross-validation. 

Note that the \code{perf} function will retrieve the \code{keepX} and \code{keepY} inputs from the previously run object. The sPLS, gPLS, sgPLS, sPLSda, gPLSda or sgPLSda functions will be run again on several and different subsets of data (the cross-folds) and certainly on different subset of selected features. For sPLS, the MSEP, \eqn{R^2}, and \eqn{Q^2} criteria are averaged across all folds. A feature stability measure is output for the user to assess how often the variables are selected across all folds. For sPLS-DA, the classification erro rate is averaged across all folds.

}

\value{
\code{perf} produces a list with the following components: 
  \item{MSEP}{Mean Square Error Prediction for each \eqn{Y} variable, only applies to object inherited from \code{"sPLS"}, \code{"gPLS"} and \code{"sgPLS"}.}
  \item{R2}{a matrix of \eqn{R^2} values of the \eqn{Y}-variables for models 
    with \eqn{1, \ldots ,}\code{ncomp} components, only applies to object inherited from \code{"sPLS"}, \code{"gPLS"} and \code{"sgPLS"}.}
  \item{Q2}{if \eqn{Y} contains one variable, a vector of \eqn{Q^2} values else a list with 
    a matrix of \eqn{Q^2} values for each \eqn{Y}-variable. Note that in the specific case of an sPLS model, it is better to have a look at the Q2.total criterion, only applies to object inherited from from \code{"sPLS"}, \code{"gPLS"} and \code{"sgPLS"}.}
    \item{Q2.total}{a vector of \eqn{Q^2}-total values for models with \eqn{1, \ldots ,}\code{ncomp} components, only applies to object inherited from from \code{"sPLS"}, \code{"gPLS"} and \code{"sgPLS"}.}
    \item{features}{a list of features selected across the folds (\code{$stable.X} and \code{$stable.Y}) or on the whole data set (\code{$final}) for the \code{keepX} and \code{keepY} parameters from the input object.}
    \item{error.rate}{
For sPLS-DA, gPLS-DA and sgPLS-DA models, \code{perf} produces a matrix of classification error rate estimation. 
The dimensions correspond to the components in the model and to the prediction method used, respectively. Note that error rates reported in any component include the performance of the model in earlier components for the specified \code{keepX} parameters (e.g. error rate reported for component 3 for \code{keepX = 20} already includes the fitted model on components 1 and 2 for  \code{keepX = 20}). For more advanced usage of the \code{perf} function, see mixOmics package and consider using the \code{predict} function.}

}

\references{	
	
Tenenhaus, M. (1998). \emph{La r\'egression PLS: th\'eorie et pratique}. Paris: Editions Technic. 

Le Cao, K.-A., Rossouw, D., Robert-Grani\'e, C. and Besse, P. (2008). A sparse PLS for variable 
selection when integrating Omics data. \emph{Statistical Applications in Genetics and Molecular 
Biology} \bold{7}, article 35.

Mevik, B.-H., Cederkvist, H. R. (2004). Mean Squared Error of Prediction (MSEP) Estimates for Principal Component 
Regression (PCR) and Partial Least Squares Regression (PLSR). \emph{Journal of Chemometrics} \bold{18}(9), 422-429.
}

\author{Benoit Liquet and Pierre Lafaye de Micheaux}

\seealso{\code{\link{predict}}, \code{plot.perf} (from package \code{mixOmics})}

\examples{
## validation for objects of class 'sPLS' (regression)
## Example from mixOmics package 
# ----------------------------------------
\dontrun{
data(liver.toxicity)
X <- liver.toxicity$gene
Y <- liver.toxicity$clinic


## validation for objects of class 'spls' (regression)
# ----------------------------------------
ncomp <- 7
# first, learn the model on the whole data set
model.spls <- sPLS(X, Y, ncomp = ncomp, mode = 'regression',
	 keepX = c(rep(5, ncomp)), keepY = c(rep(2, ncomp)))


# with leave-one-out cross validation
set.seed(45)
model.spls.loo.val <- perf(model.spls, validation = "loo")

#Q2 total
model.spls.loo.val$Q2.total

# R2:we can see how the performance degrades when ncomp increases
# results are similar to 5-fold
model.spls.loo.val$R2

}

}

\keyword{regression}
\keyword{multivariate}
