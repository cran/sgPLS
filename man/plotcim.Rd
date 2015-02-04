\name{plotcim}
\alias{plotcim}
\title{Plots a cluster image mapping of correlations between
  outcomes and all predictors}
\usage{
  plotcim(matX, matY, cexCol = 0.5, cexRow = 1)
}
\arguments{
  \item{matX}{data frame corresponding to the predictors.}

  \item{matY}{data frame corresponding to the outcomes.}
  
   \item{cexRow, cexCol}{positive numbers, used as \code{cex.axis} in for the row or column 
	axis labeling. The defaults currently only use number of rows or columns, respectively.}
  
  
}
\description{
  The \code{plotcim} function plots a cluster image
  mapping of correlations between outcomes and all the predictors.
}
\details{
  To be used with a small number of predictors (<1,000).
}

\author{
Benoit Liquet, \email{b.liquet@uq.edu.au}, \cr Pierre Lafaye de Micheaux \email{lafaye@dms.umontreal.ca}
}
