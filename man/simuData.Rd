\name{simuData}
\encoding{latin1}
\alias{simuData}

\docType{data}

\title{Simulated Data for group PLS-DA model}

\description{
This simulated data set contains the expression of 1000 genes for 4 clusters from 48 different individuals. 
}

\usage{data(simuData)}

\format{
A list containing the following components:
  \describe{
    \item{\code{X}}{data matrix with 48 rows and 1000 columns. Each row represents 
      an experimental sample, and each column a single gene.}
	\item{\code{Y}}{a factor variable indicating the cluster of each subject}
  }
}

\details{
This data have been simulated such that only 6 groups of 100 genes are linked to the 4 clusters. The others 4 groups of 100 genes has been added to represent some noise. The relevant groups are the group 1,2,4,6,7 and 9. The groups 3,5,8, and 10 are noise groups.
}





\keyword{datasets}
