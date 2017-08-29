sPLSda <-
function(X, 
         Y,		
         ncomp = 2, 
         keepX = rep(ncol(X), ncomp),
         max.iter = 500,		 
         tol = 1e-06)
{
  		
    # Testing the input Y
    if (is.null(dim(Y)))
    {
        Y = as.factor(Y)	
        ind.mat = unmap(as.numeric(Y))					
    }else {
        stop("'Y' should be a factor or a class vector.")						
    }		

    result = sPLS(X, ind.mat, ncomp = ncomp, mode = "regression", keepX = keepX, 
                  max.iter = max.iter, tol = tol)
       
    cl = match.call()
    cl[[1]] = as.name('sPLSda')
    result$call = cl
	 
    result$ind.mat = ind.mat
    result$names$Y = levels(Y)
    
    class(result) = c("sPLSda","splsda","plsda")
    return(invisible(result))	
}

