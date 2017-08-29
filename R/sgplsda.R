sgPLSda <-
function(X, 
         Y,		
         ncomp = 2, 
         keepX = rep(ncol(X), ncomp),
         max.iter = 500,		 
         tol = 1e-06,
         ind.block.x, alpha.x = alpha.x, upper.lambda = 10^5)
{
  		
    # Testing the input Y
    if (is.null(dim(Y)))
    {
        Y = as.factor(Y)	
        ind.mat = unmap(as.numeric(Y))					
    }else {
        stop("'Y' should be a factor or a class vector.")						
    }		

    result = sgPLS(X, ind.mat, ncomp = ncomp, mode = "regression", keepX = keepX, 
                  max.iter = max.iter, tol = tol,ind.block.x = ind.block.x,alpha.x = alpha.x,upper.lambda = upper.lambda)
       
    cl = match.call()
    cl[[1]] = as.name('sgPLSda')
    result$call = cl
	 
    result$ind.mat = ind.mat
    result$names$Y = levels(Y)
    
    class(result) = c("sgPLSda","gPLSda","splsda","plsda")
    return(invisible(result))	
}

