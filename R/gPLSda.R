gPLSda <-
  function(X, 
           Y,		
           ncomp = 2, 
           keepX = rep(ncol(X), ncomp),
           max.iter = 500,		 
           tol = 1e-06,
           ind.block.x)
  {
    
    # Testing the input Y
    if (is.null(dim(Y)))
    {
      Y = as.factor(Y)	
      ind.mat = unmap(as.numeric(Y))					
    }else {
      stop("'Y' should be a factor or a class vector.")						
    }		
    
    result = gPLS(X, ind.mat, ncomp = ncomp, mode = "regression", keepX = keepX, 
                   max.iter = max.iter, tol = tol,ind.block.x = ind.block.x)
    
    cl = match.call()
    cl[[1]] = as.name('gPLSda')
    result$call = cl
    
    result$ind.mat = ind.mat
    result$names$Y = levels(Y)
    
    class(result) = c("gPLSda","splsda","plsda")
    return(invisible(result))	
  }