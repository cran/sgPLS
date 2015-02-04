plotcim <- function (matX, matY,cexCol=0.5,cexRow=1) 
{
  labelY <- colnames(matY)
  mat.sim <- matrix(NA, ncol = dim(matX)[2], nrow = length(labelY))
  for (i in 1:dim(matX)[2]) {
    for (j in 1:length(labelY)) {
      mat.sim[j, i] <- cor(matX[, i], matY[, j])
    }
  }
  cim(mat.sim, labRow = labelY, labCol = colnames(matX),cexCol=cexCol,cexRow=cexRow)
}