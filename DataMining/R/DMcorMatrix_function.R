#' A Correlation Matrix Function
#'
#' This function allows you to construct a correlation Matrix
#' @param Data, lower, upper, cut_off
#' @keywords Correlation matrix, variable
#' @export
#' @examples
#' x=rnorm(100,5)
#' y=rnorm(100,5)
#' z=rnorm(100,5)
#' d=data.frame(x,y,z)
#' DMcorMatrix(data=d, lower=1, upper=3, cut_off=0)


DMcorMatrix <-
function(data, lower, upper, cut_off){
  if (dim(data)[1] > 0) {
    corMatrix = cor(data[,c(lower:upper)])
    #loop through correlation matrix
    for (i in 1:dim(corMatrix)[1]) {
      for (j in 1:dim(corMatrix)[2]){
        if ((abs(corMatrix[i,j]) < cut_off | i==j)&(cut_off!=0)){
          corMatrix[i,j]=NA
        } else if ((abs(corMatrix[i,j]) < cut_off | i==j)&(cut_off!=0)) {
          corMatrix[i,j] = corMatrix[i,j]
        }
      }
    }
    corMatrix <- corMatrix[, colSums(is.na(corMatrix)) < dim(corMatrix)[1]]
    corMatrix <- corMatrix[rowSums(is.na(corMatrix)) < dim(corMatrix)[2],]
    corMatrix
  }
}
