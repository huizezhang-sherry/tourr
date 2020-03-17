#' calculate index value given the dataset and projection bases using holes()

#' created to allow calculate index outside the whole loop
#'
#' @param data the initial data used
#' @param proj the projection basis
#' @param sphere a binary indicator whether the data need to be sphered

calc_index <- function(data, proj, sphere = TRUE) {

  if(sphere){
    data <- sphere_data(rescale(data))
  }

  mat <- as.matrix(data) %*% proj

  n <- nrow(mat)
  d <- ncol(mat)

  num <- 1 - 1/n * sum(exp(-0.5 * rowSums(mat ^ 2)))
  den <- 1 - exp(-d / 2)

  num / den
}
