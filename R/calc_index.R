#' calculate index value given the dataset and projection bases using holes()

#' created to allow calculate index outside the whole loop
#'
#' @param data the initial data used
#' @param proj the projection basis
#' @param sphere a binary indicator whether the data need to be sphered

calc_index <- function(data, proj, sphere = FALSE) {

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



calc_kol <- function(data, proj, sphere = FALSE) {

  if(sphere){
    data <- sphere_data(rescale(data))
  }

  mat <- as.matrix(data) %*% proj

  set.seed(123)
  mat_bin_count <- bin1(mat, c(min(mat), max(mat)), 10)$nc
  norm_bin_count <- bin1(rnorm(nrow(mat)), c(min(mat), max(mat)), 10)$nc
  diff <- sum((mat_bin_count - norm_bin_count)^2)/nrow(mat)

  diff
}

calc_kol_cdf <- function(data, proj, sphere = FALSE) {

  if(sphere){
    data <- sphere_data(rescale(data))
  }

  mat <- as.matrix(data) %*% proj

  set.seed(123)
  norm <- rnorm(nrow(mat))

  as.numeric(ks.test(mat, norm)$statistic)
  # ecdf_mat <- ecdf(mat)
  # ecdf_norm <- ecdf(norm)
  #
  # ordered_ecdf_mat <- ecdf_mat(mat)[order(ecdf_mat(mat))]
  # ordered_ecdf_norm <- ecdf_norm(norm)[order(ecdf_norm(norm))]
  #
  # sum(diff(ordered_ecdf_mat - ordered_ecdf_norm) < 0)/nrow(mat)
}
