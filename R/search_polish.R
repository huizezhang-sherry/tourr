#' Polish the searching after search_geodesic
#'
#' @param current starting projection
#' @param alpha_polish maximum distance to travel (currently ignored)
#' @param index interestingness index function
#' @param max.tries maximum number of basis to search
#' @param method linear or geodesic nearby basis
#' @keywords optimize
search_polish <- function(current, alpha_polish = 0.1, index, data, max_tries = 5000,
                          method = "linear"){
  method <- match.arg(method, c("linear", "geodesic"))
  if (is.na(cur_index)) cur_index <- index(current)
  new_basis <- list()
  new_index <- c()

  for(i in 1:max_tries){
    new_basis[[i]] <- basis_nearby(current, alpha_polish, method)
    new_index[i] <- index(new_basis[[i]])

    list(new_basis, new_index)
  }

  sample <- tibble::tibble(basis = new_basis, index_val = new_index)

  if (new_index > cur_index){
    return(sample[which.max(sample$index_val),]$basis[[1]])
  }else{
    return(current)
  }

}
