search_geodesic <- function(current, alpha = 1, index, max.tries = 5, n = 5, stepS = 0.01, cur_index = NA) {
  if (is.na(cur_index)) cur_index <- index(current)

  try <- 1
  while(try < max.tries) {
    direction <- list()
    peak <- list()
    record <- numeric(0)


    # Try 5 random directions and pick the one that has the highest
    # index after a small step in either direction
    direction[[try]] <- find_best_dir(current, index, tries = n, dist=stepS)

    # Travel halfway round (pi / 4 radians) the sphere in that direction
    # looking for the best projection
    peak[[try]] <- find_path_peak(current, direction, index)

    # init a record vector, record the past five peak value
    # if the new peak deviates the largest of the past 5 by eps = 0.001,
    # use the largest of the past 5 as current record


    record[try] <- peak[[try]]$index

    if(try > 5){
      past_five_max <- max(record[try - 5: try -1])

      if(past_fix_max - peak$index > 0.001){

        record[try] <- past_fix_max
      }
    }

    direction[[try]] <- direction[[which.max(record[try-5: try-1])]]
    peak[[try]] <-  peak[[which.max(record[try - 5: try-1])]]

    cat("peak: ", peak[[try]])






    pdiff <- (peak$index - cur_index) / cur_index

    dig3 <- function(x) sprintf("%.3f", x)

    cat("Value ", dig3(peak$index), " ",
        sprintf("%.1f", pdiff * 100), "% better ",
        "(", dig3(peak$dist), " away)", sep="")
    if (pdiff > 0.001) {
      cat(" - NEW BASIS\n")
      return(peak$basis)
    }
    cat("\n")

    try <- try + 1
  }
  cat("No better bases found after ", max.tries, " tries.  Giving up.\n",
      sep="")
  cat("Final projection: \n")
  if (ncol(current)==1) {
    for (i in 1:length(current))
      cat(sprintf("%.3f",current[i])," ")
    cat("\n")
  }
  else {
    for (i in 1:nrow(current)) {
      for (j in 1:ncol(current))
        cat(sprintf("%.3f",current[i,j])," ")
      cat("\n")
    }
  }

  NULL
}

#' Find the most promising direction to travel in.
#'
#' Starting from the current projection, pick \code{tries} random location
#' and take a small step towards and away from each location.  The most
#' promising direction has the highest value of the \code{index} function.
#'
#' @keywords optimize internal
#' @param old current projection
#' @param index interestingness index function
#' @param dist step size in radians, should be small
#' @param number of random steps to take
find_best_dir <- function(old, index, dist = 0.01, tries = 5) {
  bases <- replicate(tries, basis_random(nrow(old), ncol(old)),
                     simplify = FALSE)

  score <- function(new) {
    interpolator <- geodesic_info(old, new)
    forward <- step_angle(interpolator, dist)
    backward <- step_angle(interpolator, -dist)

    max(index(forward), index(backward))
  }
  scores <- sapply(bases, score)
  bases[[which.max(scores)]]
}

#' Find the most interesting projection along a geodesic.
#'
#' Use \code{\link{optimize}} to find the most interesting projection amongst
#' all projections on a geodesic.  This method assumes that the function is
#' continuous with a single maximum, but seems to do ok even if there are
#' multiple maxima.
#'
#' @param old currention project
#' @param new projection that gives direction to travel in
#' @param index interestingness index function
#' @param max_dist maximum distance to travel along in radians
#' @keywords optimize internal
find_path_peak <- function(old, new, index, max_dist = pi / 4) {
  interpolator <- geodesic_info(old, new)

  index_pos <- function(alpha) index(step_angle(interpolator, alpha))
  alpha <- stats::optimize(index_pos, c(-max_dist, max_dist), maximum = TRUE, tol = 0.01)

  list(
    basis = step_angle(interpolator, alpha$maximum),
    index = alpha$objective,
    dist = abs(alpha$maximum)
  )
}
