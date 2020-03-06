#' Search for most interesting projection along random geodesics.
#'
#' This is a novel method for finding more interesting projections for the
#' guided tour.  It works by first taking a small step in \code{n} random
#' directions, and then picking the direction that looks most promising
#' (based on the height of the index function).  Once the best direction is
#' found, it performs a linear search along the geodesic in that direction,
#' traveling up to half way around the sphere.
#'
#' You should not to have call this function directly, but should supply it
#' to the \code{\link{guided_tour}} as a search strategy.
#'
#' @param current starting projection
#' @param alpha maximum distance to travel (currently ignored)
#' @param index interestingness index function
#' @param max.tries maximum number of failed attempts before giving up
#' @param n number of random steps to take to find best direction
#' @param stepS step size for evaluation of best direction
#' @param cur_index index value for starting projection, set NA if it needs to
#'   be calculated
#' @keywords optimize
search_geodesic_latest <- function(current, alpha = 1, index, max.tries = 5, n = 5, stepS = 0.01, cur_index = NA) {
  #browser()
  if (is.na(cur_index)) cur_index <- index(current)

  try <- 1
  while(try < max.tries) {
    # Try 5 random directions and pick the one that has the highest
    # index after a small step in either direction
    # change the original parameter tries to counter since it conflicts with the tries in geodesic-path.r
    direction <- find_best_dir(old = current, index, counter = n, dist=stepS)
    best_dir <- direction$best_dir
    direction_search <- direction$record_temp %>% mutate(loop = try)

    # Travel halfway round (pi / 4 radians) the sphere in that direction
    # looking for the best projection
    peak <- find_path_peak(current, best_dir, index) %>%
      mutate(tries = tries, loop = try)

    record <<- bind_rows(record, direction_search, peak)
    new_index <- tail(peak$index_val,1)
    pdiff <- (new_index- cur_index)/cur_index

    dig3 <- function(x) sprintf("%.3f", x)


    cat("Value ", dig3(new_index), " ",
        sprintf("%.1f", pdiff * 100), "% better ")
    if (pdiff > 0.001) {
      cat(" - NEW BASIS\n")
      cur_index <<- new_index
      return(list(record = record,
                  target = record$basis[[length(record$basis)]]))
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
find_best_dir <- function(old, index, dist = 0.01, counter = 5) {
#browser()

  # change the original parameter tries to counter since it conflicts with the tries in geodesic-path.r
  bases <- replicate(counter, basis_random(nrow(old), ncol(old)),
                     simplify = FALSE)

  score <- function(new) {
    #browser()
    interpolator <- geodesic_info(old, new)
    forward <- step_angle(interpolator, dist)
    backward <- step_angle(interpolator, -dist)

    index_val_larger <- max(index(forward), index(backward))

    tibble::tibble(basis = c(list(forward), list(backward)),
           index_val = c(index(forward), index(backward)),
           info = "direction_search",
           tries = tries)
  }

  record_temp <<- map_df(bases, score) %>%
    mutate(info = ifelse(index_val == max(index_val),
                         "best_direction_search", info))

  list(record_temp = record_temp,
       best_dir = record_temp$basis[[which.max(record_temp$index_val)]])

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
  #browser()
  interpolator <- geodesic_info(old, new)

  index_pos <- function(alpha) index(step_angle(interpolator, alpha))

  alpha <- stats::optimize(index_pos, c(-max_dist, max_dist), maximum = TRUE, tol = 0.01)

  best <- tibble(basis = list(step_angle(interpolator, alpha$maximum)),
                 index_val = alpha$objective,
                 info = "best_line_search")

  angle <-  sample(seq(-max_dist, max_dist, 0.01), 5)

  record_temp <- tibble::enframe(map(angle,
                             function(x) step_angle(interpolator, x))) %>%
    rename(basis = value) %>%
    select(-name) %>%
    mutate(index_val = map_dbl(basis, index),
           info = "line_search") %>%
    bind_rows(best)

  record_temp

}

