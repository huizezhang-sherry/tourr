#' A guided tour path for
#' @export
#' @examples
guided_tour_dep <- function(index_f, pos, d = 2, alpha = 0.5, cooling = 0.99, max.tries = 25,
                        max.i = Inf, search_f = search_geodesic, n_sample = 100, ...) {

  d <- max(pos)

  generator <- function(current, data, tries, ...) {
    index <- function(proj) {
      index_f(as.matrix(data) %*% proj)
    }

    valid_fun <- c(
      "search_geodesic", "search_better", "search_better_random",
      "search_polish", "search_posse"
    )
    method <- valid_fun[vapply(valid_fun, function(x) {
      identical(get(x), search_f)
    }, logical(1))]

    if (is.null(current)) {
      return(basis_init(ncol(data), d))
    }

    mat <- matrix(0, ncol = d, nrow = length(pos))
    for (i in seq_len(d)) {
      mat[pos == i, i] <- basis_random(sum(pos == i), 1)
    }

   current <- mat

    cur_index <- index(current)

    if (cur_index > max.i) {
      cat("Found index ", cur_index, ", larger than selected maximum ", max.i, ". Stopping search.\n",
          sep = ""
      )
      cat("Final projection: \n")
      if (ncol(current) == 1) {
        for (i in 1:length(current)) {
          cat(sprintf("%.3f", current[i]), " ")
        }
        cat("\n")
      }
      else {
        for (i in 1:nrow(current)) {
          for (j in 1:ncol(current)) {
            cat(sprintf("%.3f", current[i, j]), " ")
          }
          cat("\n")
        }
      }
      return(NULL)
    }

    # current, alpha = 1, index, max.tries = 5, n = 5, delta = 0.01, cur_index = NA, ..
    basis <- search_f(current, alpha, index, tries, max.tries, cur_index = cur_index, frozen = frozen, n_sample = n_sample, ...)

    if (method == "search_posse") {
      if (!is.null(basis$h)) {
        if (basis$h > 30) {
          alpha <<- alpha * cooling
        }
      }
    } else {
      alpha <<- alpha * cooling
    }

    list(target = basis$target, index = index)
  }

  new_geodesic_path("guided", generator)
}
# globalVariables(c("cur_index", "index"))
