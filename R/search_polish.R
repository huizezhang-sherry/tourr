#' Polish the searching after search_geodesic
#'
#' @param current starting projection
#' @param polish_alpha maximum distance to travel
#' @param iter number of sample basis to take in the sphere
#' @param nloop number of time the polish process get executed
#' @keywords optimize
search_polish <- function(current, polish_alpha = 0.05, iter = 100, nloop = 10, polish_cooling = 0.9, ...){

  cur_index <- calc_index(current, data = data)
  record_temp <- tibble::tibble(basis = list(current), index_val = cur_index,
                                loop = 0, info = "polish")

  i <- 1

  while (i < nloop) {

    #polish_alpha <-  polish_alpha * polish_cooling
    alpha_seq <- seq(0, polish_alpha, by = polish_alpha/10)
    iter_seq <- seq(1, iter, by = 1)
    grid <- expand.grid(alpha_seq, iter_seq)


    polish <- map2_dfr(grid$Var1, grid$Var2, ~tibble(basis = list(basis_nearby(current, alpha = .x))), .id = "id") %>%
      mutate(index_val = map_dbl(basis, ~calc_index(.x, data = data)),
             alpha = grid$Var1, loop = i, info = "polish")

    better_row <- polish %>% filter(index_val > cur_index,
                                    index_val  == max(index_val))

    if (nrow(better_row) == 1) {
      record_temp <- record_temp %>% bind_rows(better_row)
      current <- better_row$basis[[1]]
      cur_index <- better_row$index_val
      cat("better basis found, index_val = ", cur_index, ", i = ", i, "\n")
    }

    i <- i + 1

  }

  record_temp

}

