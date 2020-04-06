#' Polish the searching after search_geodesic
#'
#' @param current starting projection
#' @param polish_alpha maximum distance to travel
#' @param iter number of sample basis to take in the sphere
#' @param nloop number of time the polish process get executed
#' @keywords optimize
search_polish <- function(current, polish_alpha = 0.05, iter = 1000, nloop = 10, polish_cooling = 1, ...){
  #browser()
  cur_index <- calc_index(current, data = data)
  record_temp <- c()

  sim_temp <- tibble::tibble(basis = list(current), index_val = cur_index,
                                loop = 0, info = "polish")

  polish <- c()
  better_row <- tibble::tibble(basis = list(current), index_val = cur_index,
                               loop = 0, info = "polish", id = 0)
  range <-  diff(range(record$index_val))
  first_index <- better_row$index_val

  i <- 1
  improve <- 1

  while (nrow(better_row) != 0) {

    if(i == 1){
      improve = 1
    } else if(proj_dist(better_row$basis[[1]], current) > 0.01){
      improve = (better_row$index_val - cur_index)/cur_index
    }else{
      return(list(record_temp = record_temp, sim_temp = sim_temp))
    }

    cat("improvement: ", improve, "\n")

    if(improve < 0.001){
      polish_cooling <-  polish_cooling * 0.9
      polish_alpha <-  polish_alpha * polish_cooling

      if (polish_alpha <= 0.001){
        return(list(record_temp = record_temp, sim_temp = sim_temp))
      }
    }

    current <- better_row$basis[[1]]
    cur_index <- better_row$index_val
    cat("better basis found, index_val = ", cur_index, ", i = ", i, "\n")

    sim_temp <- sim_temp %>% bind_rows(polish)
    record_temp <- record_temp %>% bind_rows(better_row)

    cat("new polish alpha: ", polish_alpha * polish_cooling, "\n")

    polish <- map_dfr(1:iter, ~tibble(basis = list(basis_nearby(current, alpha = polish_alpha))), .id = "id") %>%
      mutate(index_val = map_dbl(basis, ~calc_index(.x, data = data)),
             alpha = polish_alpha, loop = i, info = "polish", id = 0)

    better_row <- polish %>% filter(index_val > cur_index,
                                    index_val  == max(index_val))

    i <- i + 1


  }

  list(record_temp = record_temp, sim_temp = sim_temp)

}




