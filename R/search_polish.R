#' Polish the searching after search_geodesic
#'
#' @param current starting projection
#' @param alpha_polish maximum distance to travel (currently ignored)
#' @param index interestingness index function
#' @param max.tries maximum number of basis to search
#' @param method linear or geodesic nearby basis
#' @keywords optimize
search_polish <- function(current,grid_gap = 0.002){
  #browser()
  i = 1
  cur_val <- calc_index(current, data = data)

  while (i <= 10) {
    grid_vec <- c(-2, -1, 0, 1, 2) * grid_gap
    grid <- c()
    for (j in 1:nrow(current)) {
      grid <- append(grid, grid_vec + current[j,])
    }

    temp <- matrix(grid, nrow = 5,ncol = 5, byrow = FALSE) %>% as_tibble() %>% expand.grid()

    sample <- temp %>% mutate(id = row_number()) %>% nest(V1:V5) %>% rename(basis = data) %>%
      mutate(index_val = map_dbl(.x = basis, ~calc_index(t(as.matrix(.x)), data = data))) %>%
      unnest(basis)

    cand <- matrix(sample[which.max(sample$index_val),] %>% dplyr::select(V1:V5) %>% as.numeric())
    cand_val <- calc_index(cand, data = data)

    if (cand_val > cur_val){
      current <- cand
    }

    record_temp <- tibble(basis = list(current),
                          index_val = calc_index(current, data = data),
                          info = "polish")

    record <<- record %>% bind_rows(record_temp)

    i <- i + 1
  }


}
