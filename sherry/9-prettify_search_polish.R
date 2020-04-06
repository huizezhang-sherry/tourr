set.seed(123456)
polish_holes_mult <- animate_dist(data_mult[,c(1,2,7:10)], tour_path = guided_tour(holes(), d = 2,
                                                                                   search_f = search_polish),
                                  rescale = FALSE, start_basis = matrix(...))

###################################
library(tidyverse)
load(here::here("sherry", "data", "data.rda"))
load(here::here("sherry", "data", "data_mult.rda"))

set.seed(123456)
data_tour <- tourr::animate_dist(data, tour_path = guided_tour(holes(), d = 1,
                                                               search_f = search_geodesic_latest),
                                 rescale = FALSE)

set.seed(123456)
data_mult_tour <- tourr::animate_dist(data_mult[,c(1,2,7:10)], tour_path = guided_tour(holes(), d = 2,
                                                                    search_f = search_geodesic_latest),
                                      rescale = FALSE)


interp_data <- data_tour %>% filter(info == "interpolation") %>% mutate(id = row_number())

interp_data %>%
  ggplot(aes(x = id, y = index_val)) +
  geom_hline(yintercept = calc_index(basis_0, data = data), col = "red") +
  geom_line()


interp_data %>% mutate(range = index_val - min(index_val)) %>% filter(range == max(range))


set.seed(123456)
polish_holes_better_stop <- tourr::animate_dist(data, tour_path = guided_tour(holes(), d = 1,
                                                               search_f = search_geodesic_latest),
                                 rescale = FALSE, polish = TRUE,
                                 nloop = 10, polish_alpha = 0.1)

0.9382048

set.seed(123456)
polish_holes_better_stop_mult <- tourr::animate_dist(data_mult[,c(1,2,7:10)], tour_path = guided_tour(holes(), d = 2,
                                                                    search_f = search_geodesic_latest),
                                      rescale = FALSE, polish = TRUE,
                                      nloop = 30, polish_alpha = 0.05)



polish_holes_better_stop$record %>% filter(info == "polish")
polish_holes_better_stop$record %>% filter(info == "interpolation") %>% tail()

basis <- polish_holes_better_stop$record %>% filter(info == "polish") %>% pull(basis)

basis_0 <- matrix(c(0, 1, 0, 0, 0))

# basis_0 <- matrix(c(0, 1, 0, 0, 0, 0,
#                     0, 0, 1, 0, 0, 0), nrow = 6, ncol = 2) # theoretical optimal
basis_1 <- basis %>% first() # last basis from interpolation
basis_2 <- basis %>% last() # last basis after polishing

basis_1
basis_2

tourr:::proj_dist(basis_1, basis_2)
tourr:::proj_dist(basis_0, basis_1)
tourr:::proj_dist(basis_0, basis_2)

basis_3 <- matrix(c(0.04, 0.99, 0.04, 0.06, 0.08))
basis_3 <- matrix(c(0.001, 0.990, 0.001, 0.001, 0.001))

# basis_3 <- matrix(c(
#  -0.01472936, -0.01336101,
#  -0.40766032,  0.91150426,
#   0.91048130  ,0.40692283,
#   0.02266436  ,0.03689678,
#   0.04883816 ,-0.02853246,
#  -0.04148581, -0.03492574), nrow = 6, ncol = 2, byrow = TRUE)

calc_index(basis_0, data = data)
calc_index(basis_1, data = data) -> idx1
calc_index(basis_2, data = data) -> idx2
calc_index(basis_3, data = data)

proj_dist(basis_0, basis_3)


sim <- tibble(id = 1: 100,
              basis = map(id, ~basis_nearby(basis_0, 0.005)),alpha = 0.005) %>%
  bind_rows(tibble(id = 1:100, basis = map(id, ~basis_nearby(basis_0, 0.01)), alpha = 0.01)) %>%
  bind_rows(tibble(id = 1:100, basis = map(id, ~basis_nearby(basis_0, 0.01)), alpha = 0.02)) %>%
  bind_rows(tibble(id = 1:100, basis = map(id, ~basis_nearby(basis_0, 0.01)), alpha = 0.03)) %>%
  bind_rows(tibble(id = 1:100, basis = map(id, ~basis_nearby(basis_0, 0.01)), alpha = 0.04)) %>%
  bind_rows(tibble(id = 1:100, basis = map(id, ~basis_nearby(basis_0, 0.01)), alpha = 0.05)) %>%
  bind_rows(tibble(id = 1:100, basis = map(id, ~basis_nearby(basis_0, 0.01)), alpha = 0.06)) %>%
  bind_rows(tibble(id = 1:100, basis = map(id, ~basis_nearby(basis_0, 0.01)), alpha = 0.07)) %>%
  bind_rows(tibble(id = 1:100, basis = map(id, ~basis_nearby(basis_0, 0.01)), alpha = 0.08)) %>%
  bind_rows(tibble(id = 1:100, basis = map(id, ~basis_nearby(basis_0, 0.01)), alpha = 0.09)) %>%
  bind_rows(tibble(id = 1:100, basis = map(id, ~basis_nearby(basis_0, 0.01)), alpha = 0.10)) %>%
  bind_rows(tibble(id = 1:100, basis = map(id, ~basis_nearby(basis_0, 0.01)), alpha = 0.15)) %>%
  bind_rows(tibble(id = 1:100, basis = map(id, ~basis_nearby(basis_0, 0.05)), alpha = 0.20)) %>%
  mutate(index_val = map_dbl(basis, ~calc_index(.x, data = data)),
         proj_dist = map_dbl(basis, ~proj_dist(basis_0, .x)))


seq_1 <- sim %>% filter(loop == 0) %>% filter(index_val == max(index_val)) %>%  pull(basis)
seq_2 <- sim %>% filter(index_val == max(index_val))


polish <- polish_holes_better_stop$record %>% filter(info == "polish")

sim <- polish_holes_better_stop$sim %>%
  mutate(proj_dist = case_when(loop == 1 ~ map_dbl(basis, ~proj_dist(polish$basis[[1]], .x)),
                               loop == 2 ~ map_dbl(basis, ~proj_dist(polish$basis[[2]], .x)),
                               loop == 9 ~ map_dbl(basis, ~proj_dist(polish$basis[[4]], .x)),
                               TRUE ~  map_dbl(basis, ~proj_dist(polish$basis[[3]], .x)),))



sim %>%
  ggplot(aes(x = proj_dist, y = index_val, col = as.factor(alpha)), ) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = calc_index(basis_0, data = data), col = "red") +
  geom_point(data = tibble(proj_dist = c(proj_dist(basis_1, basis_1),
                                         proj_dist(basis_2, basis_1)),
                           index_val = c(idx1, idx2)), col = "blue") +
  facet_wrap(vars(loop))
