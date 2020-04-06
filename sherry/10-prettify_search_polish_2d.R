load(here::here("sherry", "data", "data.rda"))
load(here::here("sherry", "data", "data_mult.rda"))

set.seed(123456)
float <- tourr::animate_dist(data, tour_path = guided_tour(holes(), d = 1,
                                                               search_f = search_geodesic_float),
                                 rescale = FALSE)

set.seed(123456)
float_mult <- tourr::animate_dist(data_mult[,c(1,2,7:10)], tour_path = guided_tour(holes(), d = 2,
                                                                    search_f = search_geodesic_float),
                                      rescale = FALSE)

dt <- float %>% filter(info == "interpolation") %>% mutate(id = row_number(), type = "float") %>%
  bind_rows(data_tour %>% filter(info == "interpolation") %>% mutate(id = row_number(), type = "non-float"))

dt %>%
  ggplot() +
  geom_hline(yintercept = calc_index(basis_0, data = data_mult[,c(1,2,7:10)]), col = "red") +
  geom_line(aes(x = id, y = index_val, group = type)) +
  facet_wrap(vars(type), ncol = 1)


dt <- float_mult %>% filter(info == "interpolation") %>% mutate(id = row_number(), type = "float") %>%
  bind_rows(data_mult_tour %>% filter(info == "interpolation") %>% mutate(id = row_number(), type = "non-float"))


dt %>%
  ggplot(aes(x = id, y = index_val)) +
  geom_hline(yintercept = calc_index(basis_0, data = data_mult[,c(1,2,7:10)]), col = "red") +
  geom_line() +
  facet_wrap(vars(type), ncol = 1)

# 2D projection
set.seed(123456)
polish_holes_better_stop_mult <- tourr::animate_dist(data_mult[,c(1,2,7:10)], tour_path = guided_tour(holes(), d = 2,
                                                                                                      search_f = search_geodesic_latest),
                                                     rescale = FALSE, polish = TRUE,
                                                     nloop = 30, polish_alpha = 0.05)

basis_0 <- matrix(c(0, 1, 0, 0, 0, 0,
                    0, 0, 1, 0, 0, 0), nrow = 6, ncol = 2) # theoretical optimal


polish_modified_mult$record %>% filter(info == "polish") %>% view

basis <- polish_modified_mult$record %>% filter(info == "polish") %>% pull(basis)

basis_0 <- matrix(c(0, 1, 0, 0, 0, 0,
                    0, 0, 1, 0, 0, 0), nrow = 6, ncol = 2) # theoretical optimal
basis_1 <- basis %>% first() # last basis from interpolation
basis_2 <- basis %>% last() # last basis after polishing

basis_1
basis_2

tourr:::proj_dist(basis_1, basis_2)
tourr:::proj_dist(basis_0, basis_1)
tourr:::proj_dist(basis_0, basis_2)

calc_index(basis_0, data = data_mult[,c(1,2,7:10)])
calc_index(basis_1, data = data_mult[,c(1,2,7:10)]) -> idx1
calc_index(basis_2, data = data_mult[,c(1,2,7:10)]) -> idx2

idx1
idx2


polish_modified_mult$record %>%
  filter(info %in% c("interpolation", "polish")) %>%
  mutate(id = row_number()) %>%
  ggplot(aes(x = id, y = index_val)) +
  geom_line() +
  geom_hline(yintercept =
               calc_index(basis_0, data = data_mult[,c(1,2,7:10)]),
             color = "red")


start <- polish_modified_mult$record %>% filter(info == "polish")
sim <- polish_modified_mult$sim %>%
  mutate(last_basis_index = ifelse(loop == 0, 1, loop),
    proj_dist = map2_dbl(basis,last_basis_index, ~proj_dist(.x, start$basis[[.y]])))

sim %>%
  ggplot(aes(x = proj_dist, y = index_val, col = as.factor(alpha))) +
  geom_point(alpha = 0.5) +
  facet_wrap(vars(loop))









########################################
polish_mult_record <- polish_holes_better_stop_mult$record
save(polish_mult_record, file = "sherry/data/polish_mult_record.rda")

polish_mult_sim <- polish_holes_better_stop_mult$sim
save(polish_mult_sim, file = "sherry/data/polish_mult_sim.rda")
