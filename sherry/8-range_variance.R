load(here::here("sherry", "data", "data.rda"))
load(here::here("sherry", "data", "data_mult.rda"))
# load(here::here("sherry", "data", "data_mult.rda"))
# data <- data_mult %>% dplyr::select(x1:x2, x7:x10)

library(foreach)

set.seed(123456)
d <- 1
basis1 <- basis_random(ncol(data), d)
basis2 <- basis_random(ncol(data), d)
geo <- geodesic_path(basis1, basis2)
cur_dist <- 0
target_dist <- geo$dist
pos <- seq(cur_dist, target_dist, 1/150)
interp <- geodesic_info(basis1, basis2)
path <- tibble(index_val = map_dbl(pos, ~step_fraction(interp = interp,fraction = .x) %>% calc_kol(data = data))) %>% mutate(id = row_number())
path %>% ggplot(aes(x = id, y = index_val)) +
  geom_line() +
  geom_point()


path <- tibble(index_val = map_dbl(pos, ~step_fraction(interp = interp,fraction = .x) %>% calc_index(data = data))) %>% mutate(id = row_number())
path %>% ggplot(aes(x = id, y = index_val)) +
  geom_line() +
  geom_point()



sim_rand_diff <- function(i){

  set.seed(1234 + i)

  basis1 <- basis_random(5,1)
  basis2 <- basis_random(5,1)

  geo <- geodesic_path(basis1, basis2)
  interp <- geodesic_info(basis1, basis2)

  cur_dist <- 0
  target_dist <- geo$dist
  pos <- seq(cur_dist, target_dist, 1/100)

  path <- tibble(basis = map(pos, ~step_fraction(interp, .x))) %>%
    mutate(kol = map_dbl(basis, ~calc_kol(proj = .x, data = data)),
           kol_cdf = map_dbl(basis, ~calc_kol_cdf(proj = .x, data = data)),
           hole = map_dbl(basis, ~calc_index(proj = .x, data = data)),
           id = row_number())

  path

}

sim <- map_dfr(1:1000, sim_rand_diff)


index_random_path <- sim %>% mutate(i = ifelse(id == 1, 1, 0),
                                    group = cumsum(i)) %>%
  dplyr::select(-i)

end <- index_random_path %>%
  mutate(id_lead = lead(id,default = 1)) %>%
  filter(id_lead == 1) %>%
  rename(end.kol = kol, end.kol_cdf = kol_cdf, end.hole = hole)

start <- index_random_path %>%
  filter(id == 1) %>%
  rename(start.kol = kol, start.kol_cdf = kol_cdf, start.hole = hole)


path <- bind_cols(start, end) %>%
  dplyr::select(-c(basis1, id, group, id1, group1, id_lead)) %>%
  pivot_longer(-basis, names_to = "index", values_to = "value") %>%
  separate(index, into = c("position", "index"), sep = "\\.") %>%
  pivot_wider(names_from = position, values_from = value) %>%
  mutate(range = abs(start - end))


path %>%
  ggplot(aes(y = range)) +
  geom_boxplot() +
  facet_wrap(vars(index), scales = "free_y")


pretty_bin <- function(x){
  breaks <- pretty(range(x), n = nclass.scott(x), min.n = 1)
  bwidth <- breaks[2] - breaks[1]
  bwidth
}


path %>% ggplot(aes(x = range)) +
  geom_histogram(aes(y = ..ncount..),binwidth = function(x) pretty_bin(x)) +
  geom_density(aes(y = ..ndensity..)) +
  facet_wrap(vars(index), scales = "free_x")


path %>% group_by(index) %>%
  summarise(mean = mean(range), sd = sd(range))

#################################

index_diff <- index_random_path %>%
  group_by(group) %>%
  mutate(kol = c(NA, diff(kol)),
         kol_cdf = c(NA, diff(kol_cdf)),
         hole = c(NA, diff(hole))) %>%
  pivot_longer(kol:hole, names_to = "index", values_to = "value")

index_diff %>% filter(!is.na(value)) %>% group_by(index) %>%
  summarise(smoothness = sd(value)/abs(mean(value)))

index_diff %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(vars(index), scales = "free_x")



#################################

compute_sim_holes <- function(){

  a <- tourr::animate_dist(data, tour_path = guided_tour(holes(), d = 1,
                                                         search_f = search_geodesic_latest),
                           rescale = FALSE)

  a %>% filter(info == "interpolation") %>% tail(1) %>% mutate(index = "holes")
}



sim_holes <- foreach(i = 1: 1000, .combine = "rbind") %do% {
  compute_sim_holes()
}



sim_holes %>%
  summarise(mean = mean(index_val), sd = sd(index_val))

#################################
range <- index_random_path %>%
  dplyr::select(-id) %>%
  pivot_longer(kol: hole, names_to = "index",values_to = "value") %>%
  group_by(group, index) %>%
  mutate(range = diff(range(value)),
         id = paste0(group, index)) %>%
  ungroup() %>%
  filter(!duplicated(id))

range %>%
  ggplot(aes(x = range)) +
  geom_histogram() +
  facet_wrap(vars(index), scales = "free_x")

range %>% group_by(index) %>%
  summarise(mean = mean(value), sd = sd(value))

library(extRemes)
range_hole <- range %>% filter(index == "hole") %>% pull(range)

fit0 <- fevd(range_hole)
plot(fit0)

fit1 <- fevd(range_hole)



#################################
save(index_random_path, file = "sherry/data/index_random_path.rda")
save(sim_holes, file = "sherry/data/sim_holes.rda")





