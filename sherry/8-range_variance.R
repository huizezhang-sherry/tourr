load(here::here("sherry", "data", "data.rda"))
load(here::here("sherry", "data", "data_mult.rda"))

library(tidyverse)
library(foreach)

#################################
# simulate the interpolation path on non-structure data to see variation of each index
noise <- data %>% dplyr::select(-x2)

sim_variation <- function(i){

  set.seed(1234 + i)

  basis1 <- basis_random(4,1)
  basis2 <- basis_random(4,1)

  geo <- geodesic_path(basis1, basis2)
  interp <- geodesic_info(basis1, basis2)

  cur_dist <- 0
  target_dist <- geo$dist
  pos <- seq(cur_dist, target_dist, 1/100)

  path <- tibble(basis = map(pos, ~step_fraction(interp, .x))) %>%
    mutate(kol = map_dbl(basis, ~calc_kol(proj = .x, data = noise)),
           kol_cdf = map_dbl(basis, ~calc_kol_cdf(proj = .x, data = noise)),
           hole = map_dbl(basis, ~calc_index(proj = .x, data = noise)),
           id = row_number())

  path

}

variation <- map_dfr(1:1000, sim_variation) %>%
  mutate(i = ifelse(id == 1, 1, 0), group = cumsum(i)) %>%
  dplyr::select(-i)


#################################
# simulate the interpolation path with ending projection at the theoretical structure matrix to see the range of each index

sim_range <- function(i){

  set.seed(1234 + i)

  basis1 <- basis_random(5,1)
  basis2 <- matrix(c(0, 1, 0, 0, 0))

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

range <- map_dfr(1:1000, sim_range) %>%
  mutate(i = ifelse(id == 1, 1, 0), group = cumsum(i)) %>%
  dplyr::select(-i)



#################################
# start and end of the first simulation are very close because the noise data doesn't have a structure
end <- range %>%
  mutate(id_lead = lead(id,default = 1)) %>%
  filter(id_lead == 1) %>%
  rename(end.kol = kol, end.kol_cdf = kol_cdf, end.hole = hole)

start <- range %>%
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


library(extRemes)
range_hole <- range %>% filter(index == "hole") %>% pull(range)

fit0 <- fevd(range_hole)
plot(fit0)

fit1 <- fevd(range_hole)


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
save(variation, file = "sherry/data/variation.rda")
save(range, file = "sherry/data/range.rda")
save(sim_holes, file = "sherry/data/sim_holes.rda")





