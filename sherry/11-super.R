load(here::here("sherry/data/eco_long.rda"))

library(gghighlight)
library(tidyverse)

#####################################

# Comparing variation across different simulations using all time
set.seed(123456)
sim_num <- sample(1:400, size = 40)
eco_sim <- matrix(nrow = 14, ncol = 40)

i <- 1

while(i <= length(sim_num)){

  cat("i = ", i, "\n")

  dt <- eco_long %>% pivot_wider(names_from = var, values_from = value)  %>%
    filter(sim == sim_num[i]) %>% dplyr::select(intTotal: domTotal) %>% scale()

  set.seed(123456)
  a <- tourr::animate_dist(dt, tour_path = guided_tour(holes(), d = 1,
                                                       search_f = search_geodesic_latest),
                           rescale = FALSE)
  final <- a %>% filter(info == "interpolation") %>% pull(basis) %>% tail(1)

  eco_sim[,i] <-  final[[1]]

  i <- i + 1

}


#####################################
# Comparing weight between different time using all simulation
eco_t <- matrix(nrow = 14, ncol = 22)

# t1 is same for all
i <- 2
while(i <= 22){

  cat("i = ", i, "\n")

  dt <- eco_long %>% pivot_wider(names_from = var, values_from = value) %>%
    filter(t == i) %>% dplyr::select(intTotal: domTotal) %>% scale()

  set.seed(123456)
  a <- tourr::animate_dist(dt, tour_path = guided_tour(holes(), d = 1,
                                                       search_f = search_geodesic_latest),
                           rescale = FALSE)
  final <- a %>% filter(info == "interpolation") %>% pull(basis) %>% tail(1)

  eco_t[,i] = final[[1]]

  i <- i + 1

}


#####################################

# comparing different result on different searching methods
eco_t_polish <- matrix(nrow = 14, ncol = 22)

# t1 is same for all
i <- 2
while(i <= 22){

  cat("i = ", i, "\n")

  dt <- eco_long %>% pivot_wider(names_from = var, values_from = value) %>%
    filter(t == i) %>% dplyr::select(intTotal: domTotal) %>% scale()

  set.seed(123456)
  a <- tourr::animate_dist(dt, tour_path = guided_tour(holes(), d = 1,
                                                       search_f = search_polish2),
                           rescale = FALSE)
  final <- a %>% filter(info == "interpolation") %>% pull(basis) %>% tail(1)

  eco_t_polish[,i] = final[[1]]


  i <- i + 1

}


#####################################
save(eco_t, file = "sherry/data/eco_t.rda")
save(eco_sim, file = "sherry/data/eco_sim.rda")
save(eco_t_polish, file = "sherry/data/eco_t_polish.rda")


save(eco_scaled_t, file = "sherry/data/eco_scaled_t.rda")
save(eco_scaled_t_polish, file = "sherry/data/eco_scaled_t_polish.rda")



