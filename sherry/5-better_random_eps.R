library(tidyverse)
library(foreach)
library(doFuture)
registerDoFuture()
plan(multicore)

################################
load(here::here("sherry", "data", "data.rda"))


# functions to use
compute_random_eps<- function(eps, cooling){

  animate_dist(data, tour_path =
                 guided_tour(holes(), d = 1, eps = eps, alpha = 0.5,
                             cooling = cooling,
                             search_f = search_better_random),
               sphere = TRUE) %>%
    mutate(eps = eps,
           cooling = cooling,
           alpha = 0.5,
           method = "search_better_random")
}

temp <- compute_random_eps(0.001, 0.95)
#eps <- c(0.001,0.01, 0.05, 0.1, 0.5)
<<<<<<< HEAD
eps <- c(1e-5, 5e-5, 1e-4, 5e-4, 0.001)
=======
eps <- c(1e-5, 5e-5, 1e-4, 5e-4, 0.001, 0005, 0.01)
>>>>>>> b1bb63224cd535e2df6e698abff890e99409a36f
cooling <- c(0.95, 0.99, 0.995)
paras <- expand.grid(eps, cooling)

better_random_eps <- foreach(i = 1:nrow(paras), .combine = "rbind") %do%{
  set.seed(123456)
  compute_random_eps(paras$Var1[i], paras$Var2[i])
}

# save(better_random_eps, file = "sherry/data/better_random_eps.rda")

