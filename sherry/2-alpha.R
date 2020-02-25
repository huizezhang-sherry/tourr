library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(purrr)
library(foreach)
library(doFuture)
registerDoFuture()
plan(multicore)

# simulate data
set.seed(1234)
x1 <- rnorm(1000, 0, 1)
x2 <- c(rnorm(500, -5, 1), rnorm(500, 5, 1))
x8 <- rnorm(1000, 0, 1)
x9 <- rnorm(1000, 0, 1)
x10 <- rnorm(1000, 0, 1)

# animate_dist(data, tour_path = guided_tour(holes(), d = 1,
#                                            search_f = search_better),
#              polish =FALSE)

# functions to use
compute_better_alpha<- function(alpha, cooling){
  data <- cbind(x1, x8, x9, x10, x2)

  result <- animate_dist(data, tour_path =
                           guided_tour(holes(), d = 1, alpha = alpha,
                                       cooling = cooling,
                                       search_f = search_better),
                         sphere = TRUE) %>%
    mutate(alpha = alpha) %>%
    mutate(cooling = cooling ) %>%
    mutate(method = "search_better")

  return(result)
}
compute_random_alpha <- function(alpha, cooling){
  data <- cbind(x1, x8, x9, x10, x2)

  result <- animate_dist(data, tour_path =
                           guided_tour(holes(), d = 1, alpha = alpha,
                                       cooling = cooling,
                                       search_f = search_better_random),
                         sphere = TRUE) %>%
    mutate(alpha = alpha) %>%
    mutate(cooling = cooling ) %>%
    mutate(method = "search_better_random")

  return(result)
}
compute_geodesic_alpha <- function(stepS){
  #browser()
  data <- cbind(x1, x8, x9, x10, x2)

  result <- animate_dist(data, tour_path =
                           guided_tour(holes(), d = 1, stepS = stepS,
                                       search_f = search_geodesic_latest),
                         sphere = TRUE) %>%
    mutate(stepS = stepS) %>%
    mutate(method = "geodesic")

  return(result)
}


# grid value
#search_better(_random)
alpha <- c(0.1, 0.2, 0.3, 0.4 ,0.5)
cooling <- c(0.8, 0.85, 0.9, 0.95, 0.99, 0.995)
paras <- expand.grid(alpha, cooling)
# search_geodesic
stepS <- c(0.01, 0.1, 0.5, 0.9)

# simulation
better_alpha <- foreach(i = 1:nrow(paras), .combine = "rbind") %do%{
  set.seed(123456)
  compute_better_alpha(paras$Var1[i], paras$Var2[i])
}
better_random_alpha <- foreach(i = 1:nrow(paras), .combine = "rbind") %do%{
  set.seed(123456)
  compute_random_alpha(paras$Var1[i], paras$Var2[i])
}
geodesic_alpha <- foreach(i = 1:length(stepS), .combine = "rbind") %do%{
  set.seed(123456)
  compute_geodesic_alpha(stepS[i])
}


# save(better_alpha, file = "tour_optim_doc/better_alpha.rda")
# save(better_random_alpha, file = "tour_optim_doc/better_random_alpha.rda")
# save(geodesic_alpha, file = "tour_optim_doc/geodesic_alpha.rda")

