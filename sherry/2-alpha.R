library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(purrr)
library(foreach)
library(doFuture)
registerDoFuture()
plan(multicore)

# load data and trial
load(here::here("sherry", "data", "data.rda"))
a <- tourr::animate_dist(data, tour_path = guided_tour(holes(), d = 1,
                                           search_f = search_geodesic_latest))

################################

# functions to use
compute_better_alpha<- function(alpha, cooling){

  animate_dist(data, tour_path =
                           guided_tour(holes(), d = 1, alpha = alpha,
                                       cooling = cooling,
                                       search_f = search_better),
                         sphere = TRUE) %>%
    mutate(alpha = alpha) %>%
    mutate(cooling = cooling ) %>%
    mutate(method = "search_better")

}
compute_random_alpha <- function(alpha, cooling){

  animate_dist(data, tour_path =
                           guided_tour(holes(), d = 1, alpha = alpha,
                                       cooling = cooling,
                                       search_f = search_better_random),
                         sphere = TRUE) %>%
    mutate(alpha = alpha) %>%
    mutate(cooling = cooling ) %>%
    mutate(method = "search_better_random")

}
compute_geodesic_alpha <- function(stepS){

  animate_dist(data, tour_path =
                           guided_tour(holes(), d = 1, stepS = stepS,
                                       search_f = search_geodesic_latest),
                         sphere = TRUE) %>%
    mutate(stepS = stepS) %>%
    mutate(method = "geodesic")

}


################################

# grid value
#search_better(_random)
alpha <- c(0.1, 0.2, 0.3, 0.4 ,0.5)
cooling <- c(0.8, 0.85, 0.9, 0.95, 0.99, 0.995)
paras <- expand.grid(alpha, cooling)
# search_geodesic
stepS <- c(0.01, 0.1, 0.5, 0.9)
stepS2 <- c(0.01, 0.02, 0.05, 0.07, 0.09,  seq(0.1, 0.9, 0.1))
################################

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

geodesic_alpha2 <- foreach(i = 1:length(stepS2), .combine = "rbind") %do%{
  set.seed(123456)
  compute_geodesic_alpha(stepS[i])
}

################################
# clean the info

clean_info <- function(data){
  data %>%
    mutate(info = fct_relevel(info, c("start",
                                      "direction_search", "best_direction_search",
                                      "line_search", "best_line_search",
                                      "interpolation")),
           info2 = as.factor(ifelse(info %in% c("best_direction_search","best_line_search"),
                          "best", "normal")),
           info3 = ifelse(info2 == "normal", as.character(info),
                          str_sub(info, start = 6L, end = -1L)))
}

geodesic_alpha <- geodesic_alpha %>% clean_info()
geodesic_alpha2 <- geodesic_alpha2 %>% clean_info()




################################

# save(better_alpha, file = "sherry/data/better_alpha.rda")
# save(better_random_alpha, file = "sherry/data/better_random_alpha.rda")
# save(geodesic_alpha, file = "sherry/data/geodesic_alpha.rda")
# save(geodesic_alpha2, file = "sherry/data/geodesic_alpha2.rda")
