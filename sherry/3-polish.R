library(tidyverse)
library(foreach)
library(doFuture)
registerDoFuture()
plan(multicore)

# simulate data
set.seed(1234)
x1 <- rnorm(100, 0, 1)
x2 <- c(rnorm(50, -1, 1), rnorm(50, 1, 1))
x8 <- rnorm(100, 0, 1)
x9 <- rnorm(100, 0, 1)
x10 <- rnorm(100, 0, 1)


compute_geodesic_alpha <- function(stepS, polish_alpha){
  #browser()
  data <- cbind(x1, x8, x9, x10, x2)

  result <- animate_dist(data, tour_path =
                           guided_tour(holes(), d = 1,stepS = stepS,
                                       search_f = search_geodesic_latest),
                         polish_alpha = polish_alpha,
                         sphere = TRUE) %>%
    mutate(stepS = stepS, polish_alpha = polish_alpha) %>%
    mutate(method = "geodesic")

  return(result)
}

set.seed(123456)
stepS <- c(0.01, 0.1, 0.5, 0.9)
polish_alpha <- c(0.01, 0.05, 0.1,0.2, 0.3,0.4, 0.5)
grid <- expand.grid(stepS = stepS, polish_alpha = polish_alpha)
geodesic_polished <- map2_df(grid$stepS, grid$polish_alpha,
                             compute_geodesic_alpha)
geodesic_polished <- foreach(i = 1:nrow(grid), .combine = "rbind") %do%{
  set.seed(123456)
  compute_geodesic_alpha(grid$stepS[i], grid$polish_alpha[i])
}

# save(geodesic_polished, file = "sherry/data/geodesic_polished.rda")


polish_alpha2 <- c(0.7, 0.9)
grid2 <- expand.grid(stepS = stepS, polish_alpha2 = polish_alpha2)
geodesic_polished2 <- foreach(i = 1:nrow(grid2), .combine = "rbind") %do%{
  set.seed(123456)
  compute_geodesic_alpha(grid2$stepS[i], grid2$polish_alpha2[i])
}




pca <- geodesic_alpha %>% compute_pca() %>%
  filter(info %in% c("start", "interpolation"))

pca %>%
  ggplot(aes(x= PC1, y = PC2, col = as.factor(stepS))) +
  geom_point() +
  geom_point(data = filter(pca, info == "start"), col = "red") +
  theme(aspect.ratio = 1) +
  facet_wrap(vars(stepS))

compute_geodesic_alpha <- function(stepS, polish = FALSE){
  #browser()
  data <- cbind(x1, x8, x9, x10, x2)

  result <- animate_dist(data, tour_path =
                           guided_tour(holes(), d = 1, stepS = stepS,
                                       search_f = search_geodesic_latest),
                         polish = polish,
                         sphere = TRUE) %>%
    mutate(stepS = stepS) %>%
    mutate(method = "geodesic")

  return(result)
}


stepS <- c(0.01, 0.02, 0.05, 0.07, 0.09,  seq(0.1, 0.9, 0.1))
geodesic_alpha2 <- foreach(i = 1:length(stepS), .combine = "rbind") %do%{
  set.seed(123456)
  compute_geodesic_alpha(stepS[i])
}

#save(geodesic_alpha2, file = "sherry/data/geodesic_alpha2.rda")

