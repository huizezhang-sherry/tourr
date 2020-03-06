library(tidyverse)
library(foreach)
library(doFuture)
registerDoFuture()
plan(multicore)


load(here::here("sherry", "data", "data.rda"))
a <- tourr::animate_dist(data, tour_path = guided_tour(holes(), d = 1,
                                                       search_f = search_geodesic_latest),
                         polish = TRUE)


compute_geodesic_alpha <- function(stepS, polish_alpha){

  animate_dist(data, tour_path =
                           guided_tour(holes(), d = 1,stepS = stepS,
                                       search_f = search_geodesic_latest),
                         polish_alpha = polish_alpha,
                         sphere = TRUE, polish = TRUE) %>%
    mutate(stepS = stepS, polish_alpha = polish_alpha) %>%
    mutate(method = "geodesic")

}

set.seed(123456)
stepS <- c(0.01, 0.1, 0.5, 0.9)
polish_alpha <- c(0.01, 0.05, 0.1,0.2, 0.3,0.4, 0.5, 0.7, 0.9)
grid <- expand.grid(stepS = stepS, polish_alpha = polish_alpha)
geodesic_polished <- foreach(i = 1:nrow(grid), .combine = "rbind") %do%{
  set.seed(123456)
  compute_geodesic_alpha(grid$stepS[i], grid$polish_alpha[i])
}

# save(geodesic_polished, file = "sherry/data/geodesic_polished.rda")


pca <- geodesic_alpha %>% compute_pca() %>%
  filter(info %in% c("start", "interpolation"))

pca %>%
  ggplot(aes(x= PC1, y = PC2, col = as.factor(stepS))) +
  geom_point() +
  geom_point(data = filter(pca, info == "start"), col = "red") +
  theme(aspect.ratio = 1) +
  facet_wrap(vars(stepS))



