library(tidyverse)
library(foreach)
source(here::here("sherry", "cleaning.R"))

set.seed(1234)
x1 <- rnorm(1000, 0, 1)
x2 <- sample(c(rnorm(500, -3, 1), rnorm(500, 3, 1)), size = 1000)
x7 <- sample(c(rnorm(500, -5, 1), rnorm(500, 5, 1)), size = 1000)
x8 <- rnorm(1000, 0, 1)
x9 <- rnorm(1000, 0, 1)
x10 <- rnorm(1000, 0, 1)

data_mult <- cbind(x1, x2, x7, x8, x9, x10)

# scaling is helpful for pp to find the optimal projection
data_mult_scaled <- data_mult%>% scale()


################################
# when having two informative variables - one-dimensional projection would find either one of the informative variable
set.seed(12345)
a <- tourr::animate_dist(data_mult_scaled,
                         tour_path = guided_tour(holes(), d = 1,
                                                 search_f = search_geodesic_latest),
                         sphere = FALSE, rescale = FALSE)

proj <- a %>% filter(info == "interpolation") %>%  pull(basis) %>% tail(1)
projected <- data_mult_scaled %*% proj[[1]] %>% as_tibble()

projected %>%
  ggplot(aes(x = V1)) +
  geom_histogram(binwidth = 0.1) +
  geom_density(aes(y=0.1 * ..count..))

################################
# two dimensional projection
set.seed(12345)
mult_geodesic <- tourr::animate_xy(data_mult_scaled,
                          tour_path = guided_tour(holes(), d = 2,
                                                  search_f = search_geodesic_latest),
                          sphere = FALSE, rescale = FALSE)

pca <- foreach::foreach(i  = 1:nrow(mult_geodesic), .combine = "rbind") %do%{
  t(c(mult_geodesic$basis[[i]]))
}
loadings <- stats::predict(stats::prcomp(pca))[,1:2]
pca2 <- cbind(pca, loadings)

mult_object <- as_tibble(pca2) %>% bind_cols(mult_geodesic) %>% clean_info()


################################
save(data_mult,file = "sherry/data/data_mult.rda")
save(mult_geodesic, file = "sherry/data/mult_geodesic.rda")
save(mult_object, file = "sherry/data/mult_object.rda")
