library(tidyverse)
library(foreach)
source(here::here("sherry", "cleaning.R"))

set.seed(1234)
x1 <- rnorm(1000, 0, 1)
x2 <- sample(c(rnorm(500, -3, 1), rnorm(500, 3, 1)), size = 1000)
x3 <- sample(c(rep(-1, 500), rep(1, 500)), size = 1000)
x4 <- sample(c(rnorm(250, -3, 1), rnorm(750, 3, 1)), size = 1000)
x5 <- sample(c(rnorm(330, -5, 1), rnorm(340, 0, 1), rnorm(330, 5, 1)), size = 1000)
x6 <- sample(c(rnorm(450, -5, 1), rnorm(100, 0, 1), rnorm(450, 5, 1)), size = 1000)
x7 <- sample(c(rnorm(500, -5, 1), rnorm(500, 5, 1)), size = 1000)
x8 <- rnorm(1000, 0, 1)
x9 <- rnorm(1000, 0, 1)
x10 <- rnorm(1000, 0, 1)


data_mult <- tibble::tibble(x1 = x1, x2 = x2, x3 = x3,
                               x4 = x4, x5 = x5, x6 = x6, x7 = x7,
                               x8 = x8, x9 = x9, x10 = x10) %>% map_df(scale)


################################
# when having two informative variables - one-dimensional projection would find either one of the informative variable
# A two dimensional projection will be better!
run_geodesic <- function(var){
  set.seed(12345)
  mult_geodesic <- data_mult[,c(1,var,8,9,10)] %>%
    animate_xy(tour_path = guided_tour(holes(), d = 2,
                                       search_f = search_geodesic_latest),
               rescale = FALSE)

  return(mult_geodesic)
}


################################
# PCA and index tracing plot
pca <- foreach::foreach(i  = 1:nrow(mult_geodesic), .combine = "rbind") %do% {
  t(c(mult_geodesic$basis[[i]]))
}
loadings <- stats::predict(stats::prcomp(pca[,1:6]))[,1:2]
pca_v1 <- cbind(pca[,1:6], loadings) %>%
  as_tibble() %>%
  bind_cols(mult_geodesic) %>%
  clean_info()

loadings <- stats::predict(stats::prcomp(pca[,7:12]))[,1:2]
pca_v2 <- cbind(pca[,7:12], loadings) %>%
  as_tibble() %>%
  bind_cols(mult_geodesic) %>%
  clean_info()

################################
save(data_mult,file = "sherry/data/data_mult.rda")
save(mult_geodesic, file = "sherry/data/mult_geodesic.rda")
save(pca_v1, file = "sherry/data/pca_v1.rda")
save(pca_v2, file = "sherry/data/pca_v2.rda")
