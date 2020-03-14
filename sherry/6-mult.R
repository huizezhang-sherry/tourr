library(tidyverse)
library(foreach)

set.seed(1234)
x1 <- rnorm(1000, 0, 1)
x2 <- c(rnorm(500, -3, 1), rnorm(500, 3, 1))
X7 <- c(rnorm(500, -5, 1), rnorm(500, 5, 1))
x8 <- rnorm(1000, 0, 1)
x9 <- rnorm(1000, 0, 1)
x10 <- rnorm(1000, 0, 1)

data_mult <- cbind(x1, x2, x7, x8, x9, x10)

temp <- data_mult %>% scale()

set.seed(123456)
a <- tourr::animate_dist(temp, tour_path = guided_tour(holes(), d = 1,
                                                       search_f = search_geodesic_latest),
                         sphere = FALSE, rescale = FALSE)

################################
set.seed(123456)
mult_geodesic <- animate_dist(data_mult, tour_path =
                           guided_tour(holes(), d = 1,
                                       search_f = search_geodesic_latest),
                         sphere = FALSE, rescale = FALSE) %>%
    mutate(col = "x7") %>%
    mutate(method = "geodesic")

set.seed(123456)
mult_better <- animate_dist(data_mult, tour_path =
                                guided_tour(holes(), d = 1,
                                            search_f = search_better),
                              sphere = FALSE, rescale = FALSE) %>%
  mutate(col = "x7") %>%
  mutate(method = "better")

set.seed(123456)
mult_better_random <- animate_dist(data_mult, tour_path =
                                guided_tour(holes(), d = 1,
                                            search_f = search_better_random),
                              sphere = FALSE, rescale = FALSE) %>%
  mutate(col = "x7") %>%
  mutate(method = "better_random")

################################
compute_pca <- function(data, names){
  rows <- data %>% filter(col == names)
  pca <- foreach::foreach(i  = 1:nrow(rows), .combine = "rbind") %do%{
    t(rows$basis[[i]])
  }

  loadings <- stats::predict(stats::prcomp(pca))[,1:2]
  pca2 <- cbind(pca, loadings)

  result <- as_tibble(pca2) %>% mutate(id = 1: nrow(pca2),
                                       info = rows$info,
                                       V5_names = rows$col,
                                       tries = rows$tries,
                                       loop = rows$loop,
                                       index_val = rows$index_val)

  colnames(result)[1:4] <- c("x1", "x8", "x9", "x10")

  return(result)

}

mult_object <- mult_geodesic %>% compute_pca("x7") %>%
  mutate(info = fct_relevel(info, c("start", "direction_search",
                                    "best_direction_search", "line_search",
                                    "best_line_search")))

mult_object_better <- mult_better %>% compute_pca("x7") %>%
  mutate(info = fct_relevel(info, c("start", "random_search", "new_basis")))


mult_object_better_random <- mult_better_random %>% compute_pca("x7") %>%
  mutate(info = fct_relevel(info, c("start", "random_search", "new_basis")))


################################
save(data_mult,file = "sherry/data/data_mult.rda")
save(mult_geodesic, file = "sherry/data/mult_geodesic.rda")
save(mult_better, file = "sherry/data/mult_better.rda")
save(mult_better_random, file = "sherry/data/mult_better_random.rda")

save(mult_object, file = "sherry/data/mult_object.rda")
save(mult_object_better, file = "sherry/data/mult_object_better.rda")
save(mult_object_better_random, file = "sherry/data/mult_object_better_random.rda")





