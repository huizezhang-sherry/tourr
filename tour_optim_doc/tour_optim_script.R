library(tidyverse)
set.seed(1234)
x1 <- rnorm(100, 0, 1)
x2 <- c(rnorm(50, -1, 1), rnorm(50, 1, 1))
x8 <- rnorm(100, 0, 1)
x9 <- rnorm(100, 0, 1)
x10 <- rnorm(100, 0, 1)

origin_dt <- tibble::tibble(x1 = x1, x2 = x2, x8 = x8,
                            x9 = x9,
                            x10 = x10) %>%
  gather(names, values) %>%
  mutate(names = as_factor(names),
         names = fct_relevel(names, levels = c("x1", "x2", "x8", "x9", "x10")))
#save(origin_dt,file = "tour_optim_doc/origin_dt.rda")

origin_dt %>%
  ggplot(aes(x = values)) +
  geom_histogram(binwidth = 0.15) +
  geom_density(aes(y=0.15 * ..count..)) +
  facet_wrap(vars(names), ncol = 3)

################################


compute_global_object_geodesic <- function(var, names){
  data <- cbind(x1, x8, x9, x10, var)

  result <- animate_dist(data, tour_path =
                           guided_tour(holes(), d = 1,
                                       search_f = search_geodesic_latest),
                         sphere = TRUE) %>%
    mutate(col = names) %>%
    mutate(method = "geodesic")

  return(result)
}

compute_pca <- function(data, names){
  rows <- data %>% filter(col == names)
  pca <- foreach(i  = 1:nrow(rows), .combine = "rbind") %do%{
    t(rows$basis[[i]])
  }

  res.pca <- prcomp(pca)
  loadings <- res.pca$rotation[,c(1,2)]
  pca2 <- cbind(pca, pca %*% loadings)

  result <- as_tibble(pca2) %>% mutate(id = 1: nrow(pca2),
                                       info = rows$info,
                                       V5_names = rows$col,
                                       tries = rows$tries,
                                       index_val = rows$index_val)

  colnames(result)[1:4] <- c("x1", "x8", "x9", "x10")

  return(result)

}

################################
set.seed(1234)
x2_geodesic <- compute_global_object_geodesic(x2, "x2")
#save(x2_geodesic, file = "tour_optim_doc/x2_geodesic.rda")

x2_object <- x2_geodesic %>% compute_pca("x2")
#save(x2_object, file = "tour_optim_doc/x2_object.rda")
