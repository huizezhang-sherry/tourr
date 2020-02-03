library(tidyverse)
set.seed(1234)
x1 <- rnorm(100, 0, 1)
x2 <- c(rnorm(50, -1, 1), rnorm(50, 1, 1))
x3 <- c(rep(-1, 50), rep(1, 50))
x4 <- c(rnorm(25, -1, 1), rnorm(75, 1, 1))
x5 <- c(rnorm(33, -1, 1), rnorm(33, 0, 1), rnorm(34, 1, 1))
x6 <- c(rnorm(45, -1, 1), rnorm(10, 0, 1), rnorm(45, 1,1))
x7 <- c(rnorm(50, -3, 1), rnorm(50, 3, 1))
x8 <- rnorm(100, 0, 1)
x9 <- rnorm(100, 0, 1)
x10 <- rnorm(100, 0, 1)

# dt <- cbind(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
# df <- as.matrix(scale(dt, center = TRUE, scale = TRUE))
#
# animate_dist(df[,c(3,1,8,9, 10)], tour_path=guided_tour(index_f = holes(), 1), sphere = TRUE, start = basis_random(5, 1))
# animate_dist(df[,c(1,7,8,9, 10)], tour_path=guided_tour(index_f = holes(), 1), sphere = TRUE, start = basis_random(5, 1))
# animate_dist(df[, 1:7], tour_path=guided_tour(index_f = holes(), 1), sphere = TRUE, start = basis_random(7, 1))
#
# holes()(matrix(df[,6]))



var <- list(x2 = x2, x3 = x3, x4 = x4,
            x5 = x5, x6 = x6, x7 = x7)

names <- list("x2", "x3", "x4", "x5", "x6", "x7")

result_geodesic <- purrr::map2_df(var, names, function(var, names){
  data <- cbind(x1, x8, x9, x10, var)

  set.seed(1234)
  result <- animate_dist(data, tour_path =
                           guided_tour(holes(), d = 1,
                                       search_f = search_geodesic_latest),
                         sphere = TRUE) %>%
    mutate(col = names,
           method = "geodesic")

  result
})

#save(result_geodesic, file = "sim/result_geodesic.rda")

result_geodesic %>%
  filter(info == "interpolation")  %>%
  group_by(col) %>%
  mutate(id = row_number()) %>%
  ggplot(aes(x = id, y = index_val), size = 0.1) +
  geom_line() +
  facet_wrap(vars(col), scales = "free_y")
#ggsave("sim/result_geodesic.png")


result_better <- purrr::map2_df(var, names, function(var, names){
  data <- cbind(x1, x8, x9, x10, var)
  set.seed(1234)
  result <- animate_dist(data, tour_path =
                           guided_tour(holes(), d= 1,search_f = search_better),
                         sphere = TRUE) %>%
    mutate(col = names,
           method = "better")
  result
})


result_better  %>%
  group_by(col) %>%
  mutate(id = row_number()) %>%
  ggplot(aes(x = id, y = index_val), size = 0.1) +
  geom_line() +
  facet_wrap(vars(col), scales = "free_y")
#ggsave("sim/result_better.png")

result_better_random <- purrr::map2_df(var, names, function(var, names){
  data <- cbind(x1, x8, x9, x10, var)
  set.seed(1234)
  result <- animate_dist(data, tour_path =
                           guided_tour(holes(), d = 1, search_f = search_better_random),
                         sphere = TRUE) %>%
    mutate(col = names,
           method = "better_random")
  result
})

result_better_random %>%
  group_by(col) %>%
  mutate(id = row_number()) %>%
  ggplot(aes(x = id, y = index_val), size = 0.1) +
  geom_line() +
  facet_wrap(vars(col), scales = "free_y")
#ggsave("sim/result_better_random.png")

result_all <- bind_rows(result_geodesic, result_better, result_better_random)
#save(result_all, file = "sim/result_all.rda")






