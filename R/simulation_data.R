library(tidyverse)
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



var <- list(x2 = x2)



names <- list("x2")

result_geodesic_old <- purrr::map2_df(var, names, function(var, names){
  data <- cbind(x1, x8, x9, x10, var)

  result <- animate_dist(data, tour_path =
                           guided_tour(holes(), d = 1,
                                       search_f = search_geodesic_latest),
                         sphere = TRUE) %>%
    mutate(col = names)
  result
}) %>%
  mutate(method = "geodesic")

result_geodesic_old  %>%
  filter(info == "interpolation")  %>%
  group_by(col) %>%
  mutate(id = row_number()) %>%
  ggplot(aes(x = id, y = index_val), size = 0.1) +
  geom_line() +
  facet_wrap(vars(col))

#save(result_geodesic_old, file = "result_geodesic_old.rda")


result_better <- purrr::map2_df(var, names, function(var, names){
  data <- cbind(x1, x8, x9, x10, var)
  result <- animate_dist(data, tour_path =
                           guided_tour(holes(), d= 1,search_f = search_better),
                         sphere = TRUE) %>%
    mutate(col = names)
  result
})%>%
  mutate(method = "better")

result_better  %>% filter(col == "x3") %>% View()
  filter(info == "interpolation")  %>% View()
  group_by(col) %>%
  mutate(id = row_number()) %>%
  ggplot(aes(x = id, y = index_val), size = 0.1) +
  geom_line() +
  facet_wrap(vars(col))




result_better_random <- purrr::map2_df(var, names, function(var, names){
  data <- cbind(x1, x8, x9, x10, var)
  result <- animate_dist(data, tour_path =
                           guided_tour(holes(), d = 1, search_f = search_better_random),
                         sphere = TRUE) %>%
    mutate(col = names)
  result
}) %>%
  mutate(method = "better_random")

result_better_random %>%
filter(info == "interpolation")  %>%
  group_by(col) %>%
  mutate(id = row_number()) %>%
  ggplot(aes(x = id, y = index_val), size = 0.1) +
  geom_line() +
  facet_wrap(vars(col))

data <- bind_rows(result_geodesic, result_better, result_better_random)
#save(data, file = "optim_data.rda")


data  %>%
  filter(info == "interpolation")  %>%
  group_by(col, method) %>%
  mutate(id = row_number()) %>%
  ggplot(aes(x = id, y = index_val, col = method), size = 0.1, alpha = 0.5) +
  geom_line() + geom_point()+
  facet_wrap(vars(col))

ggsave("optim_comparison2.png")


