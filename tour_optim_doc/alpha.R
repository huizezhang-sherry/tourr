library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(purrr)

set.seed(1234)
x1 <- rnorm(100, 0, 1)
x2 <- c(rnorm(50, -1, 1), rnorm(50, 1, 1))
x8 <- rnorm(100, 0, 1)
x9 <- rnorm(100, 0, 1)
x10 <- rnorm(100, 0, 1)


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

# The choose of alpha parameter requires causion.
# A small alpha value results in a close neighbourhood in the searching
# with a cooling parameter, the searching space will become smaller and smaller.
# Thus, the algorithm may not be able to find the global maximum

# A large alpha value allows for a larger searching space, however,
# it is likely that there exists a basis on the interpolating path that has higher index_val
# this makes the searching inefficient

# have tried that smaller alpha value i.e. 1e-4, 1e-3 would not make the search
alpha <- c(0.01, 0.05, 0.1, 0.5)
cooling <- c(0.8, 0.85, 0.9, 0.95, 0.99, 0.995)
paras <- expand.grid(alpha, cooling)

set.seed(123456)

library(foreach)
library(doFuture)
registerDoFuture()
plan(multisession)

better_alpha <- foreach(i = 1:nrow(paras), .combine = "rbind") %do%{
  compute_better_alpha(paras$Var1[i], paras$Var2[i])
}

better_random_alpha <- foreach(i = 1:nrow(paras), .combine = "rbind") %do%{
  compute_random_alpha(paras$Var1[i], paras$Var2[i])
}

# better_alpha <- map2_df(paras$Var1, paras$Var2, compute_better_alpha)
# better_random_alpha <- map2_df(paras$Var1, paras$Var2, compute_random_alpha)
sim_alpha <- rbind(better_alpha, better_random_alpha)

better_interp <- better_alpha %>%
  filter(info == "interpolation") %>%
  group_by(alpha, cooling) %>%
  mutate(id = row_number())

better_interp %>%
  ggplot(aes(x = id, y = index_val, col = as.factor(cooling))) +
  geom_line() +
  facet_wrap(vars(alpha))

################
max_index_val <- better_alpha %>%
  filter(info == "new_basis") %>%
  group_by(alpha, cooling) %>%
  filter(index_val == max(index_val)) %>%
  ungroup() %>%
  mutate(alpha = as.factor(alpha),
         cooling = as.factor(cooling))

best_better <- max_index_val %>% filter(index_val == max(index_val))

max_index_val %>%
  ggplot(aes(x = alpha, y = cooling)) +
  geom_raster(aes(fill = index_val)) +
  geom_tile(data = best_better, size = 2, fill = NA, col = "red") +
  geom_text(aes(label = round(index_val, 3)), col = "white")

#################
################

random_interp <- better_random_alpha %>%
  filter(info == "interpolation") %>%
  group_by(alpha, cooling) %>%
  mutate(id = row_number())

random_interp %>%
  ggplot(aes(x = id, y = index_val, col = as.factor(cooling))) +
  geom_line() +
  facet_wrap(vars(alpha))

#################
random_max_index_val <- better_random_alpha %>%
  filter(info == "new_basis") %>%
  group_by(alpha, cooling) %>%
  filter(index_val == max(index_val)) %>%
  ungroup() %>%
  mutate(alpha = as.factor(alpha),
         cooling = as.factor(cooling))

best_random <- random_max_index_val %>% filter(index_val == max(index_val))

random_max_index_val %>%
  ggplot(aes(x = alpha, y = cooling)) +
  geom_raster(aes(fill = index_val)) +
  geom_tile(data = best_random, size = 2, fill = NA, col = "red") +
  geom_text(aes(label = round(index_val, 3)), col = "white")


sim_alpha %>%
  filter(info == "interpolation") %>%
  group_by(alpha, method) %>%
  mutate(id = row_number()) %>%
  ggplot(aes(x = id, y = index_val, col = as.factor(method))) +
  geom_line() +
  facet_wrap(vars(alpha))

