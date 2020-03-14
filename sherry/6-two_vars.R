library(tidyverse)
library(foreach)

set.seed(1234)
x1 <- rnorm(1000, 0, 1)
x2 <- c(rnorm(500, -3, 1), rnorm(500, 3, 1))
x6 <- c(rnorm(330, -3, 1), rnorm(340, 0, 1), rnorm(330, 3,1))
X7 <- c(rnorm(500, -5, 1), rnorm(500, 5, 1))
x8 <- rnorm(1000, 0, 1)
x9 <- rnorm(1000, 0, 1)
x10 <- rnorm(1000, 0, 1)

origin_dt_bi <- tibble::tibble(x1 = x1, x2 = x2, X6 = X6,
                            x8 = x8, x9 = x9,x10 = x10) %>%
  gather(names, values) %>%
  mutate(names = as_factor(names),
         names = fct_relevel(names, levels = c("x1", "x2", "X6","x8", "x9", "x10")))

origin_dt_bi %>%
  ggplot(aes(x = values)) +
  geom_histogram(binwidth = 0.5) +
  geom_density(aes(y=0.5 * ..count..)) +
  facet_wrap(vars(names), ncol = 3)

#save(origin_dt,file = "sherry/data/origin_dt_bi.rda")

data_bi <- cbind(x1, x2, X6, x8, x9, x10)
#save(data,file = "sherry/data/data_bi.rda")

temp <- scale(data_bi) %>% as_tibble()

origin_dt_bi <- temp %>%
  gather(names, values) %>%
  mutate(names = as_factor(names),
         names = fct_relevel(names, levels = c("x1", "x2", "X6","x8", "x9", "x10")))

origin_dt_bi %>%
  ggplot(aes(x = values)) +
  geom_histogram(binwidth = 0.5) +
  geom_density(aes(y=0.5 * ..count..)) +
  facet_wrap(vars(names), ncol = 3)

set.seed(123456)
a <- tourr::animate_dist(temp, tour_path = guided_tour(holes(), d = 1,
                                                       search_f = search_geodesic_latest),
                         sphere = FALSE, rescale = FALSE)


