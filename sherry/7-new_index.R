load(here::here("sherry", "data", "data.rda"))
# load(here::here("sherry", "data", "data_mult.rda"))
# data <- data_mult %>% dplyr::select(x1:x2, x7:x10)

set.seed(123456)
d <- 1
basis1 <- basis_random(ncol(data), d)
basis2 <- basis_random(ncol(data), d)
geo <- geodesic_path(basis1, basis2)
cur_dist <- 0
target_dist <- geo$dist
pos <- seq(cur_dist, target_dist, 1/150)
interp <- geodesic_info(basis1, basis2)
path <- tibble(index_val = map_dbl(pos, ~step_fraction(interp = interp,fraction = .x) %>% calc_kol(data = data))) %>% mutate(id = row_number())
path %>% ggplot(aes(x = id, y = index_val)) +
  geom_line() +
  geom_point()


path <- tibble(index_val = map_dbl(pos, ~step_fraction(interp = interp,fraction = .x) %>% calc_index(data = data))) %>% mutate(id = row_number())
path %>% ggplot(aes(x = id, y = index_val)) +
  geom_line() +
  geom_point()




set.seed(123456)
a <- tourr::animate_dist(data, tour_path = guided_tour(kol(), d = 1,
                                                       search_f = search_geodesic_latest),
                         rescale = FALSE) %>% mutate(id = row_number()) %>% clean_info()

last_tries <- a$tries[which.max(a$loop)]

index_val_path <- a %>%  ggplot(aes(x = id, y = index_val, col = info)) + geom_point()

direction <- a %>%
  filter(tries == last_tries, info != "line_search") %>%
  ggplot(aes(x = id, y = index_val, col = info)) +
  geom_point() +
  xlim(1, max(a$id)) +
  scale_color_manual(values = scales::hue_pal()(6)[c(2:3, 5:6)]) +
  theme(legend.position =  "none")

direction / index_val_path +
  plot_layout(heights = c(1, 2), guides = "collect")



clean_info <- function(data){
  data %>%
    mutate(info = fct_relevel(info, c("start",
                                      "direction_search", "best_direction_search",
                                      "line_search", "best_line_search",
                                      "interpolation")),
           info2 = as.factor(ifelse(info %in% c("best_direction_search","best_line_search"),
                                    "best", "normal")),
           info3 = ifelse(info2 == "normal", as.character(info),
                          str_sub(info, start = 6L, end = -1L)))
}
