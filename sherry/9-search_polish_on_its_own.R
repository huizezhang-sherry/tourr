devtools::load_all()
set.seed(123456)
polish_1d <- tourr::animate_dist(data, tour_path = guided_tour(holes(), d = 1,
                                                    search_f = search_polish2, cooling = 1),
                            rescale = FALSE)



polish_1d %>% filter(info %in% c("interpolation", "polish_best")) %>% view
polish_1d %>% filter(info == c("start", "interpolation")) %>% mutate(id = row_number()) %>%
  ggplot(aes(x = id, y = index_val)) +
  geom_line() +
  geom_point()



devtools::load_all()
set.seed(123456)
polish_2d <- tourr::animate_dist(data_mult[,c(1,2,7:10)],
                            tour_path = guided_tour(holes(), d = 2,
                                                    search_f = search_polish2, cooling = 1),
                            rescale = FALSE)

polish_2d %>% filter(info %in% c("interpolation", "polish_best")) %>% view
polish_2d %>% filter(info == c("start", "interpolation")) %>% mutate(id = row_number()) %>%
  ggplot(aes(x = id, y = index_val, col = )) +
  geom_line() +
  geom_point(aes(col = as.factor(tries)))


##############################
save(polish_1d, file = "sherry/data/polish_1d.rda")
save(polish_2d, file = "sherry/data/polish_2d.rda")
