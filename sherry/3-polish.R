library(tidyverse)
library(foreach)
library(doFuture)
registerDoFuture()
plan(multicore)
load(here::here("sherry", "data", "data.rda"))
load(here::here("sherry", "data", "data_mult.rda"))


set.seed(123456)
polish_holes <- animate_dist(data, tour_path = guided_tour(holes(), d = 1,
                                                search_f = search_geodesic_latest),
                  rescale = FALSE, polish = TRUE,
                  nloop = 20, polish_alpha = 0.005)

####################################
set.seed(123456)
polish_holes_mult <- animate_dist(data_mult[,c(1,2,7:10)], tour_path = guided_tour(holes(), d = 2,
                                                search_f = search_geodesic_latest),
                  rescale = FALSE, polish = TRUE,
                  nloop = 30, polish_alpha = 0.05)


####################################
# this simulation uses search_polish_old() in animate_polish
# it improves version 3 by allowing for outputing all the nearby bases in the sampling process
set.seed(123456)
polish_holes_old_mult <- tourr::animate_dist(data_mult[,c(1,2,7:10)], tour_path = guided_tour(holes(), d = 2,
                                                                                              search_f = search_geodesic_latest),
                                             rescale = FALSE, polish = TRUE,
                                             nloop = 30, polish_alpha = 0.05)

polish_holes_old_mult_record <- polish_holes_old_mult$record
polish_holes_old_mult_sim <- polish_holes_old_mult$sim


####################################
# search_polish modified
devtools::load_all()
set.seed(123456)
polish_modified_mult <- tourr::animate_dist(data_mult[,c(1,2,7:10)],
                                            tour_path = guided_tour(holes(), d = 2,
                                                                    search_f = search_geodesic_latest),
                                            rescale = FALSE, polish = TRUE,
                                            nloop = 30, polish_alpha = 0.05)

polish_mult_modified_record <- polish_modified_mult$record
polish_mult_modified_sim <- polish_modified_mult$sim

####################################
# currently only for holes index - I have problem using the index() function inside the generator
# set.seed(123456)
# kol <- animate_dist(data, tour_path = guided_tour(kol_cdf(), d = 1,
#                                                   search_f = search_geodesic_latest),
#                     rescale = FALSE, polish = TRUE)
#
#
# polish <- kol %>% filter(info == "polish") %>% pull(basis) # the first one is the ending basis

####################################

save(polish_holes, file = "sherry/data/polish_holes.rda")
save(polish_holes_mult, file = "sherry/data/polish_holes_mult.rda")

# the old polish_mult_record & sim
# polish_mult_record <- polish_holes_better_stop_mult$record
# save(polish_mult_record, file = "sherry/data/polish_mult_record.rda")
#
# polish_mult_sim <- polish_holes_better_stop_mult$sim
# save(polish_mult_sim, file = "sherry/data/polish_mult_sim.rda")



save(polish_holes_old_mult_record, file = "sherry/data/polish_holes_old_mult_record.rda")
save(polish_holes_old_mult_sim, file = "sherry/data/polish_holes_old_mult_sim.rda")





save(polish_mult_modified_record, file = "sherry/data/polish_mult_modified_record.rda")
save(polish_mult_modified_sim, file = "sherry/data/polish_mult_modified_sim.rda")


# old polish things
# compute_geodesic_alpha <- function(delta, polish_alpha){
#
#   animate_dist(data, tour_path =
#                            guided_tour(holes(), d = 1,delta = delta,
#                                        search_f = search_geodesic_latest),
#                polish = TRUE,
#                polish_alpha = polish_alpha,
#                sphere = TRUE) %>%
#     mutate(delta = delta, polish_alpha = polish_alpha) %>%
#     mutate(method = "geodesic")
#
# }
#
# set.seed(123456)
# delta <- c(0.01, 0.1, 0.5, 0.9)
# polish_alpha <- c(0.01, 0.05, 0.1,0.2, 0.3,0.4, 0.5, 0.7, 0.9)
# grid <- expand.grid(delta = delta, polish_alpha = polish_alpha)
# geodesic_polished <- foreach(i = 1:nrow(grid), .combine = "rbind") %do%{
#   set.seed(123456)
#   compute_geodesic_alpha(grid$delta[i], grid$polish_alpha[i])
# }
#
# # save(geodesic_polished, file = "sherry/data/geodesic_polished.rda")
#
#
# pca <- geodesic_alpha %>% compute_pca() %>%
#   filter(info %in% c("start", "interpolation"))
#
# pca %>%
#   ggplot(aes(x= PC1, y = PC2, col = as.factor(delta))) +
#   geom_point() +
#   geom_point(data = filter(pca, info == "start"), col = "red") +
#   theme(aspect.ratio = 1) +
#   facet_wrap(vars(delta))
#
#
#
