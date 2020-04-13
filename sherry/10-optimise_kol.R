load(here::here("sherry", "data", "data.rda"))
load(here::here("sherry", "data", "data_mult.rda"))

# a basic experiment on kol() index with search_geodesic without polishing
set.seed(123456)
kol_geodesic <- tourr::animate_dist(data, tour_path = guided_tour(kol(), d = 1,
                                                          search_f = search_geodesic_latest),
                                             rescale = FALSE, polish = FALSE)


# a basic experiment on kol() index with serach_better without polishing
set.seed(123456)
kol_better <- tourr::animate_dist(data, tour_path = guided_tour(kol(), d = 1,
                                                          search_f = search_better),
                            rescale = FALSE, polish = FALSE)


# a basic experiment on kol_cdf() with search_better without polishing
set.seed(123456)
kol_cdf_better <- tourr::animate_dist(data, tour_path = guided_tour(kol_cdf(), d = 1,
                                                                 search_f = search_better),
                                   rescale = FALSE, polish = FALSE)

# a basic experiment on kol_cdf() with search_better with polishing
# reminder that here I manually change the calc_index in search_polish to calc_col_cdf
set.seed(123456)
kol_cdf_better_polish <- tourr::animate_dist(data, tour_path = guided_tour(kol_cdf(), d = 1,
                                                                     search_f = search_better),
                                       rescale = FALSE, polish = TRUE)


###############################
save(kol_geodesic, file = "sherry/data/kol_geodesic.rda")
save(kol_better, file = "sherry/data/kol_better.rda")
save(kol_cdf_better, file = "sherry/data/kol_cdf_better.rda")
save(kol_cdf_better_polish, file = "sherry/data/kol_cdf_better_polish.rda")
