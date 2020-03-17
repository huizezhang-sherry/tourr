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
