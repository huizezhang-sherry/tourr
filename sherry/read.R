library(tidyverse)

files <- list.files("sherry/SUPA_SIM/")

col_n <- map_chr(1:22, function(x) paste0("t",x))

init <- FALSE

for (f in files){
  # first get indicator label from file name
  ind <- substring(str_extract(f, regex("[:digit:][:alpha:]+")), 2)
  # read file
  d <- read_csv(paste0("sherry/SUPA_SIM/",f), col_names = col_n)
  min_val <- min(d)
  max_val <- max(d)
  # now scaling all entries so overall range is between 0 and 1
  d <- d %>%
    mutate_all(function(x) ((x - min_val) / (max_val - min_val))) %>%
    add_column(indicator = ind) %>%
    add_column(sim = 1:nrow(d))
  if (!init){
    d_all <- d
    init <- TRUE
  }
  else{
    d_all <- rbind(d_all, d)
  }
}

eco_long <- d_long %>% as_tibble() %>% rename(t = variable, var = indicator) %>%
  mutate(t = as.numeric(str_replace(t, "t", "")))

save(eco_long, file = "sherry/data/eco_long.rda")

library(reshape2)
d_long <- melt(d_all, id.vars = c("indicator", "sim"))
ggplot(filter(d_long, sim %in% c(1, 10, 100)), aes(x=variable, y=value, color=factor(sim))) +
  geom_point() +
  facet_wrap(~indicator)


# try with subset
d_3 <- filter(d_all, indicator %in% c("unemply", "cash", "shortTerm"))
library(RColorBrewer)
clrs <- brewer.pal(3, "Dark2")
col <- clrs[as.numeric(as.factor(d_3$indicator))]
library(tourr)
quartz()
animate_groupxy(select(d_3, -sim, -indicator), col=col, group_by= col, rescale=FALSE)

# indicators as variables
library(tidyr)
d_2 <- d_all %>%
  pivot_longer(cols = starts_with("t"),
               names_to = "time") %>%
  pivot_wider(id_cols = c(time, sim),
              names_from = indicator,
              values_from = value) %>%
  dplyr::filter(time=="t2" | time=="t22")

d_3 <- d_all %>%
  pivot_longer(cols = starts_with("t"),
               names_to = "time") %>%
  pivot_wider(id_cols = c(time, sim),
              names_from = indicator,
              values_from = value) %>%
  dplyr::filter(sim==1 | sim==100)

col <- ifelse(d_3$sim==1, "#1B9E77", "#D95F02")
quartz()
animate_xy(as.matrix(dplyr::select(d_2, -time, -sim)), guided_tour(pda_pp(col)), col=col, rescale=FALSE,
           axes="bottomleft")
animate_xy(as.matrix(dplyr::select(d_2, -time, -sim)), col=col, rescale=FALSE, axes="bottomleft")

animate_xy(as.matrix(dplyr::select(d_3, -time, -sim)), col=col, rescale=FALSE, axes="bottomleft")
animate_xy(as.matrix(dplyr::select(d_3, -time, -sim)), guided_tour(pda_pp(col)), col=col, rescale=FALSE,
           pch=20)

#they all have exactly the same starting point (i.e. value at t1)

#attempt instead to look at everything at once and see structures
m <- d_all %>%
  pivot_longer(cols = starts_with("t"),
               names_to = "time") %>%
  pivot_wider(id_cols = c(time, sim),
              names_from = indicator,
              values_from = value) %>%
  dplyr::select(-time, -sim) %>%
  as.matrix()

animate(m, guided_tour(holes()))

animate_slice(m)
