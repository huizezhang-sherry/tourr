load("result_geodesic.rda")
library(foreach)
library(factoextra)

compute_pca <- function(names){
  rows <- result_geodesic_old %>% filter( col == names)
  pca <- foreach(i  = 1:nrow(rows), .combine = "rbind") %do%{
    t(rows$basis[[i]])
  }

  res.pca <- prcomp(pca)
  loadings <- res.pca$rotation[,c(1,2)]
  pca2 <- cbind(pca, pca %*% loadings)

  result <- as_tibble(pca2) %>% mutate(id = 1: nrow(pca2),
                                           info = rows$info,
                                           V5_names = rows$col,
                                       tries = rows$tries,
                                       index_val = rows$index_val)

  colnames(result)[1:4] <- c("x1", "x8", "x9", "x10")

  return(result)

}

index_val <- result_geodesic %>% dplyr::filter(col == "x2")
pca_result <- compute_pca("x2")

# colored by index_val
# pca_result %>%
#   ggplot(aes(x= PC1, y = PC2, col = index_val)) +
#   geom_point() +
#   theme(aspect.ratio = 1)
#
# # colored by id
# pca_result %>%
#   ggplot(aes(x= PC1, y = PC2, col = as.factor(tries))) +
#   geom_path() +
#   theme(aspect.ratio = 1)
#
# # color by start/end
# pca_result %>%
#   ggplot(aes(x= PC1, y = PC2)) +
#   geom_point() +
#   geom_point(data = filter(pca_result, id ==1), color = "red") +
#   geom_point(data = filter(pca_result, info == "linear_search_best"),
#            color = "blue") +
#   theme(aspect.ratio = 1)

# colour by parts of optimisation
ggplot(data = pca_result, aes(x= PC1, y = PC2, col = info)) +
  geom_point() +
  theme(aspect.ratio = 1)

# colour the interpolation from all the bases
pca_result2 <- pca_result %>%
  filter(info == "interpolation") %>%
  mutate(id2 = lag(id),
         info2 = ifelse(is.na(id2), "start",
                        ifelse(id2 - id == -1, "interpolation", "target")))

ggplot(data = pca_result, aes(x = PC1, y = PC2)) +
  geom_point() +
  geom_point(data = pca_result2, aes(col = info2)) +
  theme(aspect.ratio = 1)

# interpolation path
ggplot(data =pca_result2,
       aes(x= PC1, y = PC2)) +
  geom_point(aes(col = info2)) +
  geom_path(linetype = "dashed") +
  theme(aspect.ratio = 1)

#####
