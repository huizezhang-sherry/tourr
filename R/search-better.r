#' Generate nearby bases, e.g. for simulated annealing.
#' @keywords internal
basis_nearby <- function(current, alpha = 0.5, method = "linear") {
  method <- match.arg(method, c("linear", "geodesic"))
  new <- basis_random(nrow(current), ncol(current))

  switch(method,
    linear =   orthonormalise((1 - alpha) * current + alpha * new),
    geodesic = step_fraction(geodesic_info(current, new), alpha)
  )
}


#' Search for a better projection near the current projection.
#' @keywords internal
search_better <- function(current, alpha = 0.5, index, max.tries = 125,
  method = "linear", cur_index = NA) {
  #browser()
  if (is.na(cur_index)) cur_index <- index(current)

  try <- 1

  while(try < max.tries) {

    new_basis <- basis_nearby(current, alpha, method)
    new_index <- index(new_basis)

    record_temp <- tibble(basis = list(new_basis),
                          index_val = new_index,
                          info = "random_search",
                          tries = tries,
                          loop = try)
    record <- record %>% bind_rows(record_temp)

    if (new_index > cur_index) {
      cur_index <<- new_index

      record <<- record %>%
        mutate(row = row_number(),
               info = ifelse(row == max(row), "new_basis", info)) %>%
        dplyr::select(-row)

      return(list(record = record,
                  target = record$basis[[length(record$basis)]]))
    }

    try <- try + 1
  }

  NULL
}

#' Search for better projection, with stochastic component.
#' @keywords internal
search_better_random <- function(current, alpha = 0.5, index,
  max.tries = 125, method = "linear", eps = 0.001, cur_index = NA
) {
  #browser()
  if (is.na(cur_index)) cur_index <- index(current)

  try <- 1
  while(try < max.tries) {
    new_basis <- basis_nearby(current, alpha, method)
    new_index <- index(new_basis)
    cat("new_index", new_index, "cur_index:", cur_index, "\n")

    record_temp <- tibble(basis = list(new_basis),
                          index_val = new_index,
                          info = "random_search",
                          tries = tries,
                          loop = try)
    record <- record %>% bind_rows(record_temp)

    if (new_index > cur_index) {
      cur_index <<- new_index

      record <<- record %>%
        mutate(row = row_number(),
               info = ifelse(row == max(row), "new_basis", info)) %>%
        dplyr::select(-row)

      return(list(record = record,
                  target = record$basis[[length(record$basis)]]))

    }
    else if (abs(new_index-cur_index) < eps) {
      new_basis <- basis_random(nrow(current), ncol(current))

      record_temp <- tibble(basis = list(new_basis),
                             index_val = index(new_basis),
                             info = "random_step",
                             tries = tries,
                            loop = try)

      record <<- record %>% bind_rows(record_temp)
      return(list(record = record,
                  target = record$basis[[length(record$basis)]]))

    }

    try <-  try + 1
  }
}



search_annealing_adaptive <- function(current, alpha = 0.5, index,
                             max.tries = 25, method = "geodesic",
                             cur_index = NA){

  cat("test", "\n")
  if (is.na(cur_index)) cur_index <- index(current)
  #cat("print", current, "\n")
  #try <- 1
  #count <- 0
  #theta <- theta_min
  #cat("Old", cur_index, "try", try, "\n")


  try <- 1
  while(try < max.tries){
    new_basis <- basis_nearby(current, alpha, method)
    new_index <- index(new_basis)
    cat("new index", new_index, "cur_index", cur_index, "\n")

    # compute r and alpha for metropolis algorithm
    alpha <- min(exp((new_index - cur_index)/0.1),1)
    cat("print alpha: ", alpha, "\n")
    basis_index <- MHaccept(alpha, new_basis,new_index, current, cur_index, try = try)


    # if (new_index > cur_index){
    #   count <- count + 1
    #   return(new_basis)
    #   cat("New", new_index, "try", try, "\n")
    #   cat("update count", count, "\n")
    # }else{
    #
    #   cat("print r", r, " print alpha", alpha, "\n")
    #
    #   if(r < alpha){
    #     return(new_basis)
    #     count <- count + 1
    #     cat("update count", count, "\n")
    #   }else{
    #     count <- 0
    #     cat("clear count", count, "\n")
    #   }
    # }

    #theta <-  theta_min + lambda * log(1 + count)
    try <- try + 1
  }

  current <- basis_index

  cat("Final projection: \n")
  if (ncol(current)==1) {
    for (i in 1:length(current))
      cat(sprintf("%.3f",current[i])," ")
    cat("\n")
  }
  else {
    for (i in 1:nrow(current)) {
      for (j in 1:ncol(current))
        cat(sprintf("%.3f",current[i,j])," ")
      cat("\n")
    }
  }
  cat("No better bases found after ", max.tries, " tries.  Giving up.\n",
      sep="")

  NULL
}


search_annealing <- function(current, alpha = 0.5, index,
                             max.tries = 5, method = "geodesic",
                             cur_index = NA, c_init = 0.1, delta = 0.9, varepsilon = 0.01){
  if (is.na(cur_index)) cur_index <- index(current)

  # try <- 1
  # cur_index_v <- c()
  # cur_index_v[1] <- c_init
  # c_mean <- c_init
  # c_var <- 100
  # c <- cur_index_v[1]
  #
  # criteria <- c_var/(c*(c_init - c_mean))
  #
  # cat("print criteria", criteria, "\n")


  for(try in 1:5){
    # generating neighbour basis
    new_basis <- basis_nearby(current, alpha, method)
    new_index <- index(new_basis)

    cat("cur_index", cur_index,"new_index", new_index, "\n")

    # compute r and alpha for metropolis algorithm
    alpha <- min(exp((new_index - cur_index)/c),1)
    cat("print alpha: ", alpha, "\n")
    basis <- MHaccept(alpha, new_basis, current, try = try)

    index[try] <- new_index

    out <- max(index)

    # update the cooling scheme
    # cur_index_v[try + 1] <- c*(1 + (log(1 + delta) * c)/3 * sqrt(c_var))^(-1)
    # c_mean <- mean(cur_index_v)
    # c_var <- var(cur_index_v)
    # cat("print c_mean",c_mean, "\n")
    #
    # criteria <- c_var/(c*(c_init - c_mean))
    #
    # cat("print criteria", criteria, "\n")

  }

  cat("No better bases found after ", max.tries, " tries.  Giving up.\n",
      sep="")
  cat("Final projection: \n")
  if (ncol(current)==1) {
    for (i in 1:length(current))
      cat(sprintf("%.3f",current[i])," ")
    cat("\n")
  }
  else {
    for (i in 1:nrow(current)) {
      for (j in 1:ncol(current))
        cat(sprintf("%.3f",current[i,j])," ")
      cat("\n")
    }
  }

  return(out)
}


MHaccept = function(alpha,new_basis,new_index, current, cur_index, try){

  if(alpha==1){
    cat("succeed in accept the new basis", "try", try, "\n")
    current <- new_basis
    cur_index <- new_index

  } else if (runif(1,0,1)<=alpha){
    cat("succeed in accept the new basis", "try", try, "\n")
    current <- new_basis
    cur_index <- new_index

  } else {
    cat("fail to accept the new basis", "try", try, "\n")

  }

  return(current)
}

