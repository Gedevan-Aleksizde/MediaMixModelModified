#' @title convert wider data frame to stan input object
#' @export
#' @param data raw data
#' @param L max lag.
#' @param controls default `"price"`
to_stan_data <- function(data, L, controls = "price"){
  # 各 media の spendings には `media*` という列名を付ける
  # `controls` には文字ベクトルで control variable に使用する列名指定
  X_media <- slide(data, ~as.matrix(dplyr::select(.x, starts_with("media"))), .before = NROW(data) - L)[(NROW(data) + 1 - L):NROW(data)]
  X_media <- sapply(X_media, function(x) x, simplify = "array")
  X_ctrl <- as.matrix(dplyr::select(data, !!{{controls}})[dim(X_media)[3]:NROW(data), ])
  list(
    N = dim(X_media)[1],
    Y = data$sales[dim(X_media)[3]:NROW(data)],
    max_lag = dim(X_media)[3],
    num_media = dim(X_media)[2],
    X_media = X_media,
    num_ctrl = NCOL(X_ctrl),
    X_ctrl = X_ctrl
  )
}

#' @title Return predict values from MMM stan fit object
#' @export
#' @param fit stanfit object.
#' @param newdata same as stan input list. if not specified, return predicted values for fitting data.
#' @param inverse_trans function. inverse transformation applying for the response variable
#' @return matrix, which has (iter x chain) rows and (Times) columns.
predict <- function(fit, newdata = NULL){
  if(is.null(newdata)){
    Y <- rstan::extract(fit, "mu", permuted = F)
    Y <- apply(Y, 3, c)
  } else {
    nchain <- dim(fit)[2]
    iter <- dim(fit)[1]
    N <- dim(rstan::extract(fit, "mu", permuted = F))[3]
    Y <- array(0, dim = c(iter, nchain, N))
    # TODO: TOOOOOOSLOWLY!!!!!
    for(i in 1:iter){
      for(k in 1:nchain){
        for(nn in 1:N){
          for(m in newdata$num_media){
            Y[i, k, nn] <- rstan::extract(fit, "beta_media", permuted = F)[i, k, m] * hill(
              adstock(
                x = newdata$X_media[nn, m, ],
                weights = geo_weight,
                L = newdata$max_lag,
                args = list(
                  alpha = rstan::extract(fit, "retain_rate", permuted = F)[i, k, m],
                  theta = rstan::extract(fit, "delay", permuted = F)[i, k, m]
                )
              ),
              k = rstan::extract(fit, "ec", permuted = F)[i, k, m],
              s = rstan::extract(fit, "slope", permuted = F)[i, k, m]
            )
            Y[i, k, nn] <- Y[i, k, nn] + sum(
              newdata$X_ctrl[nn, ] *
                rstan::extract(fit, "gamma_ctrl", permuted = F)[i, k, ]
              )
          }
        }
      }
    }
    Y <- apply(Y, 3, c) # (iter x chain) x Times matrix
    Y <- Y + as.numeric((rstan::extract(fit, "tau", permuted = F)[, , 1]))
  }
  return(Y)
}


predict <- function(fit, newdata = NULL, inverse_trans = NULL){
  if(is.null(newdata)){
    Y <- rstan::extract(fit, "mu", permuted = F)
    Y <- apply(Y, 3, c)
  } else {
    Y <- apply(
      apply(
        array(
          c(
            rstan::extract(fit, "retain_rate", permuted = F),
            rstan::extract(fit, "delay", permuted = F),
            rstan::extract(fit, "ec", permuted = F),
            rstan::extract(fit, "slope", permuted = F),
            rstan::extract(fit, "beta_medias", permuted = F)
          ),
          dim = c(1000, 4, newdata$num_media, 5)
        ),
        c(1, 2),
        function(params_2d){
          colSums(
            apply(
              newdata$X_media,
              1,
              function(X_t){
                mapply(
                  adstock_o_hill,
                  apply(X_t, 1, list),
                  apply(params_2d, 1, list),
                  MoreArgs = list(L = 13, weights = geo_weight)
                )
              }
            )
          )
        }) + apply(
          rstan::extract(fit, "gamma_ctrl", permuted = F),
          c(1, 2),
          function(gamma){
            newdata$X_ctrl %*% matrix(gamma) 
          }
        ),
      1,
      function(x) x + rstan::extract(fit, "tau", permuted = F)[, , 1]
    )
  }
  if(!is.null(inverse_trans)){
    Y <- inverse_trans(Y)
  }
  return(Y)
}

adstock_o_hill <- function(x, params, L, weights){
  x <- x[[1]]
  params <- params[[1]]
  params[5] * hill(
    adstock(x, weights = weights, L = L, args = list(alpha = params[1], theta =  params[2])),
    k = params[3], s = params[4]
  )
}



#' @name ROAS
#' @title Calculate (Marginal) Return On the Advertising Spendings
#' @export
#' @param fit model parameter object
#' @param data data for extraporation
#' @param reference numeric reference levels for spendings. default is zero.
#' @param inverse_trans function. inverse transformation applying for the response variable
ROAS <- function(fit, data = NULL, L, controls = "price", reference = 0, inverse_trans = NULL){
  if(NROW(data) < L){
    stop(sprintf("input data has only less than %d rows", L))
  }
  newdata <- to_stan_data(data, L = L, controls = controls)
  spend <- colSums(dplyr::select(data, starts_with("media"))[1:(NROW(data) - L + 1), ])
  pred <- predict(fit, newdata = newdata, inverse_trans)
  # TODO
  if(!is.array(reference) && length(reference) == 1){
    newdata$X_media <- array(reference, dim(newdata$X_media))
  } else {
    newdata$X_media <- reference
  }
  pred_ref <- predict(fit, newdata = newdata, inverse_trans)
  d <- expand_grid(
    tibble(ROAS = rowSums(pred - pred_ref)),
    tibble(media = names(spend), x = spend)
  )
  d$ROAS <- with(d, ROAS/x)
  d$x <- NULL
  return(d)
}

#' @export
#' @param rate default `0.01`. counterfactual change rate.
#' @inheritParams  ROAS
mROAS <- function(fit, data = NULL, L, controls = "price", rate = 0.01, inverse_trans = NULL){
  if(NROW(data) < L){
    stop(sprintf("input data has only less than %d rows", L))
  }
  newdata <- to_stan_data(data, L = L, controls = controls)
  spend <- colSums(dplyr::select(data, starts_with("media"))[1:(NROW(data) - L + 1), ])
  pred_ref <- predict(fit, newdata = newdata, inverse_trans = inverse_trans)
  data <- mutate(data, across(starts_with("media"), ~.x * rate))
  newdata <- to_stan_data(data, L = L, controls = controls)
  pred <- predict(fit, newdata = newdata, inverse_trans = inverse_trans)
  d <- expand_grid(
    tibble(mROAS = rowSums(pred - pred_ref)),
    tibble(media = names(spend), x = spend)
  )
  d$mROAS <- with(d, mROAS/x)
  d$x <- NULL
  return(d)
}
