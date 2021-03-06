##' Rank models according to the error
##'
##' When multiple models have made predictions, we want to rank them
##' by their error, where error can be any metric e.g. Root mean
##' squared error or likelihood. Each model is ranked within the
##' groups defined by \code{groupvars}
##'
##'
##'
##'
##'
##' @title Assign ranks to models
##' @param pred data.frame with at least one column called model
##'
##' @param groupvars quoted variable names used to define groups within
##' which models should be ranked.
##'
##' @param errvar quoted column name that contains the metric used to
##' rank models
##'
##' @return data.frame with the same structure as \code{pred} and an
##' extra column called rank which contains the model ranks within
##' the groups defined by \code{groupvar}
##'
##'
##' @author Sangeeta Bhatia
##' @export
##' @examples
##' pred <- data.frame(
##'   models = as.factor(c("a", "b", "c", "a", "b", "c")),
##'   country = c("C1", "C1", "C1", "C2", "C2", "C2"),
##'   error = c(6L, 1L, 3L, 4L, 7L, 9L),
##'   date = c("1", "1", "1", "2", "2", "2"),
##'   stringsAsFactors = FALSE
##' )
##' model_ranks(pred, c("country", "date"), "error")
model_ranks <- function(pred, groupvars, errvar) {

  indices <- list()
  for (groupvar in groupvars) {
    indices <- append(
      x = indices,
      values = list(pred[[groupvar]])
    )
  }

  pred_by <- by(
    pred,
    INDICES = indices,
    FUN = function(x){
      x$rank <- rank(x[[errvar]])
      x
    },
    simplify = TRUE
  )

  do.call(
    what = 'rbind', args = unname(pred_by)
  )

}

##' Compute model weights from model ranks
##'
##' In a group defined by \code{groupvars}, the weight of a model is
##' defined as
##' \deqn{weight(M) = \sum\limits_{i = 1}^{M}{K(i) / i}}
##' where \eqn{K(i)} is the number of times model M is ranked \eqn{i}
##' among \eqn{M_1, M_2, \dots M} models.
##' @title Compute Model Weights
##' @param pred data.frame that has at least columns specified via
##' parameters \code{groupvars} and \code{rank}. pred is expected
##' to have the same number of elements of grouping variable within
##' each group. That is, each model should have been run for the same
##' number of elements of groupvar. If this is not the case, use
##' \code{model_weights} instead.
##' @param groupvars quoted variable names used to define groups within
##' which model weights are computed. Defaults to model.
##' @param rankvar quoted variable names that contain model ranks.
##' @seealso \code{model_ranks} \code{model_weights}
##' @return a data.frame with columns model and weight where weight is
##' the unnormalised weight of the model
##' @author Sangeeta Bhatia
##' @export
##' @examples
##'
##' pred <- data.frame(
##'   model = as.factor(c("a", "b", "c", "a", "b", "c")),
##'   country = c("C1", "C1", "C1", "C2", "C2", "C2"),
##'   error = c(6L, 1L, 3L, 4L, 7L, 9L),
##'   date = c("1", "1", "1", "2", "2", "2"),
##'   stringsAsFactors = FALSE
##' )
##' ranked <- model_ranks(pred, c("country", "date"), "error")
##' model_weights_in_group(ranked)
model_weights_in_group <- function(pred,
                                   groupvars = "model",
                                   rankvar = "rank") {

  indices <- list()
  for (groupvar in groupvars) {
    indices <- append(
      x = indices,
      values = list(pred[[groupvar]])
    )
  }
  ## Number of predictions by each model.
  npreds <- sapply(
    split(pred, indices), nrow
  )

  if (! max(npreds) == min(npreds)) {
    stop(
      "Number of predictions made by each model are not the same"
    )
  }

  ## TODO Generalise so that it works with more than 1 groupvar
  out <- as.data.frame(table(pred[[groupvars]], pred[[rankvar]]))
  ## Var1 is model name
  ## Var2 is rank
  ## Freq is the number of times a model is assigned a given rank
  out <- by(
    out,
    INDICES = out$Var1,
    FUN = function(x){
      wt <- sum(x$Freq / as.integer(x$Var2))
      wt <- round(wt, digits = 1)
      data.frame(
        model = x$Var1[1],
        weight = wt
      )
    },
    simplify = TRUE
  )
  out <- do.call(what = 'rbind', args = out)
  out$npreds[match(x = names(npreds), table = out$model)] <- npreds

  out
}

##' Compute Model Weights
##'
##' Compute model weights within each group
##' @title Compute Model Weights
##' @inheritParams model_ranks
##'
##' @return named list where the names are the combinations of
##' models run for a group, and each element of the list is a
##' data.frame containing model weights outputted from
##' \code{model_weights_in_group}
##' @seealso model_weights_in_group
##' @author Sangeeta Bhatia
##' @export
model_weights <- function(pred, groupvars, errvar) {

  ## Identify countries that have the same set of models
  mapping <- groupvar_to_model(pred, groupvars)
  models <- names(mapping)
  out <- vector(
    mode = "list", length = length(models)
  )
  names(out) <- sapply(models, paste, collapse = "_")

  for (combo in names(mapping)) {
    idx <- pred[[groupvars]] %in% mapping[[combo]]
    df <- droplevels(pred[idx, ])
    ranked <- model_ranks(df, groupvars, errvar)
    out[[combo]] <- model_weights_in_group(ranked)
  }

  out

}
