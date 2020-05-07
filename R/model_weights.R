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
##' parameters \code{groupvars} and \code{rank}
##' @param groupvars quoted variable names used to define groups within
##' which model weights are computed. Defaults to model.
##' @param rankvar quoted variable names that contain model ranks.
##' @seealso \code{model_ranks}
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
##' model_weights(ranked)
model_weights <- function(pred, groupvars = "model", rankvar = "rank") {

  indices <- list()
  for (groupvar in c(groupvars)) {
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
    warning(
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

  do.call(what = 'rbind', args = out)
}
