##' Mapping of groupvar to models run
##'
##' In some cases, we might not have the same set of models run for
##' a grouping variable. For example, say the grouping variable is
##' country, and we have Models \eqn{m1}, \eqn{m2} and \eqn{m3}
##' run for country \eqn{c1}, and models \eqn{m1, m2, m3, m4} run
##' for country \eqn{c2}. For this example, \code{groupvar_to_model} returns
##' a named list where names are formed by concatenating models
##' and elements are countries for which a particular set of models has been run.
##' (m1_m2_m3 = c1, m1_m2_m3_m4 = c2)
##'
##'
##' @title Mapping of groupvars to model
##' @inheritParams model_ranks
##' @return list
##' @author Sangeeta Bhatia
##' @export
groupvar_to_model <- function(pred, groupvars) {

  indices <- list()
  for (groupvar in groupvars) {
    indices <- append(
      x = indices,
      values = list(pred[[groupvar]])
    )
  }
  out <- split(pred, indices, drop = TRUE)
  out <- sapply(
    out,
    FUN = function(x) {
      unique(as.character(x$model))
    },
    simplify = FALSE
  )

  models <- unique(out)
  grpvar_to_model <- vector(
    mode = "list", length = length(models)
  )
  names(grpvar_to_model) <- sapply(models, paste, collapse = "_")
  for (model in models) {
    idx <- Filter(function(x) identical(x, model), out)
    grpvar_to_model[[paste(model, collapse = "_")]] <- names(idx)
  }

  grpvar_to_model

}

##' Mapping of models to groups for which they have been run
##'
##' In some cases, we might not have the same set of models run for
##' a grouping variable. For example, say the grouping variable is
##' country, and we have Models \eqn{M_1}, \eqn{M_2} and \eqn{M_3}
##' run for country \eqn{C_1}, and models \eqn{M_1, M_2, M_3, M_4} run
##' country \eqn{C_2}. For this example, this function will return the
##' list
##' {m_1 = {c_1, c_2}, m_2 = {c_1, c_1}, m_3 = {c_1, c_2}, m_4 = {c_2}}
##' This can be used to identify for which countries a give model has
##' been run.
##' @title Mapping of models to groups for which they have been run
##' @inheritParams model_ranks
##' @return list
##' @author Sangeeta Bhatia
##' @export
model_to_groupvar <- function(pred, groupvars) {


  sapply(
    split(pred, pred$model, drop = TRUE),
    FUN = function(x) {
      as.character(x[[groupvars]])
    }
  )

}
