##' Mapping of groupvar to models run
##'
##' In some cases, we might not have the same set of models run for
##' a grouping variable. For example, say the grouping variable is
##' country, and we have Models \eqn{m_1}, \eqn{m_2} and \eqn{m_3}
##' run for country \eqn{c_1}, and models \eqn{m_1, m_2, m_3, m_4} run
##' country \eqn{c_2}. For this example, this function will return the
##' list
##' {c_1 = {m_1, m_2, m_3}, c_2 = {m_1, m_2, m_3, m_4}}
##' This can be used to identify which countries have the same set of
##' models run for them.
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
  out <- split(pred, indices)
  out <- sapply(
    out,
    FUN = function(x) {
      unique(x$model)
    }
  )
  out

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
    split(pred, pred$model),
    FUN = function(x) {
      x[[groupvars]]
    }
  )

}
