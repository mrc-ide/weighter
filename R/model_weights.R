##' Rank models according to the error
##'
##' When multiple models have made
##' predictions, we want to rank them
##' by their error, where error can
##' be any metric e.g. Root mean squared
##' error or likelihood. Each model
##' is ranked within the groups defined
##' by \code{groupvars}
##'
##' @title Assign ranks to models
##' @param pred data.frame with at
##' least one column called model
##' @param groupvars quoted variable
##' names used to define groups within
##' which models should be ranked.
##' @param errvar quoted column name
##' that contains the metric used to
##' rank models
##' @return data.frame with the same
##' structure as \code{pred} and an
##' extra column called rank which
##' contains the model ranks within
##' the groups defined by \code{groupvar}
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
