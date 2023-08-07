var_N <- function(x) {
  stats::var(x) * (length(x) - 1) / length(x)
}

#' @import data.table
#' @export
decompose_variance <- function(data, formula, weight = NULL, na.rm = FALSE) {
  outcome <- all.vars(formula)[[1]]
  group <- all.vars(formula)[[2]]
  data <- data.table::as.data.table(data)
  n_na <- sum(is.na(data[[outcome]]))
  if (na.rm == FALSE & n_na > 0) {
    return(data.table(
      stat = c("total", "between", "within"), decomposition = rep(NA, 3), proportion = rep(NA, 3)
    ))
  } else {
    data <- data[!is.na(data[[outcome]])]
  }

  if (is.null(weight)) {
    cols <- c(outcome, group)
    data <- data[, ..cols]
    data[, weight := 1]
  } else {
    cols <- c(outcome, group, weight)
    data <- data[, ..cols]
    setnames(data, weight, "weight")
  }

  mv <- data[, .(
    p = sum(weight),
    mean = Hmisc::wtd.mean(get(outcome), weights = weight),
    var = Hmisc::wtd.var(get(outcome), weights = weight, normwt = TRUE, method = "ML")
  ), by = group]
  mv[, p := p / sum(p)]

  decomposition <- c(
    Hmisc::wtd.var(data[[outcome]], weights = data[["weight"]], normwt = TRUE, method = "ML"),
    mv[, Hmisc::wtd.var(mean, weights = p, normwt = TRUE, method = "ML")],
    mv[, sum(p * var)]
  )

  data.table(
    stat = c("total", "between", "within"),
    decomposition = decomposition,
    proportion = decomposition / decomposition[1]
  )
}
