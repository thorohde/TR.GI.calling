#' @export loess_prediction


loess_prediction <- \(.d, .me, .query, span = 0.4, .degree = 1) {
  .d[,"main"] <- .me
  .d[,"query"] <- .query # combinatorial effect of specific Cas12a gRNA query

  model_data <- data.frame(.d[,c("main", "query")])

  .too_many_nas <- max(
    c(sum(is.na(model_data$main)) / length(model_data$main),
      sum(is.na(model_data$query)) / length(model_data$query)) > 0.9)

  if (.too_many_nas) {.d[,"pred"] <- rep(NA, nrow(.d))}# model can only train if most of the entries are not NA
  if (!.too_many_nas) {
    model <- stats::loess(formula = query ~ main,
                          data = model_data,
                          span = span,
                          degree = .degree)
    .d[,"pred"] <- stats::predict(object = model, newdata = model_data$main)
    .d[,"GI"] <- .d[,"query"] - .d[,"pred"]} # the guide GIs are computed as the residuals between the query LFC and the loess prediction
  return(.d)
}
