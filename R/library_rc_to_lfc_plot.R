#' @export library_rc_to_lfc_plot

library_rc_to_lfc_plot <- function(lib_readcounts,
                                   lfc_list,
                                   rc_max = 1000,
                                   lfc_min = 5, lfc_max = 10,
                                   rc_binwidth = 50,
                                   lfc_binwidth = 0.5) {

  lfc_ths_pos <- seq(0, lfc_max, lfc_binwidth)
  lfc_ths_neg <- seq(lfc_min, -lfc_binwidth, lfc_binwidth)

  rc_ths_low <- c(0, seq(rc_binwidth, rc_max, by = rc_binwidth))
  rc_ths_high <- c(seq(rc_binwidth, rc_max, by = rc_binwidth), 10*rc_max)

  plot_array <- array(data = as.character(c(0, seq(rc_binwidth, rc_max, by = rc_binwidth))),
                            dim = sapply(list(as.character(c(lfc_ths_neg, lfc_ths_pos))), length),
                            dimnames = list(as.character(c(lfc_ths_neg, lfc_ths_pos))))

  #plot_array <- empty_array(dnames = list(as.character(c(lfc_ths_neg, lfc_ths_pos)),
  #                                              values = as.character(c(0, seq(rc_binwidth, rc_max, by = rc_binwidth)))))

  .d <- data.table::data.table(lib_rc = lib_readcounts,
                   lfc_min = Reduce(pmin, lfc_list),
                   lfc_max = Reduce(pmax, lfc_list))

  for (i in seq_along(1:length(rc_ths_low))) {
    .d_in_bin <- .d[!is.na(get("lib_rc")) & data.table::between(get("lib_rc"), rc_ths_low[i], rc_ths_high[i])]
    if (.d_in_bin[, .N] == 0) {
      plot_array[as.character(c(lfc_ths_neg, lfc_ths_pos)),i] <- 0
    } else {
      plot_array[as.character(lfc_ths_pos),i] <- sapply(lfc_ths_pos, \(.x) .d_in_bin[, sum(lfc_max > .x, na.rm = T) / .N], simplify = T)
      plot_array[as.character(lfc_ths_neg),i] <- sapply(lfc_ths_neg, \(.x) .d_in_bin[, sum(lfc_min < .x, na.rm = T) / .N], simplify = T)
    }}
  return(plot_array)
}
