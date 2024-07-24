#' @import data.table
#' @import ggplot2
#'
#' @export sequencing_coverage_plot

## ---- sequencing_coverage_plot function ----

sequencing_coverage_plot <- \(read_count_dt,
                              columns,
                              info_file) {

  #print(str(read_count_dt))

  #print(columns)


  x <- list(data = data.table::data.table(screen = columns),
            n_constructs = base::nrow(read_count_dt))

  if (!base::missing(info_file)) {
    x$data <- merge(x$data, info_file, by = "screen", all.x = T)
  }

  x$data$abs_read_count <- base::sapply(columns,
                                        \(.s) {read_count_dt[, sum(get(.s))]})

  x$plot <- ggplot2::ggplot(
    data = x$data,
    mapping = ggplot2::aes(x$data$abs_read_count, x$data$screen)) +
    ggplot2::labs(x = "Reads per sample",
                  y = "Screen",
                  title = "Absolute read counts") +
    ggplot2::theme(legend.position = "bottom")


  x
}

