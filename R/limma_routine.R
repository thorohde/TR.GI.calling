#' @export limma_routine

limma_routine <- function(GI_array, .block, only_dup_cor = F, FDR_method = "BH", suppresswarnings = T) {

  if (suppresswarnings) {
    corfit <- suppressWarnings(limma::duplicateCorrelation(GI_array, block = .block)$consensus.correlation)
  } else {
    corfit <- limma::duplicateCorrelation(GI_array, block = .block)$consensus.correlation
  }

  if (only_dup_cor) {return(corfit)}
  if (!only_dup_cor) {
    fit <- limma::lmFit(GI_array, block = .block, correlation = corfit)
    efit <- limma::eBayes(fit)
    return(list(GI = fit$Amean,
                pval = efit$p.value,
                fdr = stats::p.adjust(efit$p.value, method = FDR_method)))}
}
