#' @title 003. plotting.
#' @description This function allows me to take cleaned PCR Ct values of target genes,
#'     and returns with a fold change relevant to the reference gene for each target gene.
#' @usage expfc(.clean.df, control, ref_col)
#' @details expfc uses the 2-ΔΔCT method to calculate fold change for relative gene expression
#'     data obtained from real-time quantitative PCR. It returns a list of three objects: the
#'     ΔCt values for control, the complete fold change table and a quick peek of common
#'     statistics summary mean, median, sd and IQR.
#' @param clean.df A tbl or a df, can be result from cleanme function.
#' @param control The control treatment: character.
#' @param ref_col Column number for reference Ct.
#' @keywords delta delta Ct
#' @references Livak KJ, Schmittgen TD. Analysis of relative gene expression data using real-time quantitative PCR and the 2- ΔΔCT method. Methods. Elsevier; 2001;25: 402–408.


