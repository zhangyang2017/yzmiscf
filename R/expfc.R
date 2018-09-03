#' @title 002. Gene expression fold changes.
#' @description This function allows me to take cleaned PCR Ct values of target genes,
#'     and returns with a fold change relevant to the reference gene for each target gene.
#' @usage expfc(clean_output, control, ref_col)
#' @details expfc uses the 2-ΔΔCT method to calculate fold change for relative gene expression
#'     data obtained from real-time quantitative PCR. It returns a list of three objects: the
#'     ΔCt values for control, the complete fold change table and a quick peek of common
#'     statistics summary mean, median, sd and IQR.
#' @param clean_output A tbl or a df, can be result from cleanme function.
#' @param control The control treatment: character.
#' @param ref_col Column number for reference Ct.
#' @keywords delta delta Ct
#' @references Livak KJ, Schmittgen TD. Analysis of relative gene expression data using
#'     real-time quantitative PCR and the 2- ΔΔCT method. Methods. Elsevier; 2001;25: 402–408.
#' @export


expfc <- function(clean_output, control, ref_col){

    options("digits" = 2, format = "f")
    ## define parameters
    control = c(control)
    ref_col = c(ref_col)
    tar_col = ref_col + 1

    ## calculate ΔCt values for control
    clean_output[tar_col:ncol(clean_output)] <-
        clean_output[tar_col:ncol(clean_output)] - clean_output[,ref_col]

    dct_cont <- filter(clean_output, treatment == control) %>%
        summarize_if(is.numeric, mean, na.rm = TRUE) %>%
        select(-id, -ref_ct)
    dct_cont <- as.list(dct_cont)

    ## calculate ΔΔCt values for the target genes
    clean_output[tar_col:ncol(clean_output)] <- clean_output[tar_col:ncol(clean_output)] - dct_cont
    clean_output[tar_col:ncol(clean_output)] <- 2^(-clean_output[tar_col:ncol(clean_output)])

    clean_output <- select(clean_output, -ref_ct) %>%
        rename_at(vars(ends_with("_ct")), funs(sub("_ct", "_fc", .)))

    ## stat peek
    peek <- select(clean_output, -id) %>%
        group_by(treatment) %>%
        summarise_all(funs(mean, sd, median, IQR)) ## check which IQR type is what i need.

    return(list(dct_cont, clean_output, t(peek)))
}
