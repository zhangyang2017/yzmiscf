#' @title 001. de-multiplex technical replicates
#' @description This function allows me to take PCR raw Ct values of technical replicates,
#'     and returns with a unique average Ct value for each biological sample.
#' @usage cleanme(.raw_data, ...)
#' @details blank.
#' @param raw_data A tbl, preferably a .csv file with a proper header. Set column 1 as "id",
#'     column 2 as "variable", the rest columns can be raw Ct values for target genes.
#' @keywords data wrangling
#' @export

cleanme <- function(raw_data, clean_output){
   vars <- raw_data %>%
            select(id, variable) %>%
            distinct(id, .keep_all = TRUE)
    clean_output <- raw_data %>%
            group_by(id) %>%
            summarise_if(is.numeric, mean, na.rm = TRUE) %>%
            mutate(treatment = vars$variable) %>%
            select(id, treatment, everything())
  clean_output
}


#' @title 002. Gene expression fold changes.
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
#' @export

expfc <- function(clean.df, control, ref_col){

  ## define parameters
  control = c(control)
  ref_col = c(ref_col)
  tar_col = ref_col + 1

  ## calculate ΔCt values for control
  clean.df <- as.data.frame(clean.df)
  clean.df[tar_col:ncol(clean.df)] <- clean.df[tar_col:ncol(clean.df)] - clean.df[,ref_col]

  dct_cont <- clean.df %>%
    filter(treatment == control) %>%
    summarise_if(is.numeric, mean, na.rm = TRUE) %>%
    select(-id, -ref_ct)
  dct_cont <- as.list(dct_cont)

  ## calculate ΔΔCt values for the target genes
  clean.df[tar_col:ncol(clean.df)] <- clean.df[tar_col:ncol(clean.df)] - dct_cont
  clean.df[tar_col:ncol(clean.df)] <- round(2^(-clean.df[tar_col:ncol(clean.df)]), digits = 2)

  ## stat peek
  peek <- clean.df %>%
    select(-id, -ref_ct) %>%
    group_by(treatment) %>%
    summarise_all(funs(mean, sd, median, IQR)) ## check which IQR type is what i need.

  return(list(dct_cont, clean.df, t(peek)))
}

