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
#' @usage expfc(.clean.df, dct_cont)
#' @details blank.
#' @param cleaned_PCR_Ct_values A tbl or a df, can be result from cleanme function.
#' @keywords delta delta Ct
#' @export

expfc <- function(clean.df, dct_cont){
  clean.df <- as.data.frame(clean.df)
  clean.df[4:ncol(clean.df)] <- clean.df[4:ncol(clean.df)] - clean.df[,3]

  dct_cont <- clean.df %>%
    filter(treatment == "1.HFD") %>%
    summarise_if(is.numeric, mean, na.rm = TRUE) %>%
    select(-id, -ref_ct)
  dct_cont <- as.list(dct_cont)

  clean.df[4:ncol(clean.df)] <- clean.df[4:ncol(clean.df)] - dct_cont[1:8]
  clean.df[4:ncol(clean.df)] <- round(2^(-clean.df[4:ncol(clean.df)]), digits = 2)

  peek <- clean.df %>%
    select(-id, -ref_ct) %>%
    group_by(treatment) %>%
    summarise_all(funs(mean, median, sd))

  return(list(clean, t(peek)))
}

