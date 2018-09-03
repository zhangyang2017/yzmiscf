#' @title 001. de-multiplex technical replicates
#' @description This function allows me to take PCR raw Ct values of technical replicates,
#'     and returns with a unique average Ct value for each biological sample.
#' @usage cleanpcr(raw_data)
#' @details 1st column of the raw data needs to be integer id.
#' @param raw_data A tbl, preferably a .csv file with a proper header. Set column 1 as "id",
#'     column 2 as "variable", the rest columns can be raw Ct values for target genes.
#' @keywords data wrangling
#' @export

cleanpcr <- function(raw_data){

    options(digits = 4)
    clean_output <- group_by_if(raw_data, is.integer) %>%
        summarize_if(is.numeric, mean, na.rm = TRUE) %>%
        mutate(treatment = distinct(liver_raw[1:2])[[2]]) %>%
        select(id, treatment, everything()) # re-order columns

    return(as.data.frame(clean_output))

}
