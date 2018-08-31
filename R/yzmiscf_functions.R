#' @title A Miscellaneous Function
#' @description This function allows you to take PCR raw Ct values of technical replicates,
#' de-multiplicates each sample, and returns with a unique average Ct value for each biological sample.
#' @param raw_data ##raw Ct values from PCR, duplicates or triplicates
#' @keywords data wrangling
#' @export
#' @examples
#' cleanme()


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




