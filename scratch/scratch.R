big_x <- function(df, threshold) {
  # Write a check for x not being in df
  if (!"x" %in% names(df)) { 
    stop("df must contain variable called x", call. = FALSE)
  }
  
  # Write a check for threshold being in df
  if ("threshold" %in% names(df)) {
    stop("df must not contain variable called threshold", call. = FALSE)
  }
  
  dplyr::filter(df, x > threshold)
}


getOption("digits")
options(digits = 2)

stop() and stopifnot()



linkedin <- c(16, 9, 13, 5, 2, 17, 14)
facebook <- c(17, 7, 5, 16, 8, 13, 14)
views <- matrix(c(linkedin, facebook), nrow = 2, byrow = TRUE)



head(select(df, city:dptp))

df <- mutate(df, variable = factor(1 * (variable2 > 80), labels = c("cold", "hot")))

df <- mutate(df, year = as.POSIXlt(date)$year + 1900)

transmute(): drop unrelated columns

qq <- quantile(chicago$pm25, seq(0, 1, 0.2), na.rm = TRUE)


f <- function(num) {
	for(i in seq_len(num)) {
		cat("Hello, world!\n")
		}


for (i in seq_along(x)){
	print(x[i])
}



lapply always returns a list, regardless of the class of the input


add 95 CI to my functions.

R (>= 3.1.0)

Imports: 
    dplyr (>= 0.7.4),
    ggplot2,
    purrr

The easiest way to add Imports and Suggests to your package is to use 
devtools::use_package(). This automatically puts them in the right place in 
your DESCRIPTION, and reminds you how to use them.

devtools::use_package("dplyr") 
# Defaults to imports #> Adding dplyr to Imports #> Refer to functions 
# with dplyr::fun() 
devtools::use_package("dplyr", "Suggests")
#> Adding dplyr to Suggests 
#> Use requireNamespace("dplyr", quietly = TRUE) 
# to test if package is #> installed, then use dplyr::fun() to refer to functions.