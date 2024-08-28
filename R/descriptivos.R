#' Descriptives
#'
#' This is [psych::describe()] function translate into Spanish. All the params are equal to original function.
#'
#' @param x A data frame or matrix
#' @param na.rm The default is to delete missing data. na.rm=TRUE will delete the case
#' @param interp Should the median be standard or interpolated
#' @param skew Should the median be standard or interpolated
#' @param ranges Should the range be calculated?
#' @param trim trim=.1 – trim means by dropping the top and bottom trim fraction
#' @param type Which estimate of skew and kurtosis should be used?
#' @param check Should we check for non-numeric variables? Slower but helpful.
#' @param fast If TRUE, will do n, means, sds, min, max, ranges for an improvement in speed. If NULL, will switch to fast mode for large (ncol * nrow > 10^7) problems, otherwise defaults to fast = FALSE
#' @param quant If not NULL, will find the specified quantiles (e.g. quant=c(.25,.75) will find the 25th and 75th percentiles)
#' @param IQR If TRUE, show the interquartile range
#' @param omit Do not convert non-numerical variables to numeric, omit them instead
#' @param data Allows formula input for specific grouping variables
#' @param size For very big problems (in terms of nvar) set size to some reasonable value
#'
#' @return Please, visit the [psych] package, this is only a language adaptation
#'
#' @importFrom psych describe
#'
#' @export
#'
#' @examples
descriptivos <- function(x, na.rm = TRUE, interp=FALSE,skew = TRUE, ranges = TRUE,trim=.1,
                         type=3,check=TRUE,fast=NULL,quant=NULL,IQR=FALSE,omit=FALSE,data=NULL,
                         size=50){
   tabla <- as.data.frame(psych::describe(x, na.rm = na.rm, interp=interp,skew = skew, ranges = ranges,trim=trim,
                                          type=type,check=check,fast=fast,quant=quant,IQR=IQR,omit=omit,data=data,
                                          size=size))
   nombres <- c('Variables', 'N', 'Media', 'Des. Tipica',  'Error std')

   if (!is.null(tabla$range)) {
      nombres <- append(nombres, c('Mediana', 'Min', 'Max', 'Rango'), after = length(nombres)-1)
   } else {
      nombres <- nombres
   }

   if (!is.null(tabla$skew)) {
      nombres <- append(nombres, c('Asimetria', 'Curtosis'), after = length(nombres)-1)
   } else {
      nombres <- nombres
   }

   if (!is.null(tabla$skew) && !is.null(tabla$range)) {
      nombres <- append(nombres, c('M. recotrada', 'MAD'), after = 5)
   } else {
      nombres <- nombres
   }
   colnames(tabla) <- nombres
   return(round(tabla, 2))
}
