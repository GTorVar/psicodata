#' Skewness and kurtosis
#'
#' Returns the simple and multivariate skewness and kurtosis. Results in Spanish
#'
#' @param datos Data frame with data, variables in columns
#' @param na.rm Remove the NA
#' @param sktype The skewness formula: 'fisher' or 'pearson'
#' @param kurtype The kurtosis formula: 'beta2' or 'g2'
#' @param mardia Include the Mardia's skewness and kurtosis (multivariate)?
#' @param plot Plot the multivariate Q-Q plot
#'
#' @return A list with data frames: Forma are the values of variables and Multivariado the values of Mardia's skewness and kurtosis
#'
#' @importFrom stats cov
#'
#' @export
#'
#' @examples
#' x <- data.frame(a=sample(1:10, 100, replace =TRUE),
#'                   b=sample(1:10, 100, replace =TRUE),
#'                   c=sample(1:10, 100, replace =TRUE))
#' est.forma(x)
#'
est.forma <- function(datos, na.rm = TRUE, sktype = 'fisher', kurtype = 'beta2',
                      mardia = FALSE, plot = FALSE){

   if (length(sktype) > 1 | length(kurtype) > 1) warning('Solo un argumento en sktype o kurtype')


   datos <- as.data.frame(datos)

   alpha1 <- apply(datos, 2, function(x) sum(x, na.rm = na.rm)/length(x))
   alpha2 <- apply(datos, 2, function(x) sum(x^2, na.rm = na.rm)/length(x))
   alpha3 <- apply(datos, 2, function(x) sum(x^3, na.rm = na.rm)/length(x))
   alpha4 <- apply(datos, 2, function(x) sum(x^4, na.rm = na.rm)/length(x))
   desvest <- apply(datos, 2, stats::sd)

   if ('fisher' %in% sktype) {
      mu3 <- (alpha3 - 3* alpha1 * alpha2 + 2* (alpha1^3))
      fisherAs <- mu3/(desvest^3)
   }

   if ('pearson' %in% sktype) {
      medias <- apply(datos, 2, mean, na.rm = na.rm)
      modas <- moda(datos)$result
      pearsonAs <- (medias - modas) / desvest
      rownames(pearsonAs) <- NULL
   }

   if ('beta2' %in% kurtype) {
      mu4 <- alpha4 - 4 * alpha1 * alpha3 + 6 * alpha1^2 * alpha2 - 3 * alpha1^4
      beta2 <-mu4/desvest^4
   }

   if ('g2' %in% kurtype) {
      mu4 <- alpha4 - 4 * alpha1 * alpha3 + 6 * alpha1^2 * alpha2 - 3 * alpha1^4
      beta2 <-mu4/desvest^4
      g2 <- beta2 - 3
   }

   forma <- data.frame(Asimetria = if (sktype == 'fisher') fisherAs else t(pearsonAs),
                       Curtosis = if (kurtype == 'beta2') beta2 else g2)
   rownames(forma) <- colnames(datos)
   forma <- round(forma, 3)

   if (isTRUE(mardia) && ncol(datos) >= 2) {
      Asimetria = psych::mardia(datos, na.rm = na.rm, plot = FALSE)$skew
      p.asimetria = psych::mardia(datos, na.rm = na.rm, plot = FALSE)$p.skew
      Curtosis = psych::mardia(datos, na.rm = na.rm, plot = FALSE)$kurtosis
      p.curtosis = psych::mardia(datos, na.rm = na.rm, plot = FALSE)$p.kurt

      mardia <- data.frame(Asimetria, p.asimetria, Curtosis, p.curtosis)

      mardia <- round(mardia, 3)

      resultado <- list(Forma = forma, Multivariado = mardia)
   } else {
      resultado <- list(Forma = forma)
   }

   if (isTRUE(plot) && ncol(datos) >= 2) {
      x <- as.matrix(datos)
      S <- cov(x)
      S.inv <- solve(S)
      D <- x %*% S.inv %*% t(x)
      d = sqrt(diag(D))
      stats::qqnorm(d, main = 'Grafico Q-Q Normal', ylab = 'Cuartiles de la muestra', xlab = 'Cuantiles teoricos')
      stats::qqline(d)
   }

   if (isTRUE(plot) && ncol(datos) <= 2) warning('Q-Q plot necesita al menos dos variables')

   return(resultado)
}
