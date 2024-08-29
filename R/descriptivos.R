#' Descriptivos
#'
#' Devuelve los estadísticos descriptivos habituales.
#'
#' @param x Conjunto de datos
#' @param na.rm Omitir valores perdidos
#' @param trim Valor de la media recortada
#'
#' @return Una tabla con los valores
#'
#' @importFrom stats median
#'
#' @export
#'
#' @examples
#' x <- data.frame(a=sample(1:10, 100, replace =TRUE),
#'                   b=sample(1:10, 100, replace =TRUE),
#'                   c=sample(1:10, 100, replace =TRUE))
#' descriptivos(x)

descriptivos <- function(x, na.rm = TRUE, trim = .1){

   Minimo = apply(x, 2, min)
   Maximo = apply(x, 2, max)
   Desv.Tipica = apply(x, 2, function(x) sqrt((sum(x^2, na.rm = na.rm)-length(x)*mean(x, na.rm = na.rm)^2)/(length(x)-1)))

   tabla <- data.frame(N = apply(x, 2, length),
      Perdidos = apply(x, 2, function(x) sum(is.na(x))),
      Media = apply(x, 2, mean, na.rm = na.rm),
      Media.recortada = apply(x, 2, mean, na.rm = na.rm, trim = trim),
      Mediana = apply(x, 2, median, na.rm = na.rm),
      Moda = apply(x, 2, function(x) moda(x)$result),
      Minimo = Minimo,
      Maximo = Maximo,
      Rango = Maximo - Minimo,
      Desv.Tipica = Desv.Tipica,
      Error.Estandar = Desv.Tipica/(length(x)-1) )

   tabla <- round(tabla, 2)

   return(tabla)
}
