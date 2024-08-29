#' Skewness and kurtosis
#'
#' Returns the simple and multivariate skewness and kurtosis. Results in Spanish
#'
#' @param datos Data frame with data, variables in columns
#' @param na.rm Remove the NA
#' @param sktype The skewness formula: 'fisher' and 'pearson'
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
est.forma <- function(datos, na.rm = TRUE, sktype = 'fisher', mardia = TRUE,
                      plot = FALSE){

   desvest <- matrix(data = NA, ncol = ncol(datos))

   if ('fisher' %in% sktype) {
      alpha1 <- matrix(data = NA, ncol = ncol(datos))
      alpha2 <- matrix(data = NA, ncol = ncol(datos))
      alpha3 <- matrix(data = NA, ncol = ncol(datos))
      mu3 <- matrix(data = NA, ncol = ncol(datos))
      fisherAs <- matrix(data = NA, ncol = ncol(datos))
      for (i in seq_along(colnames(datos))) {
         alpha1[i] <- sum(datos[[i]], na.rm = na.rm)/length(datos[[i]])
         alpha2[i] <- sum(datos[[i]]^2, na.rm = na.rm)/length(datos[[i]])
         alpha3[i] <- sum(datos[[i]]^3, na.rm = na.rm)/length(datos[[i]])
         mu3[i] <- (alpha3[i] - 3* alpha1[i] * alpha2[i] + 2* (alpha1[i]^3))
         desvest[i] <- sqrt((sum(datos[[i]]^2, na.rm = na.rm)-length(datos[[i]]) * mean(datos[[i]], na.rm = na.rm)^2)/(length(datos[[i]])-1))
         fisherAs[i] <- mu3[[i]]/(desvest[[i]]^3)
      }
      fisherAs <- as.data.frame(fisherAs)
      colnames(fisherAs) <- colnames(datos)
      rownames(fisherAs) <- 'Asim. Fisher'
   }

   if ('pearson' %in% sktype) {
      pearsonas <- matrix(data = NA, ncol = ncol(datos))
      for (i in seq_along(colnames(datos))) {
         desvest[i] <- sqrt((sum(datos[[i]]^2, na.rm = na.rm)-length(datos[[i]]) * mean(datos[[i]], na.rm = na.rm)^2)/(length(datos[[i]])-1))
         pearsonas[i] <- (mean(datos[[i]], na.rm = na.rm)-moda(x[[i]])$result)/desvest[[i]]
      }
      pearsonas <- as.data.frame(pearsonas)
      colnames(pearsonas) <- colnames(datos)
      rownames(pearsonas) <- 'Asim. Pearson'
   }

   forma <- data.frame(Asimetria = rep(0, ncol(datos)),
                       Curtosis = rep(0, ncol(datos)))

   for (i in seq_along(colnames(datos))) {
      forma$Asimetria[i] <- if (sktype == 'fisher') fisherAs[[i]] else pearsonas[[i]]
      forma$Curtosis[i] <- psych::kurtosi(datos[[i]], na.rm = na.rm, type = 2)
   }

   forma <- round(forma, 3)

   if (isTRUE(mardia)) {
      Asimetria = psych::mardia(datos, na.rm = na.rm, plot = FALSE)$skew
      p.asimetria = psych::mardia(datos, na.rm = na.rm, plot = FALSE)$p.skew
      Curtosis = psych::mardia(datos, na.rm = na.rm, plot = FALSE)$kurtosis
      p.curtosis = psych::mardia(datos, na.rm = na.rm, plot = FALSE)$p.kurt

      mardia <- data.frame(Asimetria, p.asimetria, Curtosis, p.curtosis)

      mardia <- round(mardia, 3)

      resultado <- list(Forma = forma, Multivariado = mardia)
   } else {
      resultado <- forma
   }

   if (isTRUE(plot)) {
      x <- as.matrix(datos)
      S <- cov(x)
      S.inv <- solve(S)
      D <- x %*% S.inv %*% t(x)
      d = sqrt(diag(D))
      stats::qqnorm(d, main = 'Grafico Q-Q Normal', ylab = 'Cuartiles de la muestra', xlab = 'Cuantiles teoricos')
      stats::qqline(d)
   }

   return(list(resultado))
}
