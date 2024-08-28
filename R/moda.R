#' Mode of a vector or data frame
#'
#' If the input is a data frame, it calcule the mode of each column. If there are many modes, only returns the first.
#'
#' @param x The vetor or data frame
#'
#' @return Returns a list with the result and the warning with the variables with multiple modes or empty otherwise.
#' @export
#'
#' @examples
#' x <- data.frame(A = c(1,1,2,3), B=c(2,2,3,4))
#' moda(x)
moda <- function(x){
   #Sirve para vectores y data.frames
   if (is.vector(x)) {
      completa <- as.data.frame(table(x))
      sin_primera_moda <- as.data.frame(table(x[-(as.numeric(names(which.max(table(x)))))]))
      if (completa$Freq[which.max(completa$Freq)]==sin_primera_moda$Freq[which.max(sin_primera_moda$Freq)]) {
         retorno <- list(result = as.numeric(names(which.max(table(x)))), warning = 'Varias modas')
         return(retorno)
      } else {
         retorno <- list(result = as.numeric(names(which.max(table(x)))), warning = 'Solo una moda')
         return(retorno)
      }
   } else  {
      resultado <- matrix(ncol = ncol(x))
      mensaje <- c()
      for (i in seq_along(colnames(x))) {
         completa <- as.data.frame(table(x[i]))
         sin_primera_moda <- as.data.frame(completa[-which.max(completa$Freq),])
         if (completa$Freq[which.max(completa$Freq)]==sin_primera_moda$Freq[which.max(sin_primera_moda$Freq)]) {
            resultado[i] <- as.numeric(names(which.max(table(x[i]))))
            mensaje[i] <- paste(colnames(x[i]), ' tiene varias modas')
         } else {
            resultado[i] <- as.numeric(names(which.max(table(x[i]))))
            mensaje[i] <- ''
         }
      }
      mensaje <- mensaje[mensaje != '']
      if (length(mensaje) == 0) {
         mensaje <- 'Variables con una moda'
      }
      resultado <- as.data.frame(resultado)
      colnames(resultado) <- colnames(x)
      rownames(resultado) <- 'Moda'
      retorno <- list(result = resultado, warning = mensaje)
      return(retorno)
   }
}
