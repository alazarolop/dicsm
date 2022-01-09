## @knirt mapping_functions ####

# Map scale functions

#' Minimum Legible Area by pixel area and neighborhood
#'
#' @param .ngb 
#' @param .pix 
#'
#' @return
#' @export
#'
#' @examples
ss_mla <- function(.ngb, .pix) {
  .pix <- units::set_units(.pix, "m")

  .ngb^2 * .pix^2
}




#' Average-size area
#'
#' @param .area 
#'
#' @return
#' @export
#'
#' @examples
ss_asa <- function(.area) {
  .area <- units::set_units(.area, "m^2")

  mean(.area)
}




#' Index of Minimum Reduction 
#'
#' @param .mla 
#' @param .asa 
#'
#' @return
#' @export
#'
#' @examples
ss_imr <- function(.mla, .asa) {
  #' All variables are force to be m^2. 
  #' When a unit is give, it's transformed.
  .del <- list(mla = .mla, asa = .asa) %>% 
    map(units::set_units, "m^2") %>% 
    map(units::drop_units)
  
  .imr <- sqrt(.del$asa / .del$mla)
}




#' Scale number giving a MLA by Vink (small)
#' MLD = 0.25 cm^{2}
#' @Rossiter2000MethodologySoilResource
#' 
#' @param .mla 
#'
#' @return
#' @export
#'
#' @examples
ss_sn_vink <- function(.mla) {
  #' Set units in ha (`hm^2` are equivalent and allows for sqrt calc) 
  .mla <- units::set_units(.mla, "ha") %>% 
    units::drop_units()
  
  #' From @Rossiter2000MethodologySoilResource (p. 32)
  sqrt(.mla * 400) * 1000
}





#' Scale number giving a MLA by Cornell (large)
#' MLD = 0.4 cm^{2}
#' @Rossiter2000MethodologySoilResource 
#'
#' @param .mla 
#'
#' @return
#' @export
#'
#' @examples
ss_sn_cornell <- function(.mla) {
  #' Set units in ha (`hm^2` are equivalent and allows for sqrt calc) 
  .mla <- units::set_units(.mla, "ha") %>% 
    units::drop_units()
  
  #' From @Rossiter2000MethodologySoilResource (p. 32)
  sqrt(.mla * 250) * 1000
}



#' Accuracy metrics for Confusion Matrix
#'
#' @param CM # ¿cómo es la matriz?
#'
#' @return
#' @export
#'
#' @examples
accucat <- function(CM) {
  # convert both data frames and vectors to matrices 
  cmx <- as.matrix(CM) 
  # try to convert a vector to a square matrix 
  if (ncol(cmx) == 1) {
    cmx <- matrix(cmx, byrow = TRUE, nrow = sqrt(nrow(cmx))) 
  } 

  nr <- nrow(cmx)
  nc <- ncol(cmx)
  if (nr != nc) { 
    print("Error: matrix is not square")
    break 
  }
  
  n <-sum (cmx) 
  d <- diag(cmx) 
  dsum <- sum(d)
  th1 <- dsum/n 
  th1v <- ((th1 * (1-th1))/n) 

  csum <- apply(cmx,2,sum) 
  rsum <- apply(cmx,1,sum) 
  ua <- d/rsum 
  pa <- d/csum 
  th2 <- sum(rsum * csum) / n^2

  kh <- (th1-th2)/(1-th2)

  th3 <- sum((csum + rsum) * d) / n^2

  th4 <- 0 
  for (i in 1:nr) {
    for (j in 1:nc) {
      th4 <- th4 + (cmx[i,j] * ((csum[i] + rsum[j])^2))
    } 
  } 
  th4 <- th4 / n^3

  th1c <- 1 - th1 
  th2c <- 1 - th2

  khv <- 1/n * (  ((th1 * th1c) / th2c^2) + 
                  ((2 * th1c * ((2 * th1 * th2) - th3)) / th2c^3) + 
                  ((th1c^2 * (th4 - (4 * th2^2)) / th2c^4)) )

  #per-class kappa, user’s accuracy...
  p <- cmx / n
  uap <- apply(p, 1, sum)
  pap <- apply(p, 2, sum) 
  dp <- diag(p) 
  kpu <- (dp/uap - pap) / (1 - pap) 
  #...and its variance 
  t1 <- uap - dp
  t2 <- (pap * uap) - dp
  t3 <- dp * (1 - uap - pap + dp) 
  kpuv <- ((t1/(uap^3 * (1-pap)^3)) * ((t1 * t2) + t3))/n 

  #per-class kappa, producer’s reliability...
  kpp <- (dp / pap - uap) / (1 - uap) 
  #...and its variance 
  t1 <- (pap - dp) 
  kppv <- ((t1 / (pap^3 * (1-uap)^3)) * ((t1 * t2) + t3)) / n

  #return all statistics as a list
  list( sum.n = n, 
        sum.naive = th1,
        sum.var = th1v, 
        sum.kappa = kh, 
        sum.kvar = khv,
        user.naive = ua, 
        prod.naive = pa,
        user.kappa = kpu, 
        user.kvar = kpuv, 
        prod.kappa = kpp, 
        prod.kvar = kppv)
}
# sum(z$user.naive) / nrow(x)
# sum(z$prod.naive) / ncol(x)