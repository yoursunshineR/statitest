#' Statistik uji hipotesis dan penduga selang
#'
#' Fungsi digunakan untuk menghitung berbagai penduga interval maupun uji
#' hipotesis secara ciamik menggunakan plot agar lebih mudah dipahami.
#'
#' @param alp Taraf signifikansi
#' @param vek Data sampel numeric
#' @param sig Nilai standar deviasi dari populasi
#' @return Plot penduga interval menggunakan plot

#' @export
interval.rata <- function(alp = 0.05, vek = c(10,10,10,10,10,10,10), sig = NA){
  n <- length(vek)
  xbar <- mean(vek)
  if (n < 30 & is.na(sig)== TRUE){
    sd <- sd(vek)
    v <- n-1
    moe <- abs(qt(alp/2, v)*sd/sqrt(n))
    lci <- xbar - moe
    uci <- xbar + moe

    tplot <- ggplot2::ggplot() +  ggplot2::xlim(-5,5) + ggplot2::ggtitle("Z-test")
    tplot2 <- tplot +  ggplot2::geom_function(fun = dt, args = list(df = v), colour = "red")

    tplot2 + ggplot2::geom_segment(ggplot2::aes(x = qt(alp/2, v), y = 0 ,
                                       yend = 0, xend = qt(1-alp/2, v)),color = "#6CCBDB", size = 3, alpha = 0.9) +
      ggplot2::annotate("point", x = qt(alp/2, v), y = 0, pch = 1, size = 3) +
      ggplot2::annotate("point", x = qt(1-alp/2, v), y = 0, pch = 1, size = 3) +
      ggplot2::annotate("text", x = qt(alp/2, v), y = -0.013, label = paste("t: ",round(qt(alp/2, v),2), " | LCL: ", round(lci, 2))) +
      ggplot2::annotate("text", x = qt(1-alp/2, v), y = -0.013, label = paste("t: ",round(qt(1-alp/2, v),2), " | UCL: ", round(uci,2))) +
      ggplot2::annotate("text", x = -4, y = 0.35, label = paste("MoE: ",round(moe,3))) +
      ggplot2::annotate("text", x = -4, y = 0.30, label = paste("Xbar: ",round(xbar,3)))
  } else {
    if (is.na(sig) == F){
      sd <- sig
    } else {
      sd <- sd(vek)
    }
    moe <- abs(qnorm(alp/2)*sd/sqrt(n))
    lci <- xbar - moe
    uci <- xbar + moe

    zplot <- ggplot2::ggplot() +  ggplot2::xlim(-5,5) + ggplot2::ggtitle("T-test")
    zplot2 <- zplot +  ggplot2::geom_function(fun = dnorm, colour = "red")

    zplot2 + ggplot2::geom_segment(ggplot2::aes(x = qnorm(alp/2), y = 0 ,
                                       yend = 0, xend = qnorm(1-alp/2)),color = "#6CCBDB", size = 3, alpha = 0.9) +
      ggplot2::annotate("point", x = qnorm(alp/2), y = 0, pch = 1, size = 3) +
      ggplot2::annotate("point", x = qnorm(1-alp/2), y = 0, pch = 1, size = 3) +
      ggplot2::annotate("text", x = qnorm(alp/2), y = -0.01, label = paste("z: ",round(qnorm(alp/2),2), " | LCL: ", round(lci, 2))) +
      ggplot2::annotate("text", x = qnorm(1-alp/2), y = -0.01, label = paste("z: ",round(qnorm(1-alp/2),2), " | UCL: ", round(uci,2))) +
      ggplot2::annotate("text", x = -4, y = 0.35, label = paste("MOE: ",round(moe,3))) +
      ggplot2::annotate("text", x = -4, y = 0.30, label = paste("Xbar: ",round(xbar,3)))
  }
}


