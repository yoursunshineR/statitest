#' Statistik uji hipotesis dan penduga selang
#'
#' Fungsi digunakan untuk menghitung berbagai penduga interval maupun uji
#' hipotesis secara ciamik menggunakan plot agar lebih mudah dipahami.
#'
#' @param p0 Nilai selisih proporsi dua sampel
#' @param alp Taraf signifikansi
#' @param h1 Jenis uji: two.sided ; right.sided ; left.sided
#' @param x1 Banyak karakteristik pada sampel pertama
#' @param n1 Banyak sampel pertama
#' @param x2 Banyak karakteristik pada sampel kedua
#' @param n2 Banyak sampel kedua
#' @return Plot uji proporsi dengan

#' @export
proportion_test<- function(p0 = 0, alp = 0.05, h1 = "two.sided [default]; right.sided; left.sided", x1 = NA, n1 = NA, x2 = NA, n2 = NA){
  p1 <- x1/n1
  p2 <- x2/n2
  if (p0 == 0){
    pbar <- (x1 + x2)/(n1 + n2)
    sd.p <- sqrt(pbar*(1-pbar)*(1/n1 + 1/n2))
  } else {
    sd.p <- sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
  }
  z <- (p1 - p2 - p0)/sd.p

  zplot <- ggplot2::ggplot() +  ggplot2::xlim(-5,5) + ggplot2::ggtitle("Z-tes proporsi") + ggplot2::geom_function(fun = dnorm, colour = "red")

  if(h1 == "right.sided"){
    plot.z <- zplot + ggplot2::geom_segment(ggplot2::aes(x = qnorm(alp, lower.tail = F), y = 0 ,
                                                yend = 0, xend = 5),color = "#6CCBDB", size = 3, alpha = 0.8) +
      ggplot2::geom_text(ggplot2::aes(x = qnorm(alp, lower.tail = F), y = -0.01, label = round(qnorm(alp, lower.tail = F),4))) +
      ggplot2::geom_point(ggplot2::aes(x = qnorm(alp, lower.tail = F), y = 0))
    result <- ifelse(pnorm(z) > 1 - alp, "Tolak H0", "Gagal Tolak H0")
  }
  else if(h1 == "left.sided"){
    plot.z <- zplot + ggplot2::geom_segment(ggplot2::aes(x = qnorm(alp), y = 0 ,
                                                yend = 0, xend = -5),color = "#6CCBDB", size = 3, alpha = 0.8) +
      ggplot2::geom_text(ggplot2::aes(x = qnorm(alp), y = -0.01, label = round(qnorm(alp),4))) +
      ggplot2::geom_point(ggplot2::aes(x = qnorm(alp), y = 0))
    result <- ifelse(pnorm(z) < alp, "Tolak H0", "Gagal Tolak H0")
  } else {
    plo <- zplot + ggplot2::geom_segment(ggplot2::aes(x = qnorm(alp/2), y = 0 ,
                                             yend = 0, xend = -5),color = "#6CCBDB", size = 3, alpha = 0.8) +
      ggplot2::geom_text(aes(x = qnorm(alp/2), y = -0.01, label = round(qnorm(alp/2),4))) +
      ggplot2::geom_point(aes(x = qnorm(alp/2), y = 0))
    plot.z <- plo + ggplot2::geom_segment(ggplot2::aes(x = qnorm(alp/2, lower.tail = F), y = 0 ,
                                              yend = 0, xend = 5),color = "#6CCBDB", size = 3, alpha = 0.8) +
      ggplot2::geom_text(ggplot2::aes(x = qnorm(alp/2, lower.tail = F), y = -0.01, label = round(qnorm(alp/2, lower.tail = F),4))) +
      ggplot2::geom_point(ggplot2::aes(x = qnorm(alp/2, lower.tail = F), y = 0))
    result <- ifelse(pnorm(z) < alp/2 | pnorm(z) > 1-alp/2, "Tolak H0", "Gagal Tolak H0")
  }
  plot.z + ggplot2::geom_point(ggplot2::aes(x=z , y = 0), colour = "salmon", fill = "red" ) +
    ggplot2::geom_text(ggplot2::aes(x =  z, y = 0.02, label = paste("Z-hit : ",round(z,4), "p-val:", round(pnorm(z),4)))) +
    ggplot2::geom_label(ggplot2::aes(x =  -3.3, y = 0.36, label = paste("Keputusan: ", result)), fill = "aquamarine")
}
