#' Uji Varians 1 atau 2 Populasi
#'
#' Fungsi digunakan untuk menguji varians baik dari satu ataupun dua populasi
#'
#'
#' @param varsampel varians dari sampel (untuk 1 populasi langsung input nilai, untuk 2 populasi gunakan syntax c(), contoh: c(varsampel1, varsampel2))
#' @param nsampel jumlah sampel (untuk 1 populasi langsung input nilai, untuk 2 populasi gunakan syntax c(), contoh: c(nsampel1, nsampel2))
#' @param varpop0 input varians populasi uji untuk uji 1 sampel, apabila pada parameter termuat, maka akan menghasilkan uji varians 1 populasi
#' @param h1 hipotesis alternatif (HA/H1), pilih = "two.sided", "right.sided", atau "left.sided". default = "two.sided"
#' @param alpha taraf signifikansi yang diinginkan
#' @return Uji varians

#' @export


variance.test <- function( varsampel = c(NA,NA), nsampel = c(NA,NA),
                          varpop0 = NA, h1 = "two.sided",
                          alpha = 0.05){
  if (!is.na(varpop0) & is.na(varsampel[2]) & is.na(nsampel[2])) {
    khi <- (nsampel[1]-1)*varsampel[1]/varpop0
    v <- nsampel[1] - 1
    p.val <- pchisq(khi, df = v, lower.tail = F)

    cat("\n\n                     Chi-square One Variance Test by yoursunshine \n",
        "\n\n",
        "Chi-square hitung  : ", round(khi,4), "\n",
        "Degree of freedom  : ", v, "\n",
        "Alpha              : ", alpha, "\n",
        "p-value (P[H0])    : ", round(p.val,4), "\n\n")

    #two.sided
    if (h1 == "two.sided") {
      khi.tab.bwh <- qchisq(alpha/2, df = v)
      khi.tab.ats <- qchisq(alpha/2, df = v, lower.tail = F)
      cat(" Bottom critical    : ", round(khi.tab.bwh,4), "\n",
          "Upper critical     : ", round(khi.tab.ats,4), "\n")
      if (khi < khi.tab.bwh | khi > khi.tab.ats) {
        cat(" Chi-square hitung tidak di antara dua critical \n\n")
        cat(" Keputusan          : Tolak H0 \n",
            "Dengan tingkat kepercayaan ", (1-alpha)*100, "%, \n belum cukup bukti bahwa varians populasi sama dengan ", varpop0)
      } else {
        cat(" Chi-square hitung ada di antara dua critical \n\n")
        cat(" Keputusan          : Gagal tolak H0 \n",
            "Dengan tingkat kepercayaan ", (1-alpha)*100, "%,\n cukup bukti bahwa varians populasi sama dengan ", varpop0)
      }
    } else if (h1 == "right.sided") {
      khi.tab <- qchisq(alpha, df = v, lower.tail = F)
      cat(" Critical right.sided : ", round(khi.tab,4), "\n")
      if (khi > khi.tab) {
        cat(" Chi-square hitung > Chi-square tabel \n\n")
        cat(" Keputusan          : Tolak H0 \n",
            "Dengan tingkat kepercayaan ", (1-alpha)*100, "%, \n belum cukup bukti bahwa varians populasi lebih dari ", varpop0)
      } else {
        cat(" Chi-square hitung < Chi-square tabel \n\n")
        cat(" Keputusan          : Gagal tolak H0 \n",
            "Dengan tingkat kepercayaan ", (1-alpha)*100, "%,\n cukup bukti bahwa varians populasi lebih dari ", varpop0)
      }
    } else if (h1 == "left.sided") {
      khi.tab <- qchisq(alpha, df = v)
      cat(" Critical left.sided : ", round(khi.tab,4), "\n")
      if (khi < khi.tab) {
        cat(" Chi-square hitung < Chi-square tabel \n\n")
        cat(" Keputusan          : Tolak H0 \n",
            "Dengan tingkat kepercayaan ", (1-alpha)*100, "%,\n belum cukup bukti bahwa varians populasi kurang dari ", varpop0)
      } else {
        cat(" Chi-square hitung > Chi-square tabel \n\n")
        result <- "Gagal tolak H0"
        cat(" Keputusan          : Gagal tolak H0 \n",
            "Dengan tingkat kepercayaan ", (1-alpha)*100, "%,\n cukup bukti bahwa varians populasi kurang dari ", varpop0)
      }
    } else cat("\n Masukkan nilai h1 yang benar")

  } else {
    s1 <- varsampel[1]
    s2 <- varsampel[2]
    v1 <- nsampel[1] - 1
    v2 <- nsampel[2] - 1
    f <- s1/s2

    cat("\n\n                     Fisher Two Variances Test by yoursunshine \n",
        "\n\n",
        "F hitung           : ", round(f,4), "\n",
        "Degree of freedom 1: ", v1, "\n",
        "Degree of freedom 2: ", v2, "\n",
        "Alpha              : ", alpha, "\n\n")

    #two.sided
    if (h1 == "two.sided") {
      f.tab.bwh <- qf(1 - alpha/2, df1 = v1, df2 = v2, lower.tail = F)
      f.tab.ats <- qf(alpha/2, df1 = v1, df2 = v2, lower.tail = F)
      cat(" Bottom critical    : ", round(f.tab.bwh,4), "\n",
          "Upper critical     : ", round(f.tab.ats,4), "\n")
      if (f < f.tab.bwh | f > f.tab.ats) {
        cat(" F hitung hitung tidak di antara dua critical \n\n")
        cat(" Keputusan          : Tolak H0 \n",
            "Dengan tingkat kepercayaan ", (1-alpha)*100, "%,\n
            belum cukup bukti bahwa var.pop.1 = var.pop.2")
      } else {
        cat(" F hitung ada di antara dua critical \n\n")
        cat(" Keputusan          : Gagal tolak H0 \n",
            "Dengan tingkat kepercayaan ", (1-alpha)*100, "%,\n cukup bukti bahwa var.pop.1 = var.pop.2")
      }
    } else if (h1 == "right.sided") {
      f.tab <- qf(alpha, df1 = v1, df2 = v2, lower.tail = F)
      cat(" Critical right.sided : ", round(f.tab,4), "\n")
      if (f > f.tab) {
        cat(" F hitung > F tabel \n\n")
        cat(" Keputusan      : Tolak H0 \n",
            "Dengan tingkat kepercayaan ", (1-alpha)*100, "%,\n belum cukup bukti bahwa var.pop.1 <= var.pop.2")
      } else {
        cat(" F hitung < Ftabel \n\n")
        cat(" Keputusan          : Gagal tolak H0 \n",
            "Dengan tingkat kepercayaan ", (1-alpha)*100, "%,\n cukup bukti bahwa var.pop.1 <= var.pop.2")
      }
    } else if (h1 == "left.sided") {
      f.tab <- qf(1 - alpha, df1 = v1, df2 = v2, lower.tail = F)
      cat(" Critical left.sided : ", round(f.tab,4), "\n")
      if (f < f.tab) {
        cat(" F hitung < F tabel \n\n")
        cat(" Keputusan          : Tolak H0 \n",
            " Dengan tingkat kepercayaan ", (1-alpha)*100, "%,\n belum cukup bukti bahwa var.pop.1 >= var.pop.2")
      } else {
        cat(" F hitung > F tabel \n\n")
        cat(" Keputusan          : Gagal tolak H0 \n",
            "Dengan tingkat kepercayaan ", (1-alpha)*100, "%,\n cukup bukti bahwa var.pop.1 >= var.pop.2")
      }
    } else
      cat("\nMasukkan nilai h1 yang benar")
  }
}


