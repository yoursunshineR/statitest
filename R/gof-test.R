#' Goodness of fit test
#'
#' Fungsi digunakan untuk menentukan apakah suatu data observasi memiliki
#' kecocokan dengan suatu distribusi.
#'
#' @param kiri.first Batas bawah kelompok pertama
#' @param kiri.last Batas bawah kelompok terakhir
#' @param kanan.first Batas atas kelompok pertama
#' @param kanan.last Batas atas kelompok terakhir
#' @param k Banyak kelompok kelas
#' @param f.obs Frekuensi observasi per kelas
#' @param var Varians populasi
#' @param miu Rata-rata populasi
#' @param par Banyaknya parameter dalam distribusi yang akan dicocokkan
#' @param alp Taraf signifikansi yang digunakan
#' @return Menentukan kecocokan distribusi suatu data disertai perhitungan

#' @export



gof.test <- function(kiri.first, kiri.last, kanan.first, kanan.last, k, f.obs, var, miu, par, alp) {
  # Generate kelompok interval
  b.bawah <- c(seq(kiri.first, kiri.last, k))
  b.atas <- c(seq(kanan.first, kanan.last, k))


  # Generate Z-value
  z.bawah <- function(b,var,miu) {
    z <- (b - 0.5 - miu)/sqrt(var)
    return(z)
  }
  z.atas <- function(b,var,miu) {
    z <- (b + 0.5 - miu)/sqrt(var)
    return(z)
  }
  z.bawah <- sapply(b.bawah, z.bawah, var = var, miu = miu )
  z.atas <- sapply(b.atas, z.atas, var = var, miu = miu)


  # Create p vector
  p.a <- pnorm(z.atas[1])
  p.b <- pnorm(z.bawah[length(b.bawah)], lower.tail = F)
  p <- sapply(z.atas, pnorm) - sapply(z.bawah, pnorm)
  p[1] <- p.a
  p[length(z.bawah)] <- p.b


  # Find ei
  ei <- p*sum(f.obs)


  # Find khi-square
  temp <- (f.obs - ei)^2
  khi.dat <- temp/ei
  khi <- sum(khi.dat)

  khi.tabel <- qchisq(alp, df = k - 1 - par, lower.tail = F)

  cat("\n                 Goodness of Fit Test by yoursunshine \n \n")
  cat(paste("Khisq-hitung:  ", round(khi,4)))
  cat(paste("\nKhisq-tabel :  ", round(khi.tabel,4)))
  cat(paste("\nDf :  ", k - 1 - par))
  cat(paste("\nAlpha :  ", alp))
  if (khi < khi.tabel) {
    cat(paste("\n\n\nKhisq-hitung < Khisq-tabel"))
    cat("\nKeputusan: H0 tidak ditolak")
    cat("\nObservasi mengikuti sebaran terkait")
  } else if (khi > khi.tabel) {
    cat(paste("\n\n\nKhisq-hitung > Khisq-tabel"))
    cat("\nKeputusan: H0 ditolak")
    cat("\nObservasi tidak mengikuti sebaran terkait")
  }
  cat("\n \n-----------------------  Hasil Perhitungan  ------------------------ \n \n")

  df <- data.frame("Batas-Bawah" = b.bawah, "Batas-Atas" = b.atas, "Frek-Observ" = f.obs, "Peluang-Z" = round(p,4), "Frek-Harap" = round(ei,4), "Khisq" = round(khi.dat,4))
  df
}
