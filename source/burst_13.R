library(Rcpp)
library(cairocore)
library(cairobasic)

sourceCpp(here::here("source", "burst_13.cpp"))

burst <- function(seed) {
  layers <- 15

  set.seed(seed)


  sample_shades <- function(n) {
    sample(colours(distinct = TRUE), size = n)
  }

  sample_canva <- function() {
    sample(ggthemes::canva_palettes, 1)[[1]]
  }

  shades <- sample_canva()

  # fixed
  iter <- 5000000
  prefix <- "burst"
  fname <- paste0("burst_13_", seed, ".png")
  fname2 <- paste0("burst_13_", seed, ".jpg")
  transparency <- "ff"
  adjust <- function(x) {x}
  brd <- 0


  scf <- .3
  filter_y <- c(-scf, scf)
  filter_x <- c(-scf, scf)


  bg <- "#000000"

  cat("generating...\n")

  df <- make_burst(iter, layers)
  df <- as.data.frame(df)
  names(df) <- c("x","y","c")
  df <- df[-(1:100),]

  if(!is.null(filter_x)) {
    keep <- df$y > filter_y[1] & df$y < filter_y[2] &
      df$x > filter_x[1] & df$x < filter_x[2]
    df <- df[keep, ]
    df$c[df$c < -1] <- -1
    df$c[df$c > 1] <- 1
  }

  # Manually scale the co-ordinates to the image size
  x_px <- 4000
  y_px <- 4000
  xrng <- max(df[,1]) - min(df[,1])
  yrng <- max(df[,2]) - min(df[,2])
  rng <- max(c(xrng, yrng))

  xdiff <- max(c(yrng - xrng, 0))/2
  ydiff <- max(c(xrng - yrng, 0))/2

  df[,1] <- brd + (df[,1] - min(df[,1]) + xdiff) / rng * (x_px - 2*brd)
  df[,2] <- brd + (df[,2] - min(df[,2]) + ydiff) / rng * (x_px - 2*brd)
  df[,2] <- df[,2] + (y_px - x_px)/2


  # Manually create a vector of colours
  col_idx <- as.integer((df[,3] - min(df[,3])) / (max(df[,3]) - min(df[,3])) * 255) + 1L
  pal <- (colorRampPalette(shades))(n=256)
  pal <- adjust(pal)
  pal <- paste0(pal, transparency)
  #pal <- gsub("FF$", transparency, pal)
  col <- pal[col_idx]

  #fname <- paste0(prefix, seed, "_", layers, "_", scheme, ".png")
  fpath <- here::here("image", fname)
  fpath2 <- here::here("image", fname2)

  cat("rendering...\n")

  cb <- cairobasic::CairoBasic$new(width = x_px, height = y_px, bg = bg, antialias = TRUE)
  cb$add_circles(x=df[,1], y = df[,2], r=1, fill = col, colour = NA)
  cb$write_png(fpath)

  img <- magick::image_read(fpath)
  img <- magick::image_convert(img, "jpeg")
  magick::image_write(img, fpath2)

}

seeds <- 323:340
for(s in seeds) {
  cat("seed", s, "\n")
  burst(s)
}
