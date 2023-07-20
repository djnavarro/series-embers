library(Rcpp)
library(cairocore)
library(cairobasic)

sourceCpp(here::here("source", "burst_15.cpp"))

burst <- function(seed) {
  layers <- 20

  set.seed(seed)


  sample_shades <- function(n) {
    sample(colours(distinct = TRUE), size = n)
  }

  sample_canva <- function() {
    sample(ggthemes::canva_palettes, 1)[[1]]
  }

  shades <- sample_canva()

  # fixed
  iter <- 10000000
  prefix <- "burst_15_large_"
  transparency <- "66"
  adjust <- function(x) {x}
  brd <- 0
  scf <- 3
  filter_y <- c(-scf, scf)
  filter_x <- c(-scf, scf)
  radius <- 12

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
  x_px <- 16000 #24000
  y_px <- 16000 #24000
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
  pal <- gsub("FF$", transparency, pal)
  col <- pal[col_idx]


  cat("rendering...\n")


  # create whole thing
  fname <- paste0(prefix, seed, ".png")
  fpath <- here::here("image", fname)
  cat(fname, "\n")
  shrink <- 1
  cb <- cairobasic::CairoBasic$new(width = x_px/shrink, height = y_px/shrink, bg = bg, antialias = TRUE)
  cb$add_circles(x=df$x/shrink, y = df$y/shrink, r = radius/shrink, fill = col, colour = NA)
  cb$write_png(fpath)

  # make a jpg
  fname2 <- paste0(prefix, seed, ".jpg")
  fpath2 <- here::here("image", fname2)
  img <- magick::image_read(fpath)
  img <- magick::image_convert(img, "jpeg")
  magick::image_write(img, fpath2)


  # w <- 4000
  # h <- round(w * sqrt(2))
  #
  # # now make the individual slices
  # for(xoff in 0:5) {
  #   for(yoff in 0:3) {
  #
  #     fname <- paste0(prefix, seed, "_x", xoff, "_y", yoff, ".png")
  #     fpath <- here::here("image", fname)
  #     cat(fname, "\n")
  #
  #     xmin <- xoff * w
  #     xmax <- (xoff + 1) * w
  #     ymin <- yoff * h
  #     ymax <- (yoff + 1) * h
  #
  #     buf <- 10
  #     ind <- df$x > xmin + buf & df$x <= xmax - buf & df$y > ymin + buf & df$y <= ymax - buf
  #     df2 <- df[ind,]
  #
  #     cb <- cairobasic::CairoBasic$new(width = w, height = h, bg = bg, antialias = TRUE)
  #     cb$add_circles(x=(df2$x - xmin), y = (df2$y - ymin), r = radius, fill = col[ind], colour = NA)
  #     cb$write_png(fpath)
  #
  #   }
  # }

}

seeds <- 241
for(s in seeds) {
  cat("seed", s, "\n")
  burst(s)
}
