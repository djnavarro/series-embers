library(Rcpp)
library(cairocore)
library(cairobasic)

sourceCpp(here::here("source", "ff_l.cpp"))

burst <- function(seed) {
  layers <- 10

  sample_shades <- function(n) {
    sample(colours(distinct = TRUE), size = n)
  }

  sample_canva <- function() {
    sample(ggthemes::canva_palettes, 1)[[1]]
  }

  # fixed
  iter <- 10000000
  prefix <- "burst"
  fname <- paste0("burst_06_", seed, ".png")
  transparency <- "ff"
  adjust <- function(x) {x}
  brd <- 0
  filter_y <- NULL
  filter_x <- NULL

  set.seed(seed)

  bg <- "#000000"
  #filter_y <- c(-3, 3)
  #filter_x <- c(-3, 3)


  cat("generating...\n")


  df <- flame(iter, layers)
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
  pal <- (colorRampPalette(sample_canva()))(n=256)
  pal <- adjust(pal)
  pal <- gsub("FF$", transparency, pal)
  col <- pal[col_idx]

  #fname <- paste0(prefix, seed, "_", layers, "_", scheme, ".png")
  fpath <- here::here("image", fname)

  cat("rendering...\n")

  cb <- cairobasic::CairoBasic$new(width = x_px, height = y_px, bg = bg, antialias = TRUE)
  cb$add_circles(x=df[,1], y = df[,2], r= 3, fill = col, colour = NA)
  cb$write_png(fpath)

}

seeds <- 175:200
for(s in seeds) {
  cat("seed", s, "\n")
  burst(s)
}
