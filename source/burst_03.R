library(Rcpp)
library(cairocore)
library(cairobasic)

sourceCpp(here::here("source", "ff_l.cpp"))

# parameters
seed <- 19
layers <- 5
scheme <- 40


# fixed
iter <- 10000000
prefix <- "burst"
fname <- "burst_03_large.png"
transparency <- "aa"
adjust <- function(x) {x}
brd <- 0
filter_y <- NULL
filter_x <- NULL

set.seed(seed)

if(scheme == 40) {
  bg <- "#103045"
  pl <- "scico::bamako"
  #brd <- -2500
  filter_y <- c(-1.25, 2.75)
  filter_x <- c(-.5, 3.5)
  adjust <- function(x) {
    x <- adjustcolor(x, 1, 1.2, 1.2, 1.2)
    return(x)
  }
}



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
x_px <- 24000
y_px <- 24000
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
pal <- paletteer::paletteer_c(palette = pl, n = 256)
pal <- adjust(pal)
pal <- gsub("FF$", transparency, pal)
col <- pal[col_idx]

#fname <- paste0(prefix, seed, "_", layers, "_", scheme, ".png")
fpath <- here::here("image", fname)

cat("rendering...\n")

cb <- cairobasic::CairoBasic$new(width = x_px, height = y_px, bg = bg, antialias = TRUE)
cb$add_circles(x=df[,1], y = df[,2], r= 3, fill = col, colour = NA)
cb$write_png(fpath)
