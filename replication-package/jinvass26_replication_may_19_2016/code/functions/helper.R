################################################################################
## helper.R — Core utility functions and package loading
##
## Sourced by every analysis script via config.R or directly.
## Provides: utility functions, package loading, color palettes, ggplot theme.
################################################################################

## ---- Dependencies -----------------------------------------------------------

source("code/functions/getX.R")

## ---- Package Loading --------------------------------------------------------

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, repos = "https://cloud.r-project.org/", dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak(c("rlang", "dplyr", "ggplot2","tidyverse", "reshape2"))
ipak(c("RColorBrewer", "grid"))
ipak(c("miscTools", "lazyeval", "scales", "magrittr", "labeling", "digest", "foreign", "stargazer", "hash"))

## ---- Utility Functions ------------------------------------------------------

## Remove pHYs chunk from PNG files so LaTeX \includegraphics[scale=X] uses
## the default 72 DPI assumption (matching benchmark PNGs without DPI metadata).
strip_png_dpi <- function(path) {
  if (!file.exists(path)) return(invisible(NULL))
  raw <- readBin(path, "raw", file.info(path)$size)
  n <- length(raw)
  sig_len <- 8L
  pos <- sig_len + 1L
  out <- raw[1:sig_len]
  while (pos + 7 <= n) {
    chunk_len <- as.integer(raw[pos]) * 16777216L +
                 as.integer(raw[pos + 1]) * 65536L +
                 as.integer(raw[pos + 2]) * 256L +
                 as.integer(raw[pos + 3])
    chunk_type <- rawToChar(raw[(pos + 4):(pos + 7)])
    total <- 4L + 4L + chunk_len + 4L
    if (pos + total - 1L > n) break
    if (chunk_type != "pHYs") {
      out <- c(out, raw[pos:(pos + total - 1L)])
    }
    pos <- pos + total
  }
  writeBin(out, path)
  invisible(path)
}

winsorize <- function(x, probs = c(0.01, 0.99), na.rm = TRUE) {
  qs <- quantile(x, probs = probs, na.rm = na.rm)
  x[x < qs[1]] <- qs[1]
  x[x > qs[2]] <- qs[2]
  x
}

scale_01 <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

inv.logit <- function(x) 1 / (1 + exp(-x))

which.median = function(x) {
  if (length(x) %% 2 != 0) {
    which(x == median(x))
  } else {
    a = sort(x)[c(length(x)/2, length(x)/2+1)]
    c(which(x == a[1]), which(x == a[2]))
  }
}

colMax <- function(X) apply(X, 2, max)
colMin <- function(X) apply(X, 2, min)

na_to_zero <- function(x) ifelse(is.na(x), 0, x)

as_date_70 <- function(date){
  out = as.Date(date, origin = "1970-01-01")
  return(out)
}

## ---- Plot Configuration -----------------------------------------------------

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
pd <- position_dodge(0.5)

## FTE-style ggplot2 theme
fte_theme <- function() {
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]

  theme_bw(base_size=12) +
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=12,color=color.axis.title)) +
    theme(plot.title=element_text(color=color.title, size=14, hjust=0.5, vjust=1.25)) +
    theme(axis.text.x=element_text(size=12,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=12,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=12,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=12,color=color.axis.title, vjust=1.25)) +
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}
