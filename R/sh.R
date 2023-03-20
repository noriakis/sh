
#' sh_color
#' @export
sh_color <- function(...) {
  
  sh_colors <- c(
    `h1`     = "#E3B244",
    `h2`   = "#644F47",
    `h3` = "#79A0BD",
    `n1`  =  "#9D8F97",
    `n2` = "#5B5254",
    `m1` = "#B78162",
    `m2`     = "#686488",
    `m3` = "#B3AEA3",
    `r` = "#794049"
  )
  
  cols <- c(...)
  
  if (is.null(cols))
    return (shinsengumi_colors)
  
  sh_colors[cols]
}

#' sh_palettes
#' @export
sh_palettes <- function(palette = "h", ...) {
  
  sh_palettes <- list(
    `h` = sh_color("h1", "h2", "h3","r"),
    `n` = sh_color("n1", "n2", "h3","r"),
    `m` = sh_color("m1", "m2", "m3")
  )
  
  sh_palettes[[palette]]
  
}

#' palette_gen
palette_gen <- function(palette = "h", direction = 1) {
  
  function(n) {
    
    if (n > length(sh_palettes(palette)))
      warning("Not enough colors in this palette!")
    
    else {
      
      all_colors <- sh_palettes(palette)
      
      all_colors <- unname(unlist(all_colors))
      
      all_colors <- if (direction >= 0) all_colors else rev(all_colors)
      
      color_list <- all_colors[1:n]
      
    }
  }
}



#' palette_gen_c
#' @importFrom grDevices colorRampPalette
palette_gen_c <- function(palette = "h", direction = 1, ...) {
  
  pal <- sh_palette(palette)
  
  pal <- if (direction >= 0) pal else rev(pal)
  
  colorRampPalette(pal, ...)
  
}

#' scale_fill_sh
#' @export
scale_fill_sh <- function(palette = "h", direction = 1, ...) {
  
  ggplot2::discrete_scale(
    "fill", "sh",
    palette_gen(palette, direction),
    ...
  )
}

#' @export
scale_colour_sh <- function(palette = "h", direction = 1, ...) {
  
  ggplot2::discrete_scale(
    "colour", "sh",
    palette_gen(palette, direction),
    ...
  )
}

scale_color_sh <- scale_colour_sh

#' @export
scale_color_sh_c <- function(palette = "h", direction = 1, ...) {
  
  pal <- palette_gen_c(palette = palette, direction = direction)
  
  scale_color_gradientn(colors = pal(256), ...)
  
}

