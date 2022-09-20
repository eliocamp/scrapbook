ggplot2::theme_set(ggplot2::theme_minimal(base_size = 14) +
            ggplot2::theme(
              legend.position = "bottom", legend.box = "vertical",
              # tagger.panel.tag.background = ggplot2::element_rect(color = NA),
              strip.text = ggplot2::element_text(size =  ggplot2::rel(11/16)),
              legend.box.spacing = grid::unit(-.5, "lines"),
              panel.grid = ggplot2::element_line(color = scales::alpha("gray60", 0.5), size = 0.1),
              panel.ontop = TRUE) )


simplemap <- maps::map("world", fill = TRUE,
                       col = "transparent", plot = FALSE, wrap = c(0, 360))
simplemap <- sf::st_as_sf(simplemap)
simplemap <- rmapshaper::ms_simplify(simplemap, keep = 0.015, weighting = 0.7)

geom_qmap <- function(subset = identity,
                      crop = NULL,
                      color = "gray50", size = 0.3,
                      fill = NA, ...) {
  lon <- lat <- group <- NULL
  data <- simplemap
  
  if (!is.null(crop)) {
    bbox <- sf::st_bbox(data)
    
    for (n in names(crop)) {
      bbox[[n]] <- crop[[n]]
    }
    
    data <- sf::st_crop(data, bbox)
  }
  
  data <- subset(data)
  
  ggplot2::geom_sf(data = data,
                   inherit.aes = FALSE,
                   color = color,
                   size = size,
                   fill = fill,
                   ...)
  
}

no_grid <- theme(panel.grid = element_blank())

geom_coords <- function() {
  
  list(
    no_grid,
    annotate("segment",
             y = seq(-90, 0, by = 15),
             yend = seq(-90, 0, by = 15),
             x = 0,
             xend = 360,
             size = 0.1, alpha = 0.5),
    
    annotate("segment",
             x = seq(0, 360 - 30, by = 30),
             xend =  seq(0, 360 - 30, by = 30),
             y = -90 + 15,
             yend = Inf,
             size = 0.1, alpha = 0.5),
    shadowtext::geom_shadowtext(data = data.frame(x = 0, y = seq(-90 + 15, 0, 
                                                                 by = 15)),
                                aes(x, y, label = LatLabel(y)), size = 1.5, 
                                alpha = 0.7,
                                colour = "black",
                                bg.colour = "white")
  )
}


coord_polar2 <- function(ymax = -20, ...) {
  x <- c(seq(0, 360, length.out = 40), 
         seq(360, 0, length.out = 40), 
         0)
  y <- c(rep(ymax, length.out = 40), 
         rep(60, length.out = 40), 
         ymax)
  
  white <- cbind(x, y) |>  
    list() |> 
    sf::st_polygon() |> 
    sf::st_sfc(crs = "+proj=latlong") 
  
  list(
    geom_sf(data = white, inherit.aes = FALSE, 
            fill = "white", 
            colour = "white", size = 2),
    coord_sf(ylim = c(-90, ymax), 
             lims_method = "box",
             crs = "+proj=laea +lat_0=-90",
             default_crs = "+proj=longlat",
             label_axes =  "----", ...)
  )
}


ReIm <- function(complex) {
  list(Real = Re(complex), Imaginary = Im(complex))
}


sep_ReIm <- function(data, column, format = c("longer", "wider")) {
  R <- part <- I <- NULL
  names <- c("Real", "Imaginary")
  
  
  if (missing(column)) {
    complex <- vapply(data, function(x) inherits(x, "complex"), TRUE)
    if (sum(complex) > 1) {
      stop("`column` missing and more than one complex column found")
    }
    if (sum(complex) == 0) {
      warning("`column` missing and no complex column found. Returning unchanged data")
      return(data)
    }
    
    col <- colnames(data)[complex]
  } else {
    col <- deparse(substitute(column))
  }
  
  
  data <- data.table::copy(data)[, (names) := ReIm(get(col))]
  
  
  if (format[1] == "longer") {
    data[, c(col) := NULL]
    data <- data.table::setDT(tidyr::pivot_longer(data, Real:Imaginary, names_to = "part", values_to = col))
    data[, part := factor(part, levels = names, ordered = TRUE)]
  }
  
  return(data[])
}