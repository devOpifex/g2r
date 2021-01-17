#' Gauge Aspects
#'
#' Customise aspects of the chart.
#'  
#' @seealso [gauge] to gauge aspects of the grid and axis.
#' 
#' @inheritParams fig_point
#' @param ... Arguments to customise the gauge.
#' Generally, key value pairs of options, a vector of hex colors,
#' or a JavaScript function (wrapped in [htmlwidgets::JS()]).
#' 
#' @examples 
#' # base plot
#' g <- g2(cars, asp(speed, dist)) %>% 
#'  fig_point(asp(color = speed)) 
#' 
#' # color with vector
#' g %>% gauge_color(c("red", "white", "blue"))
#' 
#' # color with callback
#' cb <- "function(speed){
#'  if(speed > 10){
#'    return 'blue';
#'  }
#'  return 'red';
#' }"
#' 
#' g %>% gauge_color(htmlwidgets::JS(cb))
#' 
#' @name gaugeViews
#' @export 
gauge_color <- function(g, ...) UseMethod("gauge_color")

#' @method gauge_color g2r
#' @export 
gauge_color.g2r <- function(g, ...){
  gauge2_(g, ..., fn = "color")
}

#' @method gauge_color g2Proxy
#' @export 
gauge_color.g2Proxy <- function(g, ...){
  gauge2_(g, ..., fn = "color")
}

#' @rdname gaugeViews
#' @export 
gauge_size <- function(g, ...) UseMethod("gauge_size")

#' @method gauge_size g2r
#' @export 
gauge_size.g2r <- function(g, ...){
  gauge2_(g, ..., fn = "size")
}

#' @method gauge_size g2Proxy
#' @export 
gauge_size.g2Proxy <- function(g, ...){
  gauge2_(g, ..., fn = "size")
}

#' @rdname gaugeViews
#' @export 
gauge_shape <- function(g, ...) UseMethod("gauge_shape")

#' @method gauge_shape g2r
#' @export 
gauge_shape.g2r <- function(g, ...){
  gauge2_(g, ..., fn = "shape")
}

#' @method gauge_shape g2Proxy
#' @export 
gauge_shape.g2Proxy <- function(g, ...){
  gauge2_(g, ..., fn = "shape")
}

#' @rdname gaugeViews
#' @export 
gauge_tooltip <- function(g, ...) UseMethod("gauge_tooltip")

#' @method gauge_tooltip g2r
#' @export 
gauge_tooltip.g2r <- function(g, ...){
  gauge2_(g, ..., fn = "tooltip")
}

#' @method gauge_tooltip g2Proxy
#' @export 
gauge_tooltip.g2Proxy <- function(g, ...){
  gauge2_(g, ..., fn = "tooltip")
}

#' @rdname gaugeViews
#' @export 
gauge_label <- function(g, ...) UseMethod("gauge_label")

#' @method gauge_label g2r
#' @export 
gauge_label.g2r <- function(g, ...){
  gauge2_(g, ..., fn = "label")
}

#' @method gauge_label g2Proxy
#' @export 
gauge_label.g2Proxy <- function(g, ...){
  gauge2_(g, ..., fn = "label")
}

#' @rdname gaugeViews
#' @export 
gauge_style <- function(g, ...) UseMethod("gauge_style")

#' @method gauge_style g2r
#' @export 
gauge_style.g2r <- function(g, ...){
  gauge2_(g, ..., fn = "style")
}

#' @method gauge_style g2Proxy
#' @export 
gauge_style.g2Proxy <- function(g, ...){
  gauge2_(g, ..., fn = "style")
}

#' @rdname gaugeViews
#' @export 
gauge_interplay <- function(g, ...) UseMethod("gauge_interplay")

#' @method gauge_interplay g2r
#' @export 
gauge_interplay.g2r <- function(g, ...){
  gauge2_(g, ..., fn = "interaction")
}

#' @method gauge_interplay g2Proxy
#' @export 
gauge_interplay.g2Proxy <- function(g, ...){
  gauge2_(g, ..., fn = "interaction")
}

#' Color Palettes
#' 
#' Convenience function to easily apply colors palettes.
#' 
#' @inheritParams fig_point
#' 
#' @section Palettes:
#' 
#' Palletes from the `viridisLite` package, ideal for:
#' continuous data.
#' 
#' - `gauge_color_viridis` (continuous)
#' - `gauge_color_plasma` (continuous)
#' - `gauge_color_inferno` (continuous)
#' - `gauge_color_magma` (continuous)
#' 
#' Palettes from color brewer:
#' 
#' - `gauge_color_accent` (qualitative)
#' - `gauge_color_dark2` (qualitative)
#' - `gauge_color_paired` (qualitative)
#' - `gauge_color_pastel1` (qualitative)
#' - `gauge_color_pastel2` (qualitative)
#' - `gauge_color_set1` (qualitative)
#' - `gauge_color_set2` (qualitative)
#' - `gauge_color_set3` (qualitative)
#' - `gauge_color_brbg` (diverging)
#' - `gauge_color_piyg` (diverging)
#' - `gauge_color_prgn` (diverging)
#' - `gauge_color_puor` (diverging)
#' - `gauge_color_rdbu` (diverging)
#' - `gauge_color_rdgy` (diverging)
#' - `gauge_color_rdylbu` (diverging)
#' - `gauge_color_rdylgn` (diverging)
#' - `gauge_color_spectral` (diverging) 
#' 
#' Palettes taken from [coolors.co](https://coolors.co):
#' 
#' - `gauge_color_neon` (continuous)
#' - `gauge_color_std` (continuous)
#' - `gauge_color_orange` (continuous)
#' - `gauge_color_pink` (continuous)
#' - `gauge_color_lime` (continuous)
#' 
#' Palettes from `awtools` package:
#' 
#' - `gauge_color_aw` (qualitative)
#' 
#' Custom:
#' 
#' - `gauge_color_g2rc` (continuous)
#' - `gauge_color_g2rq` (qualitative)
#' - `gauge_color_g2rd` (diverging)
#' 
#' @name palettes
#' @export 
gauge_color_viridis <- function(g){
  cols <- c(
    "#440154", 
    "#482878", 
    "#3E4A89", 
    "#31688E", 
    "#26828E", 
    "#1F9E89", 
    "#35B779", 
    "#6DCD59", 
    "#B4DE2C", 
    "#FDE725"
  ) %>% 
    rev()
    
  gauge_color(g, cols)
}

#' @rdname palettes
#' @export 
gauge_color_plasma <- function(g){
  cols <- c(
    "#0D0887", 
    "#47039F", 
    "#7301A8", 
    "#9C179E", 
    "#BD3786", 
    "#D8576B", 
    "#ED7953", 
    "#FA9E3B", 
    "#FDC926", 
    "#F0F921"
  ) %>% 
    rev()

  gauge_color(g, cols)
}

#' @rdname palettes
#' @export 
gauge_color_inferno <- function(g){
  cols <- c(
    "#000004", 
    "#1B0C42", 
    "#4B0C6B", 
    "#781C6D", 
    "#A52C60", 
    "#CF4446", 
    "#ED6925", 
    "#FB9A06", 
    "#F7D03C", 
    "#FCFFA4"
  ) %>% 
    rev()

  gauge_color(g, cols)
}

#' @rdname palettes
#' @export 
gauge_color_magma <- function(g){
  cols <- c(
    "#000004", 
    "#180F3E", 
    "#451077", 
    "#721F81", 
    "#9F2F7F", 
    "#CD4071", 
    "#F1605D", 
    "#FD9567", 
    "#FEC98D", 
    "#FCFDBF"
  ) %>% 
    rev()

  gauge_color(g, cols)
}

#' @rdname palettes
#' @export 
gauge_color_accent <- function(g){
  cols <- c(
    "#7FC97F", 
    "#BEAED4", 
    "#FDC086", 
    "#FFFF99", 
    "#386CB0", 
    "#F0027F", 
    "#BF5B17", 
    "#666666"
  )

  gauge_color(g, cols)
}

#' @rdname palettes
#' @export 
gauge_color_dark2 <- function(g){
  cols <- c(
    "#1B9E77", 
    "#D95F02", 
    "#7570B3", 
    "#E7298A", 
    "#66A61E", 
    "#E6AB02", 
    "#A6761D", 
    "#666666"
  )

  gauge_color(g, cols)
}

#' @rdname palettes
#' @export 
gauge_color_paired <- function(g){
  cols <- c(
    "#A6CEE3", 
    "#1F78B4", 
    "#B2DF8A", 
    "#33A02C", 
    "#FB9A99", 
    "#E31A1C", 
    "#FDBF6F", 
    "#FF7F00", 
    "#CAB2D6", 
    "#6A3D9A", 
    "#FFFF99", 
    "#B15928"
  )

  gauge_color(g, cols)
}

#' @rdname palettes
#' @export 
gauge_color_pastel1 <- function(g){
  cols <- c(
    "#FBB4AE", 
    "#B3CDE3", 
    "#CCEBC5", 
    "#DECBE4", 
    "#FED9A6", 
    "#FFFFCC", 
    "#E5D8BD", 
    "#FDDAEC", 
    "#F2F2F2"
  )

  gauge_color(g, cols)
}

#' @rdname palettes
#' @export 
gauge_color_pastel2 <- function(g){
  cols <- c(
    "#B3E2CD", 
    "#FDCDAC", 
    "#CBD5E8", 
    "#F4CAE4", 
    "#E6F5C9", 
    "#FFF2AE", 
    "#F1E2CC", 
    "#CCCCCC"
  )

  gauge_color(g, cols)
}

#' @rdname palettes
#' @export 
gauge_color_set1 <- function(g){
  cols <- c(
    "#E41A1C", 
    "#377EB8", 
    "#4DAF4A", 
    "#984EA3", 
    "#FF7F00", 
    "#FFFF33", 
    "#A65628", 
    "#F781BF"
  )

  gauge_color(g, cols)
}

#' @rdname palettes
#' @export 
gauge_color_set2 <- function(g){
  cols <- c(
    "#66C2A5", 
    "#FC8D62", 
    "#8DA0CB", 
    "#E78AC3", 
    "#A6D854", 
    "#FFD92F", 
    "#E5C494", 
    "#B3B3B3"
  )

  gauge_color(g, cols)
}

#' @rdname palettes
#' @export 
gauge_color_set3 <- function(g){
  cols <- c(
    "#8DD3C7", 
    "#FFFFB3", 
    "#BEBADA", 
    "#FB8072", 
    "#80B1D3", 
    "#FDB462", 
    "#B3DE69", 
    "#FCCDE5"
  )

  gauge_color(g, cols)
}

#' @rdname palettes
#' @export 
gauge_color_neon <- function(g){
  cols <- c(
    "#7400b8", 
    "#6930c3", 
    "#5e60ce", 
    "#5390d9", 
    "#4ea8de", 
    "#48bfe3", 
    "#56cfe1", 
    "#64dfdf",
    "#72efdd",
    "#80ffdb"
  ) %>% 
    rev()

  gauge_color(g, cols)
}

#' @rdname palettes
#' @export 
gauge_color_std <- function(g){
  cols <- c(
    "#f94144", 
    "#f3722c", 
    "#f8961e", 
    "#f9c74f", 
    "#90be6d", 
    "#43aa8b", 
    "#577590"
  )

  gauge_color(g, cols)
}

#' @rdname palettes
#' @export 
gauge_color_pink <- function(g){
  cols <- c(
    "#ff0a54", 
    "#ff477e", 
    "#ff5c8a", 
    "#ff7096", 
    "#ff85a1", 
    "#ff99ac", 
    "#fbb1bd", 
    "#f9bec7",
    "#f7cad0",
    "#fae0e4"
  ) %>% 
    rev()

  gauge_color(g, cols)
}

#' @rdname palettes
#' @export 
gauge_color_orange <- function(g){
  cols <- c(
    "#ff4800", 
    "#ff5400", 
    "#ff6000", 
    "#ff6d00", 
    "#ff7900", 
    "#ff8500", 
    "#ff9100", 
    "#ff9e00",
    "#ffaa00",
    "#ffb600"
  ) %>% 
    rev()

  gauge_color(g, cols)
}

#' @rdname palettes
#' @export 
gauge_color_lime <- function(g){
  cols <- c(
    "#007f5f", 
    "#2b9348", 
    "#55a630", 
    "#80b918", 
    "#aacc00", 
    "#bfd200", 
    "#d4d700", 
    "#dddf00",
    "#eeef20",
    "#ffff3f"
  ) %>% 
    rev()

  gauge_color(g, cols)
}

#' @rdname palettes
#' @export 
gauge_color_aw <- function(g){
  cols <- c(
    "#F7DC05", 
    "#3d98d3", 
    "#EC0B88", 
    "#5e35b1", 
    "#f9791e", 
    "#3dd378", 
    "#c6c6c6", 
    "#444444"
  )

  gauge_color(g, cols)
}

#' @rdname palettes
#' @export 
gauge_color_g2rq <- function(g){
  cols <- c(
    "#731dd8", 
    "#05a8aa",  
    "#f44708", 
    "#ffae03",
    "#944bbb"
  )

  gauge_color(g, cols)
}

#' @rdname palettes
#' @export 
gauge_color_g2rc <- function(g){
  cols <- c(
    "#3b1577",
    "#4a15a3", 
    "#5a11d0", 
    "#6906ff", 
    "#8e45ff", 
    "#ab6dff",
    "#c492ff",
    "#dab6ff",
    "#eedaff"
  ) %>% 
    rev()

  gauge_color(g, cols)
}

#' @rdname palettes
#' @export 
gauge_color_g2rd <- function(g){
  cols <- c(
    "#432892", 
    "#5d4aa2",  
    "#f5c945", 
    "#efb62a"
  )

  gauge_color(g, cols)
}

#' @rdname palettes
#' @export 
gauge_color_brbg <- function(g){
  cols <- c(
    "#8c510a", 
    "#d8b365",  
    "#f6e8c3", 
    "#c7eae5",
    "#5ab4ac",
    "#01665e"
  )

  gauge_color(g, cols)
}

#' @rdname palettes
#' @export 
gauge_color_piyg <- function(g){
  cols <- c(
    "#c51b7d", 
    "#e9a3c9",  
    "#fde0ef", 
    "#e6f5d0",
    "#a1d76a",
    "#4d9221"
  )

  gauge_color(g, cols)
}

#' @rdname palettes
#' @export 
gauge_color_prgn <- function(g){
  cols <- c(
    "#762a83", 
    "#af8dc3",  
    "#e7d4e8", 
    "#d9f0d3",
    "#7fbf7b",
    "#1b7837"
  )

  gauge_color(g, cols)
}

#' @rdname palettes
#' @export 
gauge_color_puor <- function(g){
  cols <- c(
    "#b35806", 
    "#f1a340",  
    "#fee0b6", 
    "#d8daeb",
    "#998ec3",
    "#542788"
  )

  gauge_color(g, cols)
}

#' @rdname palettes
#' @export 
gauge_color_rdbu <- function(g){
  cols <- c(
    "#b2182b", 
    "#ef8a62",  
    "#fddbc7", 
    "#d1e5f0",
    "#67a9cf",
    "#2166ac"
  )

  gauge_color(g, cols)
}

#' @rdname palettes
#' @export 
gauge_color_rdgy <- function(g){
  cols <- c(
    "#b2182b", 
    "#ef8a62",  
    "#fddbc7", 
    "#e0e0e0",
    "#999999",
    "#4d4d4d"
  )

  gauge_color(g, cols)
}

#' @rdname palettes
#' @export 
gauge_color_rdylbu <- function(g){
  cols <- c(
    "#d73027", 
    "#fc8d59",  
    "#fee090", 
    "#e0f3f8",
    "#91bfdb",
    "#4575b4"
  )

  gauge_color(g, cols)
}

#' @rdname palettes
#' @export 
gauge_color_rdylgn <- function(g){
  cols <- c(
    "#d73027", 
    "#fc8d59",  
    "#fee08b", 
    "#d9ef8b",
    "#91cf60",
    "#1a9850"
  )

  gauge_color(g, cols)
}

#' @rdname palettes
#' @export 
gauge_color_spectral <- function(g){
  cols <- c(
    "#d53e4f", 
    "#fc8d59",  
    "#fee08b", 
    "#e6f598",
    "#99d594",
    "#3288bd"
  )

  gauge_color(g, cols)
}

#' Gauge2
#' 
#' While [gauge_()] applies the `scale` to the
#' global `chart` object, [gauge2_()] applies it to the 
#' `view`.
#' 
#' @inheritParams fig_point
#' @param ... Arguments to customise the gauge.
#' Generally, key value pairs of options, a vector of hex colors,
#' or a JavaScript function (wrapped in [htmlwidgets::JS()]).
#' @param fn Name of the function.
#' 
#' @keywords internal
gauge2_ <- function(g, ..., fn){
  handler <- list(...)

  if(!length(handler))
    stop("Must pass args to `...`", call. = FALSE)

  if(missing(fn))
    stop("Missing `fn`", call. = FALSE)

  for(i in 1:length(g$x$views)){

    if(is.null(g$x$views[[i]][[fn]]))
      next

    if(is.logical(g$x$views[[i]][[fn]]))
      next

    if(length(handler) == 1 && is.null(names(handler)))
      handler <- handler[[1]]

    if(is.logical(handler[[1]]) && is.null(names(handler)))
      g$x$views[[i]][[fn]] <- handler
    else 
      g$x$views[[i]][[fn]] <- list(
        g$x$views[[i]][[fn]], handler
      )
  }
  g
}