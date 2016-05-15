#' Use \code{facet_wrap()} over multiple pages
#'
#' @description Allows \code{ggplot2} panels to be plotted over multiple pages.
#'
#' @param plot a ggplot object
#' @param facets variables to facet by
#' @param nrow number of rows
#' @param ncol number of columns
#' @param scales should scales be fixed ("fixed", the default), free ("free"), 
#' or free in one dimension ("free_x", "free_y")
#'
#' @seealso \code{\link{facet_wrap}}, \code{\link{facet_layout}} or 
#' \code{gridExtra::marrangeGrob}
#' @examples
#' \dontrun{
#' p <- ggplot(diamonds, aes(x = price, y = carat, color = cut)) + 
#' geom_point(alpha = 0.5) + 
#' labs(x = 'Price', y = 'Carat', title = 'Diamonds')
#' 
#' facet_multiple(plot = p, facets = 'color', ncol = 2, nrow = 2)
#' }
#' @import ggplot2
#' @export
#'
facet_multiple <- function(plot = NULL, facets = NULL, ncol = 2, nrow = 2, scales = 'fixed') {
  
  if(is.null(plot)) {   # Check plot argument
    stop('Argument \"plot\" required')
  }
  
  if(is.null(facets)) {   # Check facets argument
    message('Argument \"facets\" not provided. Ploting single panel')
    return(plot)
  }
  
  if(!all(facets %in% colnames(plot$data))) {   # Ensure facets exists
    stop(paste('The facets:', facets, 'could not be found in the data'))
  }
  
  if(is.null(ncol) | is.null(nrow)) {   # Check ncol and nrow arguments
    stop('Arguments \"ncol\" and \"nrow\" required')
  }
  
  # Get info on layout
  n_panel_tot <- nrow(unique(plot$data[, facets, drop = FALSE]))
  n_layout    <- ncol*nrow
  n_pages     <- ceiling(n_panel_tot/n_layout)
  plot        <- plot + facet_wrap(facets = facets, ncol = ncol, scales = scales)
  
  # When no multiple page needed
  if(n_pages == 1) {
    return(plot)
  }
  
  # Extract ggplot2 data and title
  data   <- plot$data
  title  <- plot$labels$title
  
  # Work with the scales
  if(!scales %in% c('free', 'free_x') &&                             # if scale fixed on x
     is.numeric(eval(plot$mapping$x, data)) &&                       # and x is numeric
     length(grep('xmax', plot$scales$scales, fixed = TRUE)) == 0) {  # and x-scale hasn't been defined in ggplot2
    plot$coordinates$limits$x <- range(eval(plot$mapping$x, data))
  }
  
  if(!scales %in% c('free', 'free_y') &&                             # if scale fixed on y
     is.numeric(eval(plot$mapping$y, data)) &&                       # and y is numeric
     length(grep('ymax', plot$scales$scales, fixed = TRUE)) == 0) {  # and y-scale hasn't been defined in ggplot2
    plot$coordinates$limits$y <- range(eval(plot$mapping$y, data))
  }
  
  # Prepare the grouping
  data$groups <- findInterval(unclass(interaction(data[,facets])),
                              seq(from = 1, by = n_layout, length.out = n_pages)[-1])+1
  
  # Plot each page
  for (i in seq_along(1:n_pages)) {
    plot <- plot %+% data[data$groups == i,] + 
      ggtitle(label = bquote(atop(bold(.(title)), atop(italic(Page~.(i)~of~.(n_pages))))))
    
    # For last page call facet_layout
    if(i == n_pages) {
      plot <- facet_layout(plot = plot, facets = facets, ncol = ncol, nrow = nrow, scales = scales)
    }
    
    # Print plots
    if(!is.null(plot)) {
      print(plot)
    }
  } # End for loop
  
} # End facet_multiple