# Specification of colors accorind to Corporate Design Guidelines as of 02.07.2019
# Note: This Script is hevily inspired by the BBC Style Guide

zi_colors <- c(
  `zidarkred`       =  rgb(156/255,5/255,136/255),
  `zired`           =  rgb(200/255,0/255,177/255),
  `zilightred`      =  rgb(244/255,244/255,239/255),
  `zidarkblue`       =  rgb(0/255,101/255,150/255),
  `ziblue`           =  rgb(0/255,134/255,197/255),
  `zilightblue`      =  rgb(204/255,231/255,243/255),
  `zidarkgreen`      =  rgb(136/255,156/255,5/255),
  `zigreen`          =  rgb(177/255,200/255,0/255),
  `zilightgreen`     =  rgb(239/255,244/255,204/255),
  `zidarkorange`     =  rgb(175/255,119/255,3/255),
  `ziorange`         =  rgb(228/255,153/255,0/255),
  `zilightorange`    =  rgb(250/255,235/255,204/255),
  `zipurple`         =  rgb(197/255,0/255,134/255),
  `zidarkgrey`       =  rgb(88/255,88/255,90/255),
  `zigrey`           =  rgb(135/255,136/255,138/255),
  `zilightgrey`      =  rgb(217/255,218/255,219/255),
  `ziultralightgrey` =  rgb(247/255,248/255,249/255)
)


#' Zi Theme based on theme_grey
#'
#' @param Define a base_size (Defaults to 12) and base_family and bold_family for Fonts used (defaults to ggplot2's defaults)
#' @keywords theme
#' @export
#' @examples
#' library(extrafont)
#' library(tidyverse)
#' font_import()
#' loadfonts(device="win")
#' ggplot(as.data.frame(Titanic) %>% group_by(Class) %>% summarise(n=sum(Freq)), aes(x=Class,  y=n)) + geom_bar(stat="identity" , fill=zi_cols("ziblue")) +  theme_zi() 

theme_zi <- function(fontsize=14,font = "Calibri") {
  
  ggplot2::theme(
    
    # Text format:
    ## Title
    plot.title = ggplot2::element_text(family=font, size=fontsize+4, face="bold", color="#222222"),
    
    ## subtitle 
    plot.subtitle = ggplot2::element_text(family=font,size=fontsize+2,margin=ggplot2::margin(7,0,7,0)),
    plot.caption = ggplot2::element_text(family=font, size=fontsize-4),
    #This leaves the caption text element empty, because it is set elsewhere in 
    #the finalise plot function
    
    #Legend format
    legend.position = "top",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = element_text(size=fontsize,color="#222222", face = "bold"),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family=font,
                                        size=fontsize,
                                        color="#222222"),
    
    #Axis format
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(family=font,
                                      size=fontsize,
                                      color="#222222"),
    axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    
    #Grid lines
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
    panel.grid.major.x = ggplot2::element_blank(),
    
    #Blank background
    #This sets the panel background as blank, removing the standard grey ggplot 
    #background colour from the plot
    panel.background = ggplot2::element_blank(),
    
    #Strip background (#This sets the panel background for facet-wrapped plots to white, 
    #removing the standard grey ggplot background colour and sets the title size of the 
    #facet-wrap title to font size 22)
    strip.background = ggplot2::element_rect(fill="white"),
    strip.text = ggplot2::element_text(size  = fontsize,  hjust = 0)
  )
}

#' Zi Theme with axis and labels based on theme_zi() optimal suited to print maps.
#'
#' @param Define a base_size (Defaults to 12) and base_family for Fonts used (defaults to ggplot2's defaults)
#' @keywords theme
#' @export
#' @examples
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +   geom_point() +  theme_zi_void()
#'

theme_zi_titels <- function (fontsize=14,font = "Calibri") {
  theme_zi(fontsize=fontsize,font = font) +
    theme(axis.title = ggplot2::element_text(family=font,size=fontsize,face="bold",color="#222222"),
          legend.title = element_text(family=font,size=fontsize,color="#222222", face = "bold"),
          legend.position = "bottom" ,
          axis.title.y = ggplot2::element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = ggplot2::element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))
}


#' Zi Theme with for horizontal charts 
#'
#' @param Define a base_size (Defaults to 12) and base_family for Fonts used (defaults to ggplot2's defaults)
#' @keywords theme
#' @export

theme_zi_horizontal <- function (fontsize=14,font = "Calibri") {
  theme_zi(fontsize=fontsize,font = font) +
  theme(panel.grid.major.x =   element_line(color = "grey"),
          panel.grid.major.y = element_blank()) 
}


#' Zi Titels Theme with for horizontal charts 
#'
#' @param Define a base_size (Defaults to 12) and base_family for Fonts used (defaults to ggplot2's defaults)
#' @keywords theme
#' @export

theme_zi_titels_horizontal <- function (fontsize=14,font = "Calibri") {
  theme_zi_titels(fontsize=fontsize,font = font) +
    theme(panel.grid.major.x =   element_line(color = "grey"),
          panel.grid.major.y = element_blank()) 
}


#' Zi Theme without axis and labels based on theme_zi() optimal suited to print maps.
#'
#' @param Define a base_size (Defaults to 12) and base_family for Fonts used (defaults to ggplot2's defaults)
#' @keywords theme
#' @export
#' @examples
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +   geom_point() +  theme_zi_void()
#'

theme_zi_void <- function (fontsize=14, font = "Calibri") {
  theme_zi(fontsize=fontsize,font = font) +
    theme(panel.grid.major.y = element_blank(),
          axis.text.x =  element_blank(),
          axis.text.y =  element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks = element_blank(),
          legend.title = element_text(size=fontsize,color="#222222", face = "bold"),
          legend.position = c(1.1,0.25),
          legend.direction = "vertical")
}


#' Zi Datalab Theme with grey bg axis and labels based on theme_zi() optimal suited to print maps.
#'
#' @param Define a base_size (Defaults to 12) and base_family for Fonts used (defaults to ggplot2's defaults)
#' @keywords theme
#' @export
#' @examples
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +   geom_point() +  theme_zidatalab() +  scale_color_zi()


theme_zidatalab <- function (fontsize=14, font = "Calibri") {
  theme_zi(fontsize=fontsize,font = font) +
    theme(panel.grid.major.y = ggplot2::element_line(color="#a1a1a1"),
          panel.background = ggplot2::element_rect(fill="#cbcbcb"),
          plot.background = ggplot2::element_rect(fill="#cbcbcb")
    )
  
}


#' Zi Theme without axis and labels based on theme_zi() optimal suited to print maps.
#'
#' @param Define a base_size (Defaults to 12) and base_family for Fonts used (defaults to ggplot2's defaults)
#' @keywords theme
#' @export
#' @examples
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +   geom_point() +  theme_zi_void()
#'

# Function to access colors by Names
zi_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (zi_colors)
  
  zi_colors[cols]
}

# Palette Definition
#' @export
zi_palettes <- list(
  `main`        = zi_cols("ziblue", "ziorange","zigreen"),
  `mainfliped`  = zi_cols("ziblue", "zigreen","ziorange"),
  `main2colors` = zi_cols("ziblue", "zigreen"),
  `main4colors` = zi_cols("ziblue","zigreen","ziorange","zired"),
  `bluegreen`   = zi_cols("ziblue", "zigreen"),
  `greengrey`   = zi_cols("zigreen", "zigrey"),
  `bluegrey`    = zi_cols("ziblue", "zigrey"),
  `green`    = zi_cols("zidarkgreen", "ziligthgreen"),
  `red`    = zi_cols("zidarkred", "ziligthred"),
  `blue`    = zi_cols("zidarkblue", "ziligthblue"),
  `greenred`    = zi_cols("zigreen", "zigrey","zired")
)

# Palette Function
zi_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- zi_palettes[[palette]]
  if (reverse) pal <- rev(pal)
  colorRampPalette(pal, ...)
}


#' Zi color scale for ggplot2 with different pallettes
#'
#' @param Options include palette (Defaults to "main" zi Blue/Green/Orange, with other options beeing based on mostly 2 colors except of "qualitative" with 4 colors)
#' @keywords custom_scale_color()
#' @export
#' @examples
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +   geom_point() +  theme_zi() + scale_color_zi()
#'
#'
scale_color_zi <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- zi_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("zi_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}


#' Zi fill scale for ggplot2 with different pallettes
#'
#' @param Options include palette (Defaults to "main" zi Blue-Shades, with other options beeing "all", "qualitative")
#' @keywords custom_scale_color()
#' @export
#' @examples
#' ggplot(iris, aes(x=Species,y=Sepal.Width, fill = Species)) +   geom_bar(stat = "summary", fun.y = "mean") +  theme_zi() + scale_fill_zi(
#'
#'
scale_fill_zi  <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- zi_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("zi_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}


# Helper Functions to finalise plot for Print
save_plot <- function (plot_grid, width, height, save_filepath) {
  grid::grid.draw(plot_grid)
  #save it
  ggplot2::ggsave(filename = save_filepath,
                  plot=plot_grid, width=(width), height=(height),  unit="cm", bg="white")
}

#Left align text
left_align <- function(plot_name, pieces){
  grob <- ggplot2::ggplotGrob(plot_name)
  n <- length(pieces)
  grob$layout$l[grob$layout$name %in% pieces] <- 2
  return(grob)
}

create_footer <- function (source_name, logo_image_path) {
  #Make the footer
  footer <- grid::grobTree(grid::textGrob(source_name,
                                          x = 0.09, hjust = 0, gp = grid::gpar(fontsize=8, fontfamily="Calibri")), # edgar: added fontfamily Calibri
                           grid::rasterGrob(png::readPNG(logo_image_path), x = 0.04))
  return(footer)
  
}

#' Arrange alignment and save BBC ggplot chart
#'
#' Running this function will save your plot with the correct guidelines for publication for a BBC News graphic.
#' It will left align your title, subtitle and source, add the BBC blocks at the bottom right and save it to your specified location.
#' @param plot_name The variable name of the plot you have created that you want to format and save
#' @param source_name The text you want to come after the text 'Source:' in the bottom left hand side of your side
#' @param save_filepath Exact filepath that you want the plot to be saved to
#' @param width_cm Width in cm that you want to save your chart to - defaults to 640
#' @param height_cm Height in cm that you want to save your chart to - defaults to 450
#' @param logo_image_path File path for the logo image you want to use in the right hand side of your chart,
#'  which needs to be a PNG file - defaults to BBC blocks image that sits within the data folder of your package
#' @return (Invisibly) an updated ggplot object.

#' @keywords finalise_plot
#' @examples
#' finalise_plot(plot_name = myplot,
#' source = "The source for my data",
#' save_filepath = "filename_that_my_plot_should_be_saved_to-nc.png",
#' width_cm = 16,
#' height_cm = 6,
#' logo_image_path = "logo_image_filepath.png"
#' )
#'
#' @export
finalise_plot <- function(plot_name,
                          source_name,
                          save_filepath=file.path(Sys.getenv("TMPDIR"), "tmp-nc.png"),
                          width_cm=16,
                          height_cm=9,
                          logo_image_path = file.path(system.file("data", package = 'zicolors'),"zilogo_square.png")) {
  
  footer <- create_footer(source_name, logo_image_path)
  
  #Draw your left-aligned grid
  plot_left_aligned <- left_align(plot_name, c("subtitle", "title", "caption"))
  plot_grid <- ggpubr::ggarrange(plot_left_aligned, footer,
                                 ncol = 1, nrow = 2, heights=c(1,.10))
  ## print(paste("Saving to", save_filepath))
  save_plot(plot_grid, width_cm, height_cm, save_filepath)
  ## Return (invisibly) a copy of the graph. Can be assigned to a
  ## variable or silently ignored.
  return(plot_grid)
}
