# Specification of colors according to Corporate Design Guidelines as of 2023
# Note: This Script is heavily inspired by the BBC Style Guide

zi_colors <- c(
  `zisignalrot`           = rgb(240/255,25/255,75/255),
  `zilindgruen`           = rgb(45/255,205/255,115/255),
  `zihimmelblau`          = rgb(60/255,155/255,250/255),
  `ziblaugruen`           = rgb(25/255,75/255,90/255),
  `ziblaugruenhell`       = rgb(65/255,130/255,150/255),
  `zihimmelblauhell`      = rgb(190/255,225/255,255/255),
  `zihimmelblaudunkel`    = rgb(50/255,105/255,200/255),
  `zilindgruenhell`       = rgb(190/255,235/255,205/255),
  `zilindgruendunkel`     = rgb(30/255,145/255,80/255),
  `ziblaugrau`            = rgb(115/255,145/255,155/255),
  `ziblaugrauhell`        = rgb(235/255,240/255,240/255)
)

#' Zi Theme based on theme_grey
#'
#' @param Define a base_size (Defaults to 10) and base_family and bold_family for Fonts used (defaults to ggplot2's defaults)
#' @keywords theme
#' @export
#' @examples
#' \dontrun{
#' library(showtext)
#' font_add_google(name = "Roboto Condensed", family = "Roboto Condensed")
#' showtext_auto()
#' ggplot(as.data.frame(Titanic) %>% group_by(Class) %>% summarise(n=sum(Freq)), aes(x=Class,  y=n)) + geom_bar(stat="identity" , fill=zi_cols("zihimmelblau")) +  theme_zi() 
#' }

theme_zi <- function(fontsize=10, font = "Roboto Condensed") {
  
  ggplot2::theme(
    
    # Text format:
    text = ggplot2::element_text(family=font, color="#194B5A", size=fontsize),
    
    ## Title
    plot.title = ggplot2::element_text(size=fontsize*1.2, face="bold"), #, margin=ggplot2::margin(0,0,0,0)
    
    ## subtitle 
    plot.subtitle = ggplot2::element_text(size=fontsize*1.2),
    #Legend format
    legend.position = "bottom",
    legend.justification='left',
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    
    #Axis format
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(color="#194B5A"),
    axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    
    #Grid lines
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color="#ebf0f0"),
    panel.grid.major.x = ggplot2::element_blank(),
    
    #Blank background
    #This sets the panel background as blank, removing the standard grey ggplot 
    #background colour from the plot
    panel.background = ggplot2::element_blank(),
    
    #Strip background (#This sets the panel background for facet-wrapped plots to white, 
    #removing the standard grey ggplot background colour)
    strip.background = ggplot2::element_rect(fill="white"),
    strip.text = ggplot2::element_text(color="#194B5A", hjust = 0),
    
    #for facet plots its useful to have more space between plots
    panel.spacing = unit(2, "lines")
  )
}

#' Zi Theme with axis and labels based on theme_zi() optimal suited to print maps.
#'
#' @param Define a base_size (Defaults to 12) and base_family for Fonts used (defaults to ggplot2's defaults)
#' @keywords theme
#' @export
#' @examples
#' \dontrun{
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +   geom_point() +  theme_zi_void()
#' }

theme_zi_axistitles <- function (fontsize=10, font = "Roboto Condensed") {
  theme_zi(fontsize=fontsize, font = font) +
    theme(axis.title = element_text(size=fontsize, face="bold"),
          axis.title.y = ggplot2::element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = ggplot2::element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))
}


#' Zi Theme with for horizontal charts 
#'
#' @param Define a base_size (Defaults to 12) and base_family for Fonts used (defaults to ggplot2's defaults)
#' @keywords theme
#' @export

theme_zi_horizontal <- function (fontsize=10, font = "Roboto Condensed") {
  theme_zi(fontsize=fontsize, font = font) +
    theme(panel.grid.major.x =   element_line(color = "#ebf0f0"),
          panel.grid.major.y = element_blank()) 
}


#' Zi Titels Theme with for horizontal charts 
#'
#' @param Define a base_size (Defaults to 12) and base_family for Fonts used (defaults to ggplot2's defaults)
#' @keywords theme
#' @export

theme_zi_axistitles_horizontal <- function (fontsize=10, font = "Roboto Condensed") {
  theme_zi_axistitles(fontsize=fontsize, font = font) +
    theme(panel.grid.major.x =   element_line(color = "#ebf0f0"),
          panel.grid.major.y = element_blank()) 
}


#' Zi Theme without axis and labels based on theme_zi() optimal suited to print maps.
#'
#' @param Define a base_size (Defaults to 12) and base_family for Fonts used (defaults to ggplot2's defaults)
#' @keywords theme
#' @export
#' @examples
#' \dontrun{
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +   geom_point() +  theme_zi_void()
#' }

theme_zi_void <- function (fontsize=10, font = "Roboto Condensed") {
  theme_zi(fontsize=fontsize,font = font) +
    theme(panel.grid.major.y = element_blank(),
          axis.text.x =  element_blank(),
          axis.text.y =  element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks = element_blank(),
          legend.position = c(1.1,0.25),
          legend.direction = "vertical")
}


#' Zi Datalab Theme with grey bg axis and labels based on theme_zi() optimal suited to print maps.
#'
#' @param Define a base_size (Defaults to 12) and base_family for Fonts used (defaults to ggplot2's defaults)
#' @keywords theme
#' @export
#' @examples
#' \dontrun{
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +   geom_point() +  theme_zidatalab() +  scale_color_zi()
#' }


theme_zidatalab <- function (fontsize=10, font = "Roboto Condensed") {
  theme_zi(fontsize=fontsize, font = font) +
    theme(panel.grid.major.y = ggplot2::element_line(color="#ebf0f0"),
          panel.background = ggplot2::element_rect(fill="white"),
          plot.background = ggplot2::element_rect(fill="white")
    )
  
}


# Function to access colors by Names
#' @export
zi_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (zi_colors)
  
  zi_colors[cols]
}

# Palette Definition
#' @export
zi_palettes <- list(
  `main`        = zi_cols("zihimmelblau", "zilindgruen"),
  `main3colors` = zi_cols("zihimmelblau", "zilindgruen", "ziblaugruen"),
  `main4colors` = zi_cols("zihimmelblau", "zilindgruen", "ziblaugruen", "zisignalrot"),
  `bluegrey`    = zi_cols("zihimmelblau", "ziblaugrau"),
  `divergent`   = zi_cols("zihimmelblau", "ziblaugrauhell", "zilindgruen")
)

# Palette Function
#' @export
zi_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- zi_palettes[[palette]]
  if (reverse) pal <- rev(pal)
  colorRampPalette(pal, ...)
}


#' Zi color scale for ggplot2 with different pallettes
#'
#' @param Options include palette (Defaults to "main" zi Blue/Green/Bluegreen, with other options being based on mostly 2 colors except of "qualitative" with 4 colors)
#' @keywords custom_scale_color()
#' @export
#' @examples
#' \dontrun{
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +   geom_point() +  theme_zi() + scale_color_zi()
#' }

scale_color_zi <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- zi_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("zi_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}


#' Zi fill scale for ggplot2 with different palettes
#'
#' @param Options include palette (Defaults to "main" zi Blue-Shades, with other options being "all", "qualitative")
#' @keywords custom_scale_color()
#' @export
#' @examples
#' \dontrun{
#' ggplot(iris, aes(x=Species,y=Sepal.Width, fill = Species)) +   geom_bar(stat = "summary", fun.y = "mean") +  theme_zi() + scale_fill_zi()
#' }

scale_fill_zi  <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- zi_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("zi_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

#' Arrange alignment and save ggplot chart
#'
#' Running this function will save your plot with left align your title, subtitle and right align source, add the Zi logo at the bottom right and save it to your specified location.
#' @param plot_name The variable name of the plot you have created that you want to format and save
#' @param source_name The text you want to in the bottom left hand side of your plot
#' @param save_filepath Exact filepath that you want the plot to be saved to
#' @param width Width in cm that you want to save your chart to - defaults to 16
#' @param height Height in cm that you want to save your chart to - defaults to 9
#' @param logo_image_path File path for the logo image you want to use in the right hand side of your chart,
#'  which needs to be a PNG file - defaults to Zi square-ish logo that sits within the data folder of your package
#' @return (Invisibly) an updated ggplot object.

#' @keywords finalise_plot_onlymagick
#' @examples
#' \dontrun{
#' finalise_plot_onlymagick(plot_name = myplot,
#' source = "The source for my data",
#' save_filepath = "filename_that_my_plot_should_be_saved_to-nc.png",
#' width = 16,
#' height = 9,
#' logo_image_path = "logo_image_filepath.png"
#' )
#' }
#'
#' @export
finalise_plot_onlymagick <- function(plot_name,
                                     source_name,
                                     save_filepath=file.path(Sys.getenv("TMPDIR"), "tmp-nc.png"),
                                     width=16,
                                     height=9,
                                     logo_image_path = file.path(system.file("data", package = 'zicolors'),
                                                                 "zilogo_square.png")) {
  
  showtext_opts(dpi = 300)
  # png(save_filepath, width=width, height=height, units="cm", res=300) # 
  # plot_name
  # dev.off()
  
  ggsave(save_filepath, plot=plot_name, 
         # device=png(), type="cairo",
         width=width, height=height,
         units="cm", dpi=300)
  
  # Call back the plot
  plot <- image_read(save_filepath)
  
  # get px size of image
  px_width <- image_info(plot) %>% pull(width)
  px_height <- image_info(plot) %>% pull(height)
  
  # And bring in a logo
  logo_raw <- image_read(logo_image_path) 
  
  # Scale down the logo and give it a border and annotation
  logowidth <- as.character(as.integer(px_width/10))
  logoheight <- as.character(as.integer(px_height/10))
  showtext_auto()
  logo <- logo_raw %>%
    image_scale(paste0(logowidth, "x", logoheight)) %>% 
    image_background("white", flatten = TRUE) %>%
    image_extent(color="white",
                 gravity="east",
                 paste0(
                   as.character(as.integer(px_width)),
                   "x",
                   as.character(logoheight)
                 )
    ) %>%
    image_annotate(text = source_name, 
                   size=20, 
                   color = "#194B5A", 
                   # font = "Roboto Condensed", 
                   gravity = "east", 
                   location = paste0("+", as.character(logowidth))) # 
  
  # Stack them on top of each other
  final_plot <- image_append(c(plot, logo), stack = TRUE)
  # And overwrite the plot without a logo
  image_write(final_plot, save_filepath)
  
}

#' Arrange alignment and save ggplot chart
#'
#' Running this function will save your plot with left align your title, subtitle and right align source, add the Zi logo at the bottom right and save it to your specified location.
#' @param plot_name The variable name of the plot you have created that you want to format and save
#' @param source_name The text you want to in the bottom left hand side of your plot
#' @param save_filepath Exact filepath that you want the plot to be saved to
#' @param width Width in cm that you want to save your chart to - defaults to 16
#' @param height Height in cm that you want to save your chart to - defaults to 9
#' @param logo_image_path File path for the logo image you want to use in the right hand side of your chart,
#'  which needs to be a PNG file - defaults to Zi square-ish logo that sits within the data folder of your package
#' @return (Invisibly) an updated ggplot object.

#' @keywords finalise_plot_captionmagick
#' @examples
#' \dontrun{
#' finalise_plot_captionmagick(plot_name = myplot,
#' source = "The source for my data",
#' save_filepath = "filename_that_my_plot_should_be_saved_to-nc.png",
#' width = 16,
#' height = 9,
#' logo_image_path = "logo_image_filepath.png"
#' )
#' }
#'
#' @export
finalise_plot_captionmagick <- function(
    plot_name,
    source_name,
    save_filepath=file.path(Sys.getenv("TMPDIR"), "tmp-nc.png"),
    width=16,
    height=9,
    logo_image_path = file.path(system.file("data", package = 'zicolors'),
                                "zilogo_square.png")) {
  
  showtext_opts(dpi = 300)
  # png(save_filepath, width=width, height=height, units="cm", res=300) # 
  # plot_name
  # dev.off()
  
  ggsave(save_filepath, 
         plot=plot_name + 
           labs(caption=source_name), 
         # device=png(), type="cairo",
         width=width, height=height,
         units="cm", dpi=300)
  
  # Call back the plot
  plot <- image_read(save_filepath)
  
  # get px size of image
  px_width <- image_info(plot) %>% pull(width)
  px_height <- image_info(plot) %>% pull(height)
  
  # And bring in a logo
  logo_raw <- image_read(logo_image_path) 
  
  # Scale down the logo and give it a border and annotation
  logowidth <- as.character(as.integer(px_width/10))
  logoheight <- as.character(as.integer(px_height/10))
  logo <- logo_raw %>%
    image_scale(paste0(logowidth, "x", logoheight)) %>% 
    image_background("white", flatten = TRUE) %>%
    image_extent(color="white",
                 gravity="east",
                 paste0(
                   as.character(as.integer(px_width)),
                   "x",
                   as.character(logoheight)
                 ))
  
  # Stack them on top of each other
  final_plot <- image_append(c(plot, logo), stack = TRUE)
  # And overwrite the plot without a logo
  image_write(final_plot, save_filepath)
  
}

#' Arrange alignment and save ggplot chart
#'
#' Running this function will save your plot with left align your title, subtitle and right align source, add the Zi logo at the bottom right and save it to your specified location.
#' @param plot_name The variable name of the plot you have created that you want to format and save
#' @param source_name The text you want to in the bottom left hand side of your plot
#' @param save_filepath Exact filepath that you want the plot to be saved to
#' @param width Width in cm that you want to save your chart to - defaults to 16
#' @param height Height in cm that you want to save your chart to - defaults to 9
#' @param logo_image_path File path for the logo image you want to use in the right hand side of your chart,
#'  which needs to be a PNG file - defaults to Zi square-ish logo that sits within the data folder of your package
#' @return (Invisibly) an updated ggplot object.

#' @keywords finalise_plot_compositemagick
#' @examples
#' \dontrun{
#' finalise_plot_compositemagick(plot_name = myplot,
#' source = "The source for my data",
#' save_filepath = "filename_that_my_plot_should_be_saved_to-nc.png",
#' width = 16,
#' height = 9,
#' logo_image_path = "logo_image_filepath.png"
#' )
#' }
#'
#' @export
finalise_plot_compositemagick <- function(
    plot_name,
    source_name,
    save_filepath=file.path(Sys.getenv("TMPDIR"), "tmp-nc.png"),
    width=16,
    height=9,
    logo_image_path = file.path(system.file("data", package = 'zicolors'),
                                "zilogo_square.png")) {
  
  showtext_opts(dpi = 300)
  # png(save_filepath, width=width, height=height, units="cm", res=300) # 
  # plot_name
  # dev.off()
  
  ggsave(save_filepath, 
         plot=plot_name + 
           labs(caption=source_name), 
         # device=png(), type="cairo",
         width=width, height=height,
         units="cm", dpi=300)
  
  # Call back the plot
  plot <- image_read(save_filepath)
  
  # get px size of image
  px_width <- image_info(plot) %>% pull(width)
  px_height <- image_info(plot) %>% pull(height)
  
  # And bring in a logo
  logo_raw <- image_read(logo_image_path) 
  
  # Scale down the logo and give it a border and annotation
  logowidth <- as.character(as.integer(px_width/10))
  logoheight <- as.character(as.integer(px_height/10))
  logo <- logo_raw %>%
    image_scale(paste0(logowidth, "x", logoheight)) %>% 
    image_background("white", flatten = TRUE)
  
  # Stack them on top of each other
  final_plot <- plot %>% 
    image_composite(logo, gravity="southwest")
  # And overwrite the plot without a logo
  image_write(final_plot, save_filepath)
  
}
