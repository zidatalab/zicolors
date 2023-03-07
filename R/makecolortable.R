# 2. Create color chart showing rectangle blocks of color, along with
# index, color name, and RGB constants in hex and decimal.
# from https://raw.githubusercontent.com/EarlGlynn/colorchart/master/ColorChart.R

displaycols <- zi_cols()[-c(9,10,11)]

# For a given color, define a text color that will have good contrast.
#   Examples:
#     > SetTextContrastColor("white")
#     [1] "black"
#     > SetTextContrastColor("black")
#     [1] "white"
#     > SetTextContrastColor("red")
#     [1] "white"
#     > SetTextContrastColor("yellow")
#     [1] "black"
SetTextContrastColor <- function(color)
{
  ifelse( mean(col2rgb(color)) > 127, "black", "white")
}

# Define this array of text contrast colors that correponds to each
# member of the colors() array.
TextContrastColor <- unlist( lapply(displaycols, SetTextContrastColor) )

# Define string vector of RGB hex and decimal constants for given color
# as a string.
#   Example:
#     > GetColorHexAndDecimal("yellow")
#     [1] "#FFFF00   255 255   0"
GetColorHexAndDecimal <- function(color)
{
  clr <- col2rgb(color)
  sprintf("#%02X%02X%02X   %3d %3d %3d",
          clr[1],clr[2],clr[3], clr[1],clr[2],clr[3])
}

# Prepare text vectors to be displayed, in addition to color names.
index <- paste(1:length(displaycols))
HexAndDec <- unlist( lapply(displaycols, GetColorHexAndDecimal) )

PerColumn <- length(displaycols)

# Plot a column of color rectangles.
  
  plot(0, type="n", ylab="", xlab="",
       axes=FALSE, ylim=c(PerColumn,0), xlim=c(0,1))
  title("Zi-Farben zi_cols() mit Hex-Code und RGB-255")
  # Column 1
  base <- 0
  remaining <- length(displaycols) - base
  ColumnSize <- ifelse(remaining < PerColumn, remaining, PerColumn)
  
  rect(0.00, 0:(ColumnSize-1),
       0.99, 1:ColumnSize,
       border="black",
       col=displaycols[(base+1):(base+ColumnSize)])
  text(0.09, 0.45+(0:(ColumnSize-1)), adj=1,
       index[(base+1):(base+ColumnSize)], cex=0.6,
       col=TextContrastColor[(base+1):(base+ColumnSize)])
  text(0.12, 0.45+(0:(ColumnSize-1)), adj=0,
       names(displaycols)[(base+1):(base+ColumnSize)], cex=0.6,
       col=TextContrastColor[(base+1):(base+ColumnSize)])
  save <- par(family="mono")  # use mono-spaced font with number columns
  text(0.7, 0.45+(0:(ColumnSize-1)), adj=0,
       HexAndDec[(base+1):(base+ColumnSize)], cex=0.6,
       col=TextContrastColor[(base+1):(base+ColumnSize)])
  par(save)
