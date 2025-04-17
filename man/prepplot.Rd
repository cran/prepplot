\name{prepplot}
\alias{prepplot}
\alias{stripes}

\title{
Functions to prepare a figure region for base graphics
}
\description{
Function prepplot prepares a figure region according to individual preferences regarding background color, axes, stripes and/or gridlines; optionally, labelling can be done with the function.
Function stripes draws the vertical or horizontal stripes for prepplot and can also be used independently.
}
\usage{
prepplot(xlim, ylim,
       bg = par("bg"), xaxs=NULL, yaxs=NULL,
       lwd.axis = 0, col.axis = "grey20",
       border = ifelse(lwd.axis == 0, bg, col.axis),
       axes = TRUE, xaxt = par("xaxt"), yaxt = par("yaxt"),
       xticks = NULL, yticks = NULL,
       xticklabs = xticks, yticklabs = yticks,
       mgpx = par("mgp"), xaxside = 1,
       mgpy = mgpx, yaxside = 2,
       xlab = NULL, ylab = NULL,
       cex = par("cex"), las = 1, lasx = las, lasy = las,
       gridx = FALSE, gridy = FALSE,
       gridxminor = 0, gridyminor = 0,
       lty.grid = ifelse(max(gridxminor, gridyminor) > 0,
                         "solid", "dotted"),
       col.grid = "grey50",
       lwd = par("lwd"), lwd.grid = lwd,
       lty.grid.minor = "dotted",
       col.grid.minor = col.grid,
       lwd.grid.minor = 0.5 * lwd.grid,
       stripesx = FALSE, stripesy = FALSE,
       col.stripes = "grey90",
       axis.arrow = FALSE,
       arrow.length = 0.3, arrow.width = 0.2,
       arrow.code = 2, arrow.type = "triangle", ...)

stripes(stripesx = numeric(0), stripesy = numeric(0), col.stripes = "grey90",
       usr=NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xlim}{
required; a numeric vector of length 2 or more, whose range will be used for the horizontal axis limits
}
  \item{ylim}{
required; a numeric vector of length 2 or more, whose range will be used for the vertical axis limits
}
  \item{bg}{
background color
}
  \item{xaxs}{
\code{"r"} for regular style (4\% extended versus \code{range(xlim)}),\cr
\code{"i"} for using unextended \code{range(xlim)}\cr
\code{NULL} for using \code{par("xaxs")}, which has default \code{"r"}
}
  \item{yaxs}{
analogous to \code{xaxs}
}

  \item{lwd.axis}{
axis line width; default 0 for no axis line (see below also for \code{lwd} and \code{lwd.grid})
}
  \item{col.axis}{
axis color; also affects ticks, tickmark labels and axis labels
}
  \item{border}{
border color; default depends on whether or not there is an axis line
}
  \item{axes}{
logical that determines whether or not axes are to be drawn (only if \code{xaxt} and \code{yaxt} do not suppress their respective axes); default: \code{TRUE}; if \code{FALSE}, both tick mark labels and axis labels are suppressed; even if \code{TRUE}, an axis line is only drawn, if \code{lwd.axis} is set to a positive value; if the default axis positioning of R itself is desired, choose \code{axes=FALSE}, and add an \code{axis} statement after the \code{preplot} call (see also the Examples section)
}
  \item{xaxt}{default: \code{par("xaxt")}; set to \code{"n"} for suppressing the x axis in spite of \code{"axes=TRUE"} (you can add a horizontal axis with the \code{axis} command later on, see examples section)
}
  \item{yaxt}{default: \code{=par("yaxt")}; set to \code{"n"} for suppressing the y axis in spite of \code{"axes=TRUE"} (you can add a vertical axis with the \code{axis} command later on, see examples section)
}
  \item{xticks}{
tick positions for the horizontal axis; default \code{NULL} (see Details section)
}
  \item{yticks}{
tick positions for the vertical axis; default \code{NULL} (see Details section)
}
  \item{xticklabs}{
tick labels for the horizontal axis; default \code{xticks}; if explicitly provided, must have the same length as the \code{xticks} values derived from \code{xticks}, \code{gridx} or \code{stripesx} (see Details section)
}
  \item{yticklabs}{
tick labels for the vertical axis; default \code{yticks}; if explicitly provided, must have the same length as the \code{yticks} values derived from \code{yticks}, \code{gridy} or \code{stripesy} (see Details section)
}
  \item{mgpx}{
position of the horizontal axis label, tick marks and line in terms of margin lines (default: par("mgp")); see Details section
}
\item{xaxside}{
side of the horizontal axis; 1 for bottom (default), 3 for top
}
  \item{mgpy}{
analogous to \code{mgpx}, for vertical axis; default: equal to \code{mgpx}
}
\item{yaxside}{
side of the vertical axis; 2 for left (default), 4 for right
}
  \item{xlab}{
label for horizontal axis; default: empty
}
  \item{ylab}{
label for vertical axis; default: empty
}
  \item{cex}{
general annotation size; default \code{par("cex")}; if specified, it also adapts defaults for \code{cex.main}, \code{cex.sub}, \code{cex.axis} and  \code{cex.lab}, multiplying them with \code{cex/par("cex")}; these can be overruled by explicit specification in \dots.
}
  \item{las}{
direction of axis labeling; default: parallel to horizontal axis; you may want to change this to 0 (parallell to axes) in case of longer y-axis labels
}
  \item{lasx}{
direction of tick labels for horizontal axis; default: equal to \code{las}; note that \code{xlab} remains parallel to the axis, regardless of the choice for \code{las} (can only be changed by adding it later with \code{\link[graphics]{mtext}})
}
  \item{lasy}{
direction of tick labels for vertical axis; default: equal to \code{las}; note that \code{ylab} remains parallel to the axis, regardless of the choice for \code{las} (can only be changed by adding it later with \code{\link[graphics]{mtext}})
}
\item{gridx}{
logical or numeric; if \code{TRUE}, draws grid lines at the tick mark positions; if numeric, number of grid lines between the \code{xlim} values or numeric vector of grid line positions for horizontal axis; default: \code{FALSE} for no grid lines (positive \code{gridxminor} implies switch to \code{gridx=TRUE})
}
  \item{gridy}{
logical or numeric; if \code{TRUE}, draws grid lines at the tick mark positions; if numeric, number of grid lines between the \code{ylim} values or numeric vector of grid line positions for vertical axis; default: \code{FALSE} for no grid lines (positive \code{gridyminor} implies switch to \code{gridy=TRUE})
}
  \item{gridxminor}{
number of minor grid lines between pairs of major grid lines for horizontal axis; default: \code{0} for no minor grid lines
}
  \item{gridyminor}{
number of minor grid lines between pairs of major grid lines for vertical axis; default: \code{0} for no minor grid lines
}
  \item{col.grid}{
color for grid lines
}
  \item{lty.grid}{
line type for (major) grid lines; defaults to a dotted line with no minor grid lines and a solid line with minor grid lines (regardless of the direction of the minor grid lines)
}
  \item{lwd}{
line width (for grid lines), default is from the current \code{\link[graphics]{par}} setting
}
  \item{lwd.grid}{
line width for grid lines, default is \code{lwd}
}
  \item{col.grid.minor}{
color for minor grid lines
}
  \item{lty.grid.minor}{
line type for minor grid lines; default: dotted line
}
  \item{lwd.grid.minor}{
line width for minor grid lines (default: \code{0.5*lwd.grid})
}
  \item{stripesx}{
logical (default \code{FALSE}) or numeric vector of color change positions in striped background (vertical stripes with reference to horizontal axis); see Details section for further information; default: no stripes
}
  \item{stripesy}{
logical (default \code{FALSE}) or numeric vector of color change positions in striped background (horizontal stripes with reference to vertical axis); increasing size is assumed (or ascertained by sorting); see Details section for further information; default: no stripes
}
  \item{col.stripes}{
color of the stripes
}
  \item{axis.arrow}{
  logical (default \code{FALSE}) or numeric vector of two positions at which to draw axis arrows for vertical and horizontal axis (first value: y position for horizontal axis, second value: x position for vertical axis); if set to \code{TRUE}, the positions are the values for \code{xaxpos} and \code{yaxpos}, respectively; for one axis arrow only, use \code{NA} for the other element
  }
  \item{arrow.length}{numeric arrow length for function \code{\link[shape]{Arrows}}; default: 0.3}
  \item{arrow.width}{numeric arrow width for function \code{\link[shape]{Arrows}}; default: 0.2}
  \item{arrow.type}{character arrow type for function \code{\link[shape]{Arrows}}; default: "triangle"}
  \item{arrow.code}{default: 2 (y axis arrow upwards, x axis arrow to the right); for other choices see Details section of \code{\link[shape]{Arrows}}}
  \item{usr}{
  \code{NULL} (default), or a numeric vector with limits of the area within which to draw the stripes; the default amounts to \code{par("usr")}
  }
  \item{\dots}{
further arguments given to functions \code{plot}, \code{mtext}, \code{stripes}, \code{axis} and \code{Arrows}; these can e.g. include the option \code{tcl} for tick length.
}
}
\details{
\code{prepplot} supports the preparation of a customized plot region to which the information carrying graphical elements can be added. It can be used with all functions that allow adding to existing base graphics plots (e.g. \code{points}, \code{lines}, \code{barplot}, \code{rect}, \code{symbols}, ...). Usage with other functions is also possible, but requires careful application after setting \code{par(new=TRUE)}.

If an axis is not suppressed (by \code{axes=FALSE} or \code{xaxt="n"} or \code{xaxt="n"}), axis ticks are placed at the positions specified in \code{xticks} or \code{yticks}; if these are \code{NULL}, numeric specifications for \code{gridx} or \code{gridy} determine the tick positions, and if these are also non-existent, numeric specifications for \code{stripesx} or \code{stripesy} determine the tick positions. If all these are unspecified, the tick positions are determined from the default axis behavior using function \code{\link[grDevices]{axisTicks}} (with option \code{log=FALSE}). Per default, axis lines (whether visible or width 0) meet at the position (\code{xlim[1]}, \code{ylim[1]}) (assuming 2-element limits, which are determined from longer \code{*lim} arguments with the \code{range} function, where needed); options \code{mgpx} or \code{mgpy} can be used for moving the axis line, tick labels and/or axis labels inwards or outwards; they default to the settings in \code{par("mgp")}, and it may be convenient to change that setting rather than using the option in several \code{prepplot} calls (see also Example section). Instead of specifying the labelling with function \code{prepplot}, it can also be handled by subsequent \code{title} and/or \code{axis} statements.

For grid lines, it is possible to provide minor grid lines (by specifying the number of minor grid lines between major ones); it is also possible to specify major grid lines in complete independence of the tick mark positions, e.g. for displaying information on regulatory limits, target values or specific events (on a time axis).

For stripes, specifying \code{TRUE} uses \code{xticks} or \code{yticks} values for the creation of stripes. Elements of stripes vectors that are outside their respective axis limits are silently moved to the nearest limit of the plot area (i.e. to the suitable element of \code{usr}). Note that the sorting of stripe entries should correspond to the sorting of axis limits (i.e., e.g., if \code{xlim[1]>xlim[2]}, sorting is decreasing instead of increasing); elements of \code{stripesx} are sorted to conform to this rule, and duplicates are removed. If the remaining vector \code{stripesx} has an even number of elements, \code{length(stripesx/2)} vertical stripes are drawn between pairs of neighbouring \code{stripesx} elements. Otherwise, the handling depends on the first and last (sorted) element of \code{stripesx}: if the first (sorted) element equals \code{xlim[1]}, a narrow stripe in the beginning is drawn, and the remaining even number of \code{stripesx} elements is treated in pairs as before; otherwise, if the last (sorted) element equals \code{xlim[2]}, a narrow stripe in the end is drawn; if neither the first nor the last element coincides with an element of \code{xlim}, the last element of \code{stripesx} is simply omitted. \code{stripesy} is treated analogously to \code{stripesy}. Like gridlines, stripes can be completely independent of tick marks (see e.g. the last example).
}
\value{
The function does not return anything; it is called for its side effects.
}
\references{
Murrell, P. (2011). R graphics. CRC, Boca Raton. \url{https://www.stat.auckland.ac.nz/~paul/RG2e/}

Rahlf, T. (2017). Data visualisation with R. Springer International Publishing AG, Cham.
}
\author{
Ulrike Groemping
}

\seealso{
See also \code{\link[shape]{Arrows}} for arrows.
}
\examples{
## default
prepplot(0:10, -5:5)
prepplot(0:10, -5:5, xaxs="i", yaxs="i")
## with stripes and grid based on default tick positions
prepplot(0:10, -5:5, stripesy=TRUE, gridx=TRUE)

## with white background,
## axis lines and small ticks,
## major and minor grid for y,
## and plot area defined by axis limits
##    instead of default usr coordinates
## (border is drawn because of lwd.axis)
## mgpx moves tick position labels closer to axes
prepplot(0:10, -5:5, bg="white", xaxs="i", yaxs="i",
    lwd.axis = 1,
    gridy=c(-5,0,5), gridyminor=4,
    tcl=-0.2, mgpx=c(3,0.5,0))

## without axis lines but with default background
## looks better with bg.area="lim"
## unless actual data points extend to the limits
prepplot(0:10, -5:5, yticks=seq(-5,5,5),
         gridy=-5:5, gridx=TRUE,
         xaxs="i", yaxs="i")

## with axis arrows
## narrower margins
## small tick marks
## tick annotations close to axis
par(mar=c(3,3,2,1), mgp=c(2,0.35,0))
prepplot(0:10, -5:5, yticks=seq(-5,5,5),
         gridy=-5:5, gridx=TRUE, lwd.axis=1, tcl=-0.2,
         border="grey92", axis.arrow=TRUE)
dev.off()  ## eliminates modified par settings

## xaxs and yaxs set in par
## labeling subsequently or in prepplot
## mgp or mgpx used for moving labeling closer to axis
par(mfrow=c(1,2), xaxs="i", yaxs="i")
  ## adding labeling subsequently
  par(mgp=c(2.25,0.75,0))
  prepplot(0:10, -5:5, yticks=seq(-5,5,5),
           gridy=-5:5, gridx=TRUE)
  title(xlab="x axis label", ylab="y axis label",
  sub="Labeling added subsequently", main="mgp set in par")
  ## adding labeling subsequently
  par(mgp=c(3,1,0)) # back to default
  ## adding labeling within the function
  prepplot(0:10, -5:5, yticks=seq(-5,5,5),
           gridy=-5:5, gridx=TRUE,
           xlab="x axis label", ylab="y axis label",
           mgpx=c(2.25,0.75,0),
           main="mgpx set in prepplot",
           sub="Labeling added within prepplot")
  ## the difference: sub reacts to mgp, not to mgpx
dev.off()   ## eliminates modified par settings

## further examples in the pdf vignette
## access with vignette("prepplotOverview")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
\keyword{ aplot }
