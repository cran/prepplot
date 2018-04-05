prepplot <- function(xlim, ylim,                      
                     bg = "grey92", xaxs=NULL, yaxs=NULL,
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
                     col.stripes = "grey98",
                     axis.arrow = FALSE,
                     arrow.length = 0.3, arrow.width = 0.2, 
                     arrow.code = 2, arrow.type = "triangle", ...){
  ## prepare a plot region for adding a plot
  ## a background color can be provided (bg)
  ## the plot area is extended by 4% versus axis limits or not (xaxs and yaxs)
  ## axes can be suppressed by axes or xaxt or yaxt argument
  ## if axes are not suppressed, 
     ## xticks and / or yticks determine tick positions
     ## if these are not given (i.e. default NULL is kept)
     ## gridx / gridy or stripesx / stripesy determine tick locations
     ## if neither of these is given either, 
     ##    default tick positions are used
  ## Per default, an axis line is not displayed (lwd.axis=0), 
  ##    and the border has background color; 
  ## if an axis line is activated (by specifying a positive line width),
  ##    the border has axis color per default, and
  ##    placement of axis line, tick mark and axis labels is controlled
  ##        by mgpx and mgpy arguments (default: par("mgp"))
  ## cex affects all annotation text sizes by multiplying the 
  ##    respective defaults with cex/par("cex")
  ##    unless specific cex instances are separately specified
  ##    (e.g. cex.main)
  ## las affects the text direction for the tick labels
  
  ## gridlines can be provided for x and/or y
  ##    (note: gridlines or stripes, not both for the same axis)
  ## if tick marks are specified and gridx / gridy = TRUE, 
  ##    grid lines are drawn at tick positions
  ##       if tick marks are not specified, 
  ##       grid positions also serve to define tick mark positions
  ## gridxminor / gridyminor specify the number of minor grid lines
  ##    between major grid lines (assuming regular grid positions, 
  ##    step length determined from first two elements of gridx)
  
  ## stripes can be provided 
  ##    (note: stripes or gridlines, not both for the same axis)
  ## stripesx / stripesy default to FALSE (no stripes));
  ## TRUE leads to stripes defined by tick positions;
  ## a numeric vector specifies values between which stripes 
  ##    of color col.stripes are drawn (i.e. pattern with bg color)
  ## warning if both colors are the same
  aufruf <- match.call()
  if (is.null(xaxs)) xaxs <- par("xaxs")   ## default "r", alternative "i"
  if (is.null(yaxs)) yaxs <- par("yaxs")   ## default "r", alternative "i"

  stopifnot(xaxs %in% c("r", "i"))
  stopifnot(yaxs %in% c("r", "i"))
  stopifnot(is.numeric(xlim), is.numeric(ylim), 
            is.numeric(xaxside), is.numeric(yaxside),
            is.numeric(mgpx), is.numeric(mgpy))
  stopifnot(xaxside %in% c(1,3), yaxside %in% c(2,4))
  stopifnot(length(xlim)>=2, length(ylim)>=2)
  if (length(xlim)>2) xlim <- range(xlim)
  if (length(ylim)>2) ylim <- range(ylim)
  stopifnot(is.numeric(gridx) || is.logical(gridx), 
            is.numeric(gridy) || is.logical(gridy),
            is.numeric(stripesx) || is.logical(stripesx),
            is.numeric(stripesy) || is.logical(stripesy))
  stopifnot(is.numeric(gridxminor), is.numeric(gridyminor))
  if (gridxminor > 0 && is.logical(gridx)) gridx <- TRUE
  if (gridyminor > 0 && is.logical(gridy)) gridy <- TRUE
  if ((is.logical(gridx) && gridx) || (is.numeric(gridx) && length(gridx)>0))
    if ((is.logical(stripesx) && stripesx) || 
        (is.numeric(stripesx) && length(stripesx)>1)) 
        stop("don't specify both grid lines and stripes for x axis")
  if ((is.logical(gridy) && gridy) || (is.numeric(gridy) && length(gridy)>0))
    if ((is.logical(stripesy) && stripesy) || 
        (is.numeric(stripesy) && length(stripesy)>1)) 
        stop("don't specify both grid lines and stripes for y axis")
  
  stopifnot(is.numeric(cex))
  stopifnot(cex>0)
  cexfact <- cex/par("cex")
  ## all default font sizes are affected by the same percentage
  ## if not specifically set
  if (!"cex.axis" %in% names(aufruf)) cex.axis <- cexfact*par("cex.axis")
     else cex.axis <- aufruf$cex.axis
  if (!"cex.lab" %in% names(aufruf)) cex.lab <- cexfact*par("cex.lab")
     else cex.lab <- aufruf$cex.lab
  if (!"cex.main" %in% names(aufruf)) cex.main <- cexfact*par("cex.main")
     else cex.main <- aufruf$cex.main
  if (!"cex.sub" %in% names(aufruf)) cex.sub <- cexfact*par("cex.sub")
     else cex.sub <- aufruf$cex.sub
  
  ## warnings muffle function from Romain Francois
  ## http://romainfrancois.blog.free.fr/index.php?post/2009/05/20/Disable-specific-warnings
  h <- function(w) if( any( grepl( "mgp[1:3]", w) ) ) 
    invokeRestart( "muffleWarning" )
  # call mgpx and mgpy with the command below, 
  # in order to suppress warnings because of differing signs
  # withCallingHandlers( command depending on mgp*, warning = h )
  
  stopifnot(is.logical(axes))
  if (!is.null(xticks)) {
    stopifnot(length(xticks)==length(xticklabs))
    if (is.logical(gridx) && gridx) 
      gridx <- xticks
  }
  if (!is.null(yticks)) {
    stopifnot(length(yticks)==length(yticklabs))
    if (is.logical(gridy) && gridy) 
      gridy <- yticks
  }
  
  if (is.numeric(gridx) && length(gridx)==1) gridx <- seq(xlim[1], xlim[2],
                                     length.out=gridx+2)
  if (is.numeric(gridy) && length(gridy)==1) gridy <- seq(ylim[1], ylim[2],
                                     length.out=gridy+2)
  if (is.logical(gridx) && !gridx) gridx <- numeric(0)
  if (is.logical(gridy) && !gridy) gridy <- numeric(0)
  if (is.logical(stripesx) && !stripesx) stripesx <- numeric(0)
  if (is.logical(stripesy) && !stripesy) stripesy <- numeric(0)
  ## for xticks=NULL, gridx or stripesx may still be TRUE instead of numeric vector
  ## for yticks=NULL, gridy or stripesy may still be TRUE instead of numeric vector
  
  ## handle x tick labels
    if (is.null(xticks)){
      if (is.numeric(gridx) && length(gridx) > 0)
        xticks <- gridx
      else 
        if (length(stripesx) > 1) 
          xticks <- stripesx
    if (!is.null(xticks) && !is.null(xticklabs) && 
        !length(xticklabs)==length(xticks)){
          warning("xticklabs implies different number of ticks than gridx or stripesx and is ignored")
          xticklabs <- NULL
        }
    }
    if (!is.null(xticks) && is.null(xticklabs)) xticklabs <- xticks  
    ## non-empty after accounting for gridx or stripesx, with NULL or valid xticklabs

    ## still null, xticklabs not permitted
    ## default ticks are obtained after usr is obtained from plot statement
    if (is.null(xticks)) {
      if (!is.null(xticklabs)) 
        warning("Without xticks, gridx or stripesx specified, xticklabs is ignored.")
      #xticks <- xticklabs <- axisTicks(xlim, log=FALSE)
    }
  
  ## handle y tick labels
    if (is.null(yticks)){ 
      if (is.numeric(gridy) && length(gridy) > 0)
        yticks <- gridy
      else 
        if (length(stripesy) > 1)
          yticks <- stripesy
    if (!is.null(yticks) && !is.null(yticklabs) && 
        !length(yticklabs)==length(yticks)){
        warning("yticklabs implies different number of ticks than gridy or stripesy and is ignored")
        yticklabs <- NULL
      }
    }
    if (!is.null(yticks) && is.null(yticklabs)) yticklabs <- yticks  
    ## non-empty after accounting for gridx or stripesx, with NULL or valid xticklabs

    ## still null, yticklabs not permitted
    ## default ticks are obtained after usr is obtained from plot statement
    if (is.null(yticks)) {
      if (!is.null(yticklabs)) 
        warning("Without yticks, gridy or stripesy specified, yticklabs is ignored.")
      #yticks <- yticklabs <- axisTicks(ylim, log=FALSE)
    }
  
  ### the action starts
  plot(xlim, ylim, xlim=xlim, ylim=ylim, xaxs=xaxs, yaxs=yaxs, 
       type="n", axes=FALSE, xlab="", ylab="", cex.axis=cex.axis, 
       cex.main=cex.main, cex.sub=cex.sub, ...)
  ## main is handled here, axis labels (even if given) are not 
  ## (i.e., cex.lab is not needed)
  
  usr <- par("usr")

  ## obtain default tick positions from usr
  if (is.null(xticks))
    xticks <- xticklabs <- axisTicks(usr[1:2], log=FALSE)
  if (is.null(yticks))
    yticks <- yticklabs <- axisTicks(usr[3:4], log=FALSE)
  ## obtain default grid and / or stripes positions
  ## where logical is defined
  ## handle gridx / gridy=TRUE for default ticks
  if (is.logical(gridx) && gridx) gridx <- xticks
  ## the FALSE case was already handled above
  if (is.logical(gridy) && gridy) gridy <- yticks
  ## the FALSE case was already handled above
  ## handle stripesx / stripesy=TRUE for default ticks
  if (is.logical(stripesx) && stripesx) {
    stripesx <- xticks
    if (bg==col.stripes) warning("stripe color equals background color")
  }
  ## the FALSE case was already handled above
  if (is.logical(stripesy) && stripesy) {
    stripesy <- yticks
    if (bg==col.stripes) warning("stripe color equals background color")
  }
  ## the FALSE case was already handled above
  
  if (gridxminor > 0 && !length(gridx)==0){ 
    if (length(unique(round(diff(gridx),8)))>1) 
      warning("irregular spacing of major x grid lines")
    xminorstep <- (gridx[2]-gridx[1])/(gridxminor+1)
  }
  if (gridyminor > 0 && !length(gridy)==0){ 
    if (length(unique(round(diff(gridy),8)))>1) 
      warning("irregular spacing of major y grid lines")
    yminorstep <- (gridy[2]-gridy[1])/(gridyminor+1)
  }
  
  ## overall background; 
  ## border will be redrawn after potential overdraws by other activities
    rect(usr[1],usr[3],usr[2],usr[4], col=bg, border=border)

  ## prepare x stripes
  ## enforce correct range
    descendx <- xlim[2] < xlim[1]
    
  stripesx[stripesx < min(xlim)] <- ifelse(descendx, usr[2], usr[1])
  stripesx[stripesx > max(xlim)] <- ifelse(descendx, usr[1], usr[2])
  stripesx <- unique(stripesx)
  ls <- length(stripesx)
  stripesx <- sort(stripesx, decreasing=descendx)
  if (!ls%%2==0){
    ## prepare stripe to the left of first stripe
    ## or to the right of the last stripe
    first <- abs(stripesx[1]-usr[1]) > abs(stripesx[ls] - usr[2])
    if (first)
      stripesx <- c(usr[1], stripesx)
    else 
      stripesx <- c(stripesx, usr[2])
  }
  ## prepare y stripes
  descendy <- ylim[2] < ylim[1]
  stripesy[stripesy < min(ylim)] <- ifelse(descendy, usr[4], usr[3])
  stripesy[stripesy > max(ylim)] <- ifelse(descendy, usr[3], usr[4])
  stripesy <- unique(stripesy)
  ls <- length(stripesy)
  if (!ls%%2==0){
    stripesy <- sort(stripesy, decreasing=(ylim[1]>ylim[2]))
    ## prepare stripe below first stripe
    ## or above the last stripe
    first <- abs(stripesy[1]-usr[3]) > abs(stripesy[ls] - usr[4])
    if (first)
      stripesy <- c(usr[3], stripesy)
    else 
      stripesy <- c(stripesy, usr[4])
  }
  ## draw stripes (nothing happens, if both length 0)
  stripes(stripesx, stripesy, col.stripes=col.stripes, usr=usr, ...)
  
  ## draw vertical grid lines, if requested
  if (length(gridx) > 0) {
    if (gridxminor > 0)
      abline(v=seq(gridx[1], gridx[length(gridx)], xminorstep), 
                 col=col.grid.minor, lty=lty.grid.minor, 
                 lwd=lwd.grid.minor) 
                # y1=ifelse(lwd.axis>0, ylim[1], usr[3]), 
                # y2=ifelse(lwd.axis>0, ylim[2], usr[4]))  
    abline(v=gridx, col=col.grid, lty=lty.grid, lwd=lwd.grid) 
               # y1=ifelse(lwd.axis>0, ylim[1], usr[3]), 
               # y2=ifelse(lwd.axis>0, ylim[2], usr[4]))  
  }
  ## draw horizontal grid lines, if requested
  if (length(gridy) > 0) {
    if (gridyminor > 0)
      abline(h=seq(gridy[1], gridy[length(gridy)], yminorstep), 
                 col=col.grid.minor, lty=lty.grid.minor, 
                 lwd=lwd.grid.minor) 
                 # x1=ifelse(lwd.axis>0, xlim[1], usr[1]), 
                 # x2=ifelse(lwd.axis>0, xlim[2], usr[2]))  
    abline(h=gridy, col=col.grid, lty=lty.grid, lwd=lwd.grid) 
                # x1=ifelse(lwd.axis>0, xlim[1], usr[1]), 
                # x2=ifelse(lwd.axis>0, xlim[2], usr[2]))  
  }
  
  ## redraw border, if it does not conceal major grid lines
  #if (length(intersect(usr, c(gridx, gridy)))==0)
  rect(usr[1],usr[3],usr[2],usr[4], border=border)

  ## draw axes, if needed

  if (!is.null(xticks) && axes && !xaxt=="n") 
    axis(side=xaxside, at=xticks, labels=xticklabs, col=col.axis, col.axis = col.axis,
         las=lasx, lwd=lwd.axis, cex.axis=cex.axis, mgp=mgpx, ...)
  
  if (!is.null(yticks) && axes && !yaxt=="n") 
    axis(side=yaxside, at=yticks, labels=yticklabs, col=col.axis, col.axis = col.axis,, 
         las=lasy, lwd=lwd.axis, cex.axis=cex.axis, mgp=mgpy, ...)

  ## if given, main and sub were already handled in plot statement
  if (!is.null(xlab) && !xaxt=="n") 
    title(xlab=xlab, col.lab=col.axis, cex.lab=cex.lab,
          mgp=mgpx)
  if (!is.null(ylab) && !yaxt=="n") 
    title(ylab=ylab, col.lab=col.axis, cex.lab=cex.lab,
          mgp=mgpy)

  if (!is.logical(axis.arrow) || ((is.logical(axis.arrow) && axis.arrow))){
    if (is.logical(axis.arrow)) axis.arrow=c(ifelse(xaxside==1, usr[3], usr[4]),
                                             ifelse(yaxside==2, usr[1], usr[2]))
    if (is.numeric(axis.arrow)){
      if (length(axis.arrow)==2){
        Arrows(axis.arrow[2],usr[3], axis.arrow[2], usr[4], 
               col=col.axis, arr.length=arrow.length, 
               arr.width=arrow.width, code=2, arr.type=arrow.type, 
               arr.adj=1, lwd=1.5*lwd.grid, xpd=TRUE, ...)
        Arrows(usr[1],axis.arrow[1],usr[2],axis.arrow[1], 
               col=col.axis, arr.length=arrow.length, 
               arr.width=arrow.width, code=2, arr.type=arrow.type, 
               arr.adj=1, lwd=1.5*lwd.grid, xpd=TRUE, ...)
      }
    }
  }
}
