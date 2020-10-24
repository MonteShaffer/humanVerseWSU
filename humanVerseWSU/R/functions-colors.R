# http://www.sthda.com/english/wiki/colors-in-r

# Generate a plot of color names which R knows about.
#++++++++++++++++++++++++++++++++++++++++++++
# cl : a vector of colors to plots
# bg: background of the plot
# rot: text rotation angle
#usage=showCols(bg="gray33")
showCols <- function(cl=colors(), bg = "grey",
                     cex = 0.75, rot = 30) {
    m <- ceiling(sqrt(n <-length(cl)))
    length(cl) <- m*m; cm <- matrix(cl, m)
    require("grid")
    grid.newpage(); vp <- viewport(w = .92, h = .92)
    grid.rect(gp=gpar(fill=bg))
    grid.text(cm, x = col(cm)/m, y = rev(row(cm))/m, rot = rot,
              vp=vp, gp=gpar(cex = cex, col = cm))
  }
# showCols(cl= colors(), bg="gray33", rot=30, cex=0.75)

## build a collection of graphs, each two columsn ... 
## limit the total number per row of a column ... pagination ...
## how to let them copy it?  identify? clipboard?

library(grid);

displayColorOptions = function(my.colors = colors(),
                              xlim=c(0,10),
                              ylim=c(0,10),
                              cex = 0.75, 
                              ncol=2, nrow=10)
  {
  nc = length(my.colors);
  per.page = ncol*nrow;
  pages = ceiling(nc / per.page);

  i = 1;
  page = 1;
  
  xunit = diff(xlim) / ncol;
  yunit = diff(ylim) / nrow;
  
  while(page < pages)
    {
    xstart = xlim[1];
    ystart = ylim[2];
    plot.new( );
    plot.window( 
                xlim=xlim, 
                ylim=ylim,
                log="",
                par(mar=c(0.25, 0.25, 0.25, 0.25))

              );
    
    for(c in 1: ncol)
      {
      xleft = xstart + (c-1) * xunit;
      ytop  = ystart;
      for(r in 1: nrow)
        {
        mycolor = my.colors[i];
        mycolor.label = paste0(mycolor, "  ... ",i);
        xright  = xleft + xunit;
        ybottom = ytop - yunit;
        
        rect(xleft, ybottom, xright, ytop, col=mycolor);
      
          top.y = mean(c(ytop,ytop,ytop,ybottom));
        text(xleft, top.y, label=mycolor.label, 
                              cex=cex, pos=4, col="black");
          bottom.y = mean(c(ytop,ybottom,ybottom,ybottom));
        text(xleft, bottom.y, label=mycolor.label, 
                              cex=cex, pos=4, col="white");
        
        i = 1 + i;
        ytop = ybottom;
        }
      ytop  = ystart;
      }
    page = 1 + page;
    }
  }



