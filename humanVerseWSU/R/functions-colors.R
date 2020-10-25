# http://www.sthda.com/english/wiki/colors-in-r

## build a collection of graphs, each two columsn ... 
## limit the total number per row of a column ... pagination ...
## how to let them copy it?  identify? clipboard?

displayColorOptions = function(my.colors = colors(),
                              showHEX = FALSE,
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
  
  while(page <= pages)
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
        mycolor = as.character(my.colors[i]);
        if(is.na(mycolor)) { break; break; }
        mycolor.name = names(my.colors)[i];
        
        hexcolor = rgb2col( col2rgb(mycolor)  );
        
        if(is.null(mycolor.name)) { mycolor.name = mycolor;}
        
        if(!showHEX)
          {
          mycolor.label = paste0(mycolor.name, "  ...  ", i);
          } else { 
                  mycolor.label = paste0(mycolor.name, "  .  ", i , 
                          "  .  ", hexcolor   );
                  }
        
        
        
        
        xright  = xleft + xunit;
        ybottom = ytop - yunit;
        
        rect(xleft, ybottom, xright, ytop, col=hexcolor);  # hexcolor is safer
      
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


# colorsInGradient(4, c("red", "royalblue"));
# colorsInGradient(4, c("#FF000000", "#FF0000FF"), TRUE);  # red through alphas
# colorsInGradient(4, c("#FF000000", "#4169E1FF"), TRUE);  # red->royalblue through alphas
# rgb2col( col2rgb("royalblue") );
colorsInGradient = function(n, colvec=c("red","royalblue"), alpha=FALSE)
  {
  # alpha doesn't seem to work as expected ... unless I pass in RGBa?
  colorRampPalette(colvec, alpha=alpha)(n);
  }


# reverses col2rgb function
# col2rgb("red");
# col2rgb("red", alpha=TRUE);
# col2rgb("#FF0000FF", alpha=TRUE);
# col2rgb("#FF000033", alpha=TRUE);
# rgb2col ( col2rgb(c("#FFFF00FF","#FF330033"), alpha=TRUE) );
# rgb2col( col2rgb("red") );
# rgb2col( (x = col2rgb("#FF330033", alpha=FALSE)) );
# rgb2col( (x = col2rgb("#FF330033", alpha=TRUE)) );
# indexColors(); searchColor("red"); searchColor("#FF0000", "byhex");
rgb2col = function(x)
  {
  x.n = dim(x)[1];
  if(x.n == 4)
    {
    x.rgb = t(x[1:4,]) /255;
    rgb(  as.numeric(x.rgb[,1]), 
          as.numeric(x.rgb[,2]),
          as.numeric(x.rgb[,3]),
          as.numeric(x.rgb[,4]),
      names=rownames(x.rgb) );
    } else {
            x.rgb = t(x[1:3,]) /255;
            rgb( x.rgb, names=rownames(x.rgb) );
            }
  }



findNearestColor = function(color, index)
  {
  # would be cool if it indexed heat map, etc. 
  # at different n's ... know if it is in the
  # gradient field
  # this is fuzzySearchColor
  
  }

# # run indexColors first
# indexColors();
# searchColor("red");
# searchColor(c("red","white","blue"));
# searchColor("#FF0000", "byhex");
# searchColor(c("#FF0000","#FFFFFF","#0000FF"), "byhex");
searchColor = function(search="red",  which="byname", key="base", my.index = colors.indexed.by)
  {
  # search index for a color, by hex or by name
  result = c();
  for(s in search)
    {
    s = trimMe(tolower(s));
    if(which == "byhex") { s = toupper(s); }
    res = my.index[[key]][[which]][[s]];
    if(is.null(res)) { res = NA; }
    result = c(result, res);
    }
  result;
  }


# utils::globalVariables(c("colors.indexed.by"));
colors.indexed.by = list(); 


# create two "keyed" lists (arrays) of color ...
indexColors = function(colornames=colors(), key="base", my.index = colors.indexed.by)
  {
  if( !is.null(colors.indexed.by[[key]]) )
    {
    return (TRUE);
    } else {
            nc = length(colornames);
            
            byname = list();
            byhex = list();
            
            for(i in 1:nc)
              {
              my.name = colornames[i];
              my.hex = rgb2col(col2rgb(my.name));
              
              byname[[my.name]] = my.hex;
              if( is.null(byhex[[my.hex]]) )
                {
                byhex[[my.hex]] = c(my.name);
                } else {
                        byhex[[my.hex]] = c(byhex[[my.hex]], my.name);
                        }
              }
            my.index[[key]] = list();
            my.index[[key]][["byname"]] = byname;
            my.index[[key]][["byhex"]] = byhex;
            
            .GlobalEnv$colors.indexed.by = my.index;
            return (TRUE);
            }
 }





