

displayColorOptions = function(my.colors = colors(),
                              showHEX = FALSE,
                              alpha = TRUE, # works with showHEX = TRUE
                              xlim=c(0,10),
                              ylim=c(0,10),
                              cex = 0.75,
                              ncol=2, nrow=10)
  {
  # http://www.sthda.com/english/wiki/colors-in-r

  ## build a collection of graphs, each two columsn ...
  ## limit the total number per row of a column ... pagination ...
  ## how to let them copy it?  identify? clipboard?
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

        hexcolor = rgb2col( col2rgb(mycolor, alpha=alpha)  );

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



#' colorsInGradient
#'
#' @param n Number of colors to return
#' @param colvec Vector of color names "red" or RGB "#FF0000" or RGBa "#4169E1FF"
#' @param alpha Pass transparency filter "alpha" as TRUE or FALSE
#'
#' @return vector of colors in RGB or RGBa form (depending on alpha)
#' @export
#'
#' @examples
#'
#' colorsInGradient(4, c("red", "royalblue"));
#' colorsInGradient(4, c("#FF000000", "#FF0000FF"), TRUE);  # red through alphas
#' colorsInGradient(4, c("#FF000000", "#4169E1FF"), TRUE);
#'
colorsInGradient = function(n, colvec=c("red","royalblue"), alpha=FALSE)
  {
  # colorsInGradient(4, c("red", "royalblue"));
  # colorsInGradient(4, c("#FF000000", "#FF0000FF"), TRUE);  # red through alphas
  # colorsInGradient(4, c("#FF000000", "#4169E1FF"), TRUE);  # red->royalblue through alphas
  # rgb2col( col2rgb("royalblue") );

  # alpha doesn't seem to work as expected ... unless I pass in RGBa?
  grDevices::colorRampPalette(colvec, alpha=alpha)(n);
  }


#' rgb2col
#'
#' Reverse the built-in grDevices::col2rgb function
#'
#' @param x vector of colors
#'
#' @return vector of colors in RGB hex format
#' @export
#'
#' @examples
#'
#' rgb2col( col2rgb("red") );
#' rgb2col( col2rgb("red", alpha=TRUE) );
#' rgb2col( col2rgb("#FF0000FF", alpha=TRUE) );
#' rgb2col( col2rgb("#FF000033", alpha=TRUE) );
#'
rgb2col = function(x)
  {
  # reverses col2rgb function
  x.n = dim(x)[1];
  if(x.n == 4)
    {
    x.rgb = t(x[1:4,]) /255;
    grDevices::rgb(   as.numeric(x.rgb[,1]),
                      as.numeric(x.rgb[,2]),
                      as.numeric(x.rgb[,3]),
                      as.numeric(x.rgb[,4]),
      names=rownames(x.rgb) );
    } else {
            x.rgb = t(x[1:3,]) /255;
            grDevices::rgb( x.rgb, names=rownames(x.rgb) );
            }
  }



findNearestColor = function(color, index)
  {
  # would be cool if it indexed heat map, etc.
  # at different n's ... know if it is in the
  # gradient field
  # this is fuzzySearchColor

  }



#' lookupColorInIndex
#'
#' @param search
#' @param which search "byname" or "byhex"
#' @param key which index to search
#' @param my.index list that represents the database being searched
#'
#' @return
#' @export
#'
#' @examples
#'
#' indexColorsForLookup();
#'     lookupColorInIndex("red");
#'     lookupColorInIndex("rEd");               # tolower takes care of this
#'     lookupColorInIndex("rED", "byname");
#'     lookupColorInIndex("#FF0000", "byhex");  # more than one
#'     lookupColorInIndex("#ABCDEF", "byhex");  # NA, not found
#'     lookupColorInIndex(c("red","white","blue")); # one each
#'     lookupColorInIndex(c("#FF0000","#FFFFFF","#0000FF"), "byhex"); # multinomial can create a problem
#'     lookupColorInIndex(c("#FF0000","#FFFFFF","#0000FF"), "byhex", as.list=TRUE);
#'
lookupColorInIndex = function(search="red",  which="byname", key="base", my.index = colors.indexed.by, as.list=FALSE)
  {
  # indexColors(); searchColor("red"); searchColor("#FF0000", "byhex");
  # # run indexColors first
  # indexColors();
  # searchColor("red");
  # searchColor(c("red","white","blue"));
  # searchColor("#FF0000", "byhex");
  # searchColor(c("#FF0000","#FFFFFF","#0000FF"), "byhex");
  # search index for a color, by hex or by name
  result = c();
  r.list = list();
  i = 1;
  for(s in search)
    {
    s = trimMe(tolower(s));
    if(which == "byhex") { s = toupper(s); }
    res = my.index[[key]][[which]][[s]];
    if(is.null(res)) { res = NA; }
    result = c(result, res);
    r.list[[i]] = res;
    i = 1 + i;
    }
  if(as.list) {r.list;} else {result;}
  }


#' indexColorsForLookup
#'
#' Create a keyed table of colors
#'
#' @param colornames A vector containing the color names, must be interpretable by grDevices::col2rgb
#' @param key How to store the result
#'
#' @return Doesn't return anything, assigns to Global namespace ... colors.indexed.by
#' @export
#'
#' @examples
#'
#' indexColorsForLookup();
#'     names(colors.indexed.by);
#'     names(colors.indexed.by$base);
#'     unlist( head(colors.indexed.by$base$byname) );
#'     unlist( head(colors.indexed.by$base$byhex) );
#'
indexColorsForLookup = function(colornames=grDevices::colors(), key="base", my.index=NULL, force=FALSE)
  {
  # utils::globalVariables(c("colors.indexed.by"));
  # colors.indexed.by = list();
  # create two "keyed" lists (arrays) of color ...
  #if( !is.null(colors.indexed.by[[key]]) )
  if(list.element.exists(colors.indexed.by,"base"))
    {
    if( !is.null(colors.indexed.by[[key]]) )
      {
      if(!force)  # you could force a rebuild
        {
        return(TRUE); # don't rebuild ... key exists with data
        }
      }
    }

    nc = length(colornames);

    byname = list();
    byhex = list();

    for(i in 1:nc)
      {
      my.name = colornames[i];
      my.hex = rgb2col(grDevices::col2rgb(my.name));

      byname[[my.name]] = my.hex;
      if( is.null(byhex[[my.hex]]) )
        {
        byhex[[my.hex]] = c(my.name);
        } else {
                byhex[[my.hex]] = c(byhex[[my.hex]], my.name);
                }
      }

    if(is.null(my.index)) { my.index = list(); } # previous memory, multiple "keys"
    my.index[[key]] = list();
    my.index[[key]][["byname"]] = byname;
    my.index[[key]][["byhex"]] = byhex;

    .GlobalEnv$colors.indexed.by = my.index;
    return (TRUE);
  }





