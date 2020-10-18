library(maps);
library(geosphere);     # distm
library(measurements);  # conv_unit
library(RMariaDB);



t.test.jobs = function(jobs.subset, search.query.1 = "Microsoft Office", search.query.2 = "C++")
  {
  x = subsetDataFrame(jobs.subset, 
                          "search.query", "==", search.query.1)$job.count.k;
  y = subsetDataFrame(jobs.subset, 
                          "search.query", "==", search.query.2)$job.count.k;
  
  t.test(x,y);
  }
  
  
boxplotJobQueryComparison = function(jobs.subset, search.query.1 = "Microsoft Office", search.query.2 = "C++")
  {
  x = subsetDataFrame(jobs.subset, 
                          "search.query", "==", search.query.1)$job.count.k;
  y = subsetDataFrame(jobs.subset, 
                          "search.query", "==", search.query.2)$job.count.k;
  
  color.x = findjobPlotColor(jobs.subset,search.query.1);
  color.y = findjobPlotColor(jobs.subset,search.query.2); 
  
  #summary(x);
  #summary(y);
  
  boxplot(x,y, names = c(search.query.1, search.query.2), col=c(color.x,color.y)  );
  }


findjobPlotColor = function(jobs.subset, search.query)
  {
  deep.dive.sorted = unique(jobs.subset$search.query);
  n.jobs.subset = length(deep.dive.sorted);
  colors = rainbow(n.jobs.subset, s = 0.6, v = 0.75);
  idx = findAllIndexesWithValueInVector(deep.dive.sorted, search.query);
  colors[idx];
  }


lookupPairwiseValue = function(dist.df, loc.1, loc.2)
  {
  # symmetric
  df.names = names(dist.df);
  idx.1 = which(df.names == loc.1);
  idx.2 = which(df.names == loc.2);
  dist.df[idx.1, idx.2];
  }


plotJobs = function(jobs.subset,
                myy.lim=c(0,max(jobs.subset$job.count.k) ) )
  {
  deep.dive.sorted = unique(jobs.subset$search.query);
  n.jobs.subset = length(deep.dive.sorted);

  #colors = palette(rainbow(n.jobs.subset, s = 0.6, v = 0.75));
  # colors = rainbow(n.jobs.subset);
  colors = rainbow(n.jobs.subset, s = 0.6, v = 0.75);
  
    # first one ...
    my.search = deep.dive.sorted[1];
    my.subset = subsetDataFrame(jobs.subset, "search.query", "==", my.search);
    my.weeks = my.subset$year.week;
    
    n.my.subset = length(my.subset$job.count.k);
    xs = 38: (38 + n.my.subset - 1);
  plot(xs, my.subset$job.count.k, 
        xlim = c(38, 43), ylim = myy.lim, lwd=3,
        type = "l", col=colors[1],
        ylab="Job Count (in 1000s)", xlab=("Weeks 38-42 of 2020"),
        main = "Keyword trends in Data Analysis"
        );
      text(42, my.subset$job.count.k[n.my.subset],
              labels=my.subset$search.query[n.my.subset], 
                pos=4, col=colors[1], cex= 0.5);
        
        ## all the rest ...
      for(i in 2:n.jobs.subset)
        {
        my.search = deep.dive.sorted[i];
        my.subset = subsetDataFrame(jobs.subset, 
                          "search.query", "==", my.search);
        
        n.my.subset = length(my.subset$job.count.k);
        xs = 38: (38 + n.my.subset - 1);
        
        if(n.my.subset > 0)
          {
          par(new=TRUE); # overlays
          plot(xs, my.subset$job.count.k, 
                xlim = c(38, 43), ylim = myy.lim, lwd=3,
                type = "l", col=colors[i],
                ylab="", xlab="",
                main = ""
                );
          text(42, my.subset$job.count.k[n.my.subset],
                  labels=my.subset$search.query[n.my.subset], 
                  pos=4, col=colors[i], cex= 0.5);
          }
        }
    
  list("colors" = colors, "search" = deep.dive.sorted);  
  }



buildBoundingBoxFromRadiusAndGivenLatitudeLongitudeTemporaryFix = function(my.radius, my.latitude, my.longitude, my.units="mi")
  {
  # default values are in miles
  option = c("angstrom", "nm", "um", "mm", "cm", "dm", "m", "km", "inch", "ft",
              "yd", "fathom", "mi", "naut_mi", "au", "light_yr", "parsec", "point");
  if(!is.element(my.units,option)) { my.units = "mi"; } # miles
  factor.lat  = 68.703; if(my.units != "mi") { factor.lat  = measurements::conv_unit(68.703, "mi", my.units); }
  factor.long = 69.172; if(my.units != "mi") { factor.long  = measurements::conv_unit(69.172, "mi", my.units); }

  delta.latitude = my.radius / factor.lat ;
  # BUG ... # is this change true??
  # delta.longitude = my.radius / (factor.long * cos(deg2rad(my.longitude)));
  delta.longitude = my.radius / (factor.long * cos(deg2rad(delta.latitude)) );

  latitude.lower = my.latitude - delta.latitude;
  latitude.upper = my.latitude + delta.latitude;

  longitude.lower = my.longitude - delta.longitude;
  longitude.upper = my.longitude + delta.longitude;

  c(latitude.lower, latitude.upper, longitude.lower, longitude.upper);
  }

getNeighborsFromLatLong = function(my.radius, my.latitude, my.longitude, my.units) 
{
library(RMariaDB); # install.packages("RMariaDB", dependencies=TRUE);

# copy/paste _SECRET_/_SECRET_database_.txt into console...
mysql.connection = mysql.secretConnectionSQL(str="WSU_SANDBOX_", save="wsu");

	my.tablename = "zipcodes";
box = buildBoundingBoxFromRadiusAndGivenLatitudeLongitudeTemporaryFix(my.radius, my.latitude, my.longitude, my.units);

sql.template = "SELECT * FROM {tablename} WHERE latitude > {latitude.lower} AND latitude < {latitude.upper} AND longitude > {longitude.lower} AND longitude < {longitude.upper} ORDER BY zipcode ASC;";
	keys = c("tablename", "latitude.lower", "latitude.upper", "longitude.lower", "longitude.upper");	
	vals = c(my.tablename, box);	
sql = parseTemplateSQL(sql.template, keys, vals);
neighbors = mysql.fetchAllSQL(mysql.connection, sql);

if(dim(neighbors)[1] == 0) { stop( paste0(" QUERY FAILED: ", sql) ); }

print(paste0("The QUERY returned ...  ", dim(neighbors)[1], "  ... NEIGHBORS"));
# neighbors;

RMariaDB::dbDisconnect(mysql.connection);


library(geosphere);     # distm
library(measurements);  # conv_unit


copy.neighbors = neighbors;
# add row ... with empty values
copy.neighbors = rbind(copy.neighbors, NA);
  nrows = dim(neighbors)[1];  # let's add our single location to the end
copy.neighbors[1+nrows,2] = my.latitude;
copy.neighbors[1+nrows,3] = my.longitude;
copy.neighbors;


dist.neighbors = conv_unit(  
                      distm( copy.neighbors[,3:2],
                              fun=distHaversine),
                            "m", my.units);

my.distances = dist.neighbors[1+nrows,];

copy.neighbors$dist.from.reference = my.distances;

copy.neighbors$dist.within.radius = (my.distances < my.radius);

copy.neighbors = sortDataFrameByNumericColumns(copy.neighbors, "dist.from.reference", "ASC");

copy.neighbors = moveColumnsInDataFrame(copy.neighbors, c("latitude", "longitude", "zipcode", "state_long", "state"), "after", "dist.within.radius");

copy.neighbors;


good.neighbors = subsetDataFrame(copy.neighbors, "dist.within.radius", "==", TRUE);



list("box" = box, "neighbors" = neighbors, 
    "copy.neighbors" = copy.neighbors, "good.neighbors" = good.neighbors,
    "dist.neighbors" = dist.neighbors
    );
}




plotNeighbors = function(info, state="montana", county="flathead", 
    state.color = "#ffe4c4", county.color = "#014421", 
    state.border = 0.05, county.border = 0.05,
    nearby.states = c("idaho", "washington", "oregon"), 
    nearby.states.color = "white", 
    center.color = "yellow", center.pch=20, center.cex = 1.25, 
    inbox.color = "black", inbox.pch=20, inbox.cex = 1, 
    inradius.color = "white", inradius.pch=20, inradius.cex = 0.75,
    box.color = "black", box.lwd = 2,
    ellipse.color = "white", ellipse.pch=20, ellipse.cex = 0.25
)
  {
  state = tolower(state); 
  county = tolower(county); 
  nearby.states = tolower(nearby.states);
  my.region = c(state, nearby.states);
    region = map('state', region=my.region, plot=FALSE);
  state.idx = findAllIndexesWithValueInVector(region$names, state);
    region.colors = rep(nearby.states.color, times=1+length(nearby.states));
  region.colors[state.idx] = state.color;
  ### plot state with regional neighbor states
  map('state', region=my.region, col=region.colors, 
              plot = TRUE, fill = TRUE, myborder=state.border);
  
  
  data(county.fips);
  county.fip = county.fips$fips[ 
                  match( paste(state,county,sep=",") ,
                    county.fips$polyname)];
  state.fips = county.fips$fips[ 
                  match( map("county", state, plot=FALSE)$names,
                    county.fips$polyname) ];
  
  state.colors = rep(state.color, times=length(state.fips) );
  county.fip.idx = findAllIndexesWithValueInVector(state.fips, county.fip);
  
  state.colors[county.fip.idx] = county.color;
  ### plot counties, highlighting this county of interest ...
  map('county', state, col=state.colors,
                plot = TRUE, fill = TRUE, myborder=county.border);
  
  ### plot county of interest
  map('county', paste(state,county,sep=","), 
              col=county.color, bg=state.color,
              plot = TRUE, fill = TRUE, myborder=county.border);
  # flathead lake not found ...
  # my.lakes = map("lakes", "montana,flathead", plot=FALSE);
  
  ### overlay center target point (star on map)
  points( x = info$good.neighbors$longitude[1], 
          y = info$good.neighbors$latitude[1], 
          col=center.color, pch=center.pch, cex=center.cex);
  ### overlay bounding box
  rect( info$box[4], info$box[1], info$box[3], info$box[2], 
                          border = box.color, lwd = box.lwd );
  ### plot all candidates
  points( x = info$copy.neighbors$longitude[-1],
          y = info$copy.neighbors$latitude[-1], 
          col=inbox.color, pch=inbox.pch, cex=inbox.cex);
  ### overlay center target point (star on map)
  points( x = info$good.neighbors$longitude[1], 
          y = info$good.neighbors$latitude[1], 
          col=center.color, pch=center.pch, cex=center.cex);
  ### plot all "good" candidates
  points( x = info$good.neighbors$longitude[-1],
          y = info$good.neighbors$latitude[-1], 
          col=inradius.color, pch=inradius.pch, cex=inradius.cex);
  ### box is not square due to "mercator" projections
  # as a result, the circle will be an ellipse.  We have the center of the radius, and the radius.x and radius.y (by subtraction with the box data).
  ### EASTER EGG +5 ... overlay an ellipse to represent the "circle" region ...
  ### Since this is a midterm exam, this is available to everyone that gets it correct, not just the first person
  ##############################################################
  # the code below sort-of works, 
  # https://stackoverflow.com/questions/41820683
      # ellipse based on v box ...
      # phi = 0;
      # xc = info$good.neighbors$longitude[1];
      # yc = info$good.neighbors$latitude[1];
      # b = ( info$good.neighbors$latitude[1] - box[1]);
      # a = ( box[3] - info$good.neighbors$longitude[1]);
      # 
      # t <- seq(0, 2*pi, 0.01) 
      # x <- xc + a*cos(t)*cos(phi) - b*sin(t)*sin(phi);
      # y <- yc + a*cos(t)*cos(phi) + b*sin(t)*cos(phi);
      # 
      # points(x=x, y=y, col=ellipse.color, pch=ellipse.pch, cex=ellipse.cex);
  
  }
  


plotXYwithBoxPlots = function(x, y, ...)
  {
  # https://stackoverflow.com/questions/23050928/error-in-plot-new-figure-margins-too-large-scatter-plot
  # old.par.mar = par()$mar;
  # old.par.mar = c( 5.1, 4.1, 4.1, 2.1);
  old.par.mar = c(1,1,1,1);
  
  # http://rfunction.com/archives/1538
  mat <- matrix(c(1,2,0,3), 2);
  layout(mat, c(3.5,1), c(1,3));

  par(mar=c(0.5, 4.5, 0.5, 0.5));
    boxplot(x, horizontal=TRUE, axes=FALSE);
  par(mar=c(4.5, 4.5, 0.5, 0.5));
    plot(x, y, ...);
  # text(0.5, 85, "layout", cex=2)
  par(mar=c(4.5, 0.5, 0.5, 0.5));
    boxplot(y, axes=FALSE);
  
  # https://stackoverflow.com/questions/9292563/reset-the-graphical-parameters-back-to-default-values-without-use-of-dev-off
  # back to default ... # http://rfunction.com/archives/1302
  # graphics.off(); 
  # par("mar"); 
  dev.off();
  par(mar=old.par.mar);
  }



plotXYwithHistograms = function(x, y, ...)
  {
  # https://stackoverflow.com/questions/23050928/error-in-plot-new-figure-margins-too-large-scatter-plot
  # old.par.mar = par()$mar;
  # old.par.mar = c( 5.1, 4.1, 4.1, 2.1);
  old.par.mar = c(1,1,1,1);
  
  # http://rfunction.com/archives/1538
  mat <- matrix(c(1,2,0,3), 2);
  layout(mat, c(3.5,1), c(1,3));

  par(mar=c(0.5, 4.5, 0.5, 0.5));
    # https://stackoverflow.com/questions/50810198/rotating-histogram-horizontally-in-r
    # technical barplots of hist ... used same for both x,y
    #hist(x, horizontal=TRUE, axes=FALSE, main="");
  xhist = hist(x, plot = FALSE);
    barplot(xhist$counts, axes = FALSE, space = 0, horiz=FALSE, xlab= "", ylab="")
  
  par(mar=c(4.5, 4.5, 0.5, 0.5));
    plot(x, y, ...);
  # text(0.5, 85, "layout", cex=2)
  par(mar=c(4.5, 0.5, 0.5, 0.5));
  
  yhist = hist(y, plot = FALSE);
    barplot(yhist$counts, axes = FALSE, space = 0, horiz=TRUE, xlab= "", ylab="")
    #hist(y, axes=FALSE, main="");
  
  # https://stackoverflow.com/questions/9292563/reset-the-graphical-parameters-back-to-default-values-without-use-of-dev-off
  # back to default ... # http://rfunction.com/archives/1302
  
  # graphics.off(); 
  # par("mar"); 
  dev.off();
  par(mar=old.par.mar);
  }

colorsInGradient = function(n, colvec=c("red","royalblue"))
  {
  rev( colorRampPalette(colvec)(n) );
  }

getColorsFromTemperature = function(temps, temp.range, colors, na.color="#333333")
  {
  my.colors = c();
  for(temp in temps)
    {
    temp.round = round(temp);
    idx = which(temp.range == temp.round);
    if(length(idx) == 0) { my.color = na.color; } else { my.color = colors[idx]; }
    my.colors = c(my.colors, my.color);
    }
  my.colors;
  }

 
# http://www.statisticstoproveanything.com/2013/09/using-custom-images-as-pch-values-in-r.html
image_points = function(image, x, y, cex = 1, pos = NULL) {
    if (length(x) != length(y)) {
        stop("length(x)!=length(y): check your data")
    }
    dim.x = dim(image)[2]  #image width
    dim.y = dim(image)[1]  #image height
    if (dim.x == dim.y) {
        # obtian the ratio of width to height or height to width
        ratio.x = ratio.y = 1
    } else if (dim.x < dim.y) {
        ratio.x = dim.x/dim.y
        ratio.y = 1
    } else {
        ratio.x = 1
        ratio.y = dim.y/dim.x
    }
    cex = cex/10  #how large the image should be, divided by 10 so that it matches more closely to plotting points
    pin = par()$pin  #pin provides the width and height of the _active graphic device_
    pin.ratio = pin/max(pin)  #take the ratio
    usr = par()$usr  #usr provides the lower.x, lower.y, upper.x, upper.y values of the plotable region

    # combine the active device dimensions, the image dimensions, and the
    # desired output size
    image.size.y = (usr[4] - usr[3]) * pin.ratio[1] * cex
    image.size.x = (usr[2] - usr[1]) * pin.ratio[2] * cex
    for (i in 1:length(x)) {
        # plot each point pos can be NULL (default) or 1, 2, 3, or 4, corresponding
        # to centered (defualt), bottom, left, top, right, respectively.
        if (is.null(pos)) {
            # centered at (x,y), define the bottom/top and left/right boundaries of the
            # image
            x.pos = c(x[i] - (image.size.x * ratio.x)/2, x[i] + (image.size.x * 
                ratio.x)/2)
            y.pos = c(y[i] - (image.size.y * ratio.y)/2, y[i] + (image.size.y * 
                ratio.y)/2)

            rasterImage(image, x.pos[1], y.pos[1], x.pos[2], y.pos[2])
        } else if (pos == 1) {
            x.pos = c(x[i] - (image.size.x * ratio.x)/2, x[i] + (image.size.x * 
                ratio.x)/2)
            y.pos = c(y[i] - (image.size.y * ratio.y), y[i])
        } else if (pos == 2) {
            x.pos = c(x[i] - (image.size.x * ratio.x), x[i])
            y.pos = c(y[i] - (image.size.y * ratio.y)/2, y[i] + (image.size.y * 
                ratio.y)/2)
        } else if (pos == 3) {
            x.pos = c(x[i] - (image.size.x * ratio.x)/2, x[i] + (image.size.x * 
                ratio.x)/2)
            y.pos = c(y[i], y[i] + (image.size.y * ratio.y))
        } else if (pos == 4) {
            x.pos = c(x[i], x[i] + (image.size.x * ratio.x))
            y.pos = c(y[i] - (image.size.y * ratio.y)/2, y[i] + (image.size.y * 
                ratio.y)/2)
        }

        rasterImage(image, x.pos[1], y.pos[1], x.pos[2], y.pos[2])  #plot image
    }
}


buildClimateDataFrame = function(climate, months=1:12, keys=c("Record high F (C)", 
      "Average high F (C)", "Average low F (C)", "Record low F (C)", 
      "Average precipitation inches (mm)", "Average snowfall inches (cm)"), 
      keys.n = c("highmax", "highavg",  "lowavg", "lowmin", "rain", "snow"), 
      units=1 )
  {
  
  # hack-add from https://en.wikipedia.org/wiki/ISO_3166-2:US
  my.st = c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"); # ,"DC","AS","GU","MP","PR","UM","VI");
  my.capitals = unique(climate$capital);
  my.labels = paste0(my.capitals, ", ", my.st);
 
  climate.df = unique( removeAllColumnsBut(climate, c("state", "capital")) );
  climate.df$st = my.st;
  climate.df$labels = my.labels;
  
  # this has our data still ...
  climate.units = subsetDataFrame(climate, "units", "==", units);
  which.JanDec = getIndexOfDataFrameColumns(climate.units, c("Jan","Dec"));
  
  month.labels = month.abb;
  
  for(j in 1:length(my.capitals))
    {
    capital = my.capitals[j];
    climate.units.capital = subsetDataFrame(climate.units, "capital", "==", capital);
    print(capital);
    print(dim(  climate.units.capital ) );
    Sys.sleep(0.02);
    for(i in 1:length(keys))
      {
      key = keys[i];
      key.n = keys.n[i];
      climate.units.sub = subsetDataFrame(climate.units.capital, "key", "==", key);
      months.data = as.numeric( climate.units.sub[1,which.JanDec[1]:which.JanDec[2]] );
    
      print(key);
      print(key.n);
      print(dim(  climate.units.sub ) );
      print(months.data);
      Sys.sleep(0.01);
      
      for(m in months)
        {
        my.colname = paste0(key.n,".",month.labels[m]);
        print(my.colname); print( months.data[m] );
        col.names = names(climate.df);
        col.idx = which(col.names == my.colname);
        col.n = ncol(climate.df);
        if(length(col.idx) == 0)
          {
          col.names = c(col.names, my.colname);
          col.idx = 1+col.n;
          climate.df[,col.idx] = NA;
          colnames(climate.df) = col.names;
          }
        my.data = months.data[m];
        if(is.na(my.data)) { my.data = 0; } # NA values are now zero
        climate.df[j,col.idx] = my.data;
        }
      print(months.data);
      print("next key");
      }
    print(capital);
    print("next capital");
    }
  
  climate.df;
  }

plotTemperatureFromWikipediaData = function(climate, city.key="capital", city.val="Juneau", units=1,
      cex.bg=2, cex.fg=1, lwd.bg=4, lwd.fg=2)
  {
  climate.df = subsetDataFrame(climate, c(city.key,"units"), "==", c(city.val,units) );
  climate.df[is.na(climate.df)] = 0;
  
  which.JanDec = getIndexOfDataFrameColumns(climate.df, c("Jan","Dec"));
  
  keys.few = c("Record high F (C)", "Average high F (C)", "Daily mean F (C)", "Average low F (C)", "Record low F (C)", "Average precipitation inches (mm)", "Average snowfall inches (cm)" );
  keys.simple = c("high.max", "high.avg", "daily.avg", "low.avg", "low.max", "rain", "snow");

  # alaska ... -20 ... -45
  # phx ... 120 ... 122
  
  temp.range = c(-50:125);  # Helena is -42
  rain.lim = c(0,375); # top at 500 is 375 ...
  #temp.lim = c(min(temp.range), rain.lim[2]);
  temp.lim = c(min(temp.range), max(temp.range));
  month.lim = c(0.5,12.5);
  
  # FFFFFF  .. 4466EE  from = c("#FFFFFF","#4466EE");
  # 4169E1
  temp.n = length(temp.range);
    color.gradient = c( "#FF0000", "#EE0000", "#DD0000", "#CC0000", "#BB0000",
                        "#AA0000", "#FFA500", "#DD9900", "#DDEE33", "#FFFFCC", 
                        "#999999", "#D0D8FA", "#A1B2F6", "#728CF2", "#4466EE", 
                        "#334CBF", "#223390", "#111961", "#000033", "#000011");
  colors = colorsInGradient(temp.n, color.gradient);
  
  myMonths = month.name[1:12];  # month.abb  ... built in
  
  
 ##########################################################    
  ############# top ############  
  # let's setup plot with no data
  plot(1,1, col="white", 
            ylim = temp.lim, xlim=month.lim,
            ylab = "Temperature (in degrees F)",
            xlab = "Months",
            xaxt = 'n', bty = 'n', yaxt = 'n',
            sub = "Wikipedia (October 2020)", 
            main = paste0(climate.df$capital[1], ", ", climate.df$state[1])
            );
  text(1:12, par("usr")[3], labels = myMonths, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=.75)
  
  # Juneau
  keys.few = c("Record high F (C)", "Average high F (C)", "Daily mean F (C)", "Average low F (C)", "Record low F (C)", "Average precipitation inches (mm)", "Average snowfall inches (cm)" );
  keys.simple = c("high.max", "high.avg", "daily.avg", "low.avg", "low.max", "rain", "snow");
  key.end = 5;
  rect.mid = 3;
  
  # Phoenix   ... has ... Mean maximum °F (°C) 
  # that shows greater range for the day ... 
  keys.few = c("Record high F (C)", "Average high F (C)", "Average low F (C)", "Record low F (C)", "Average precipitation inches (mm)", "Average snowfall inches (cm)" );
  keys.simple = c("high.max", "high.avg",  "low.avg", "low.max", "rain", "snow");
  key.end = 4;
  rect.mid = 2;
  
  rectangles = NULL;
  n.keys = length(keys.few[1:key.end]);
  for(i in 1:n.keys)
    {
    my.key = keys.few[i];
    my.simple = keys.simple[i];
    climate.sub = subsetDataFrame(climate.df, "key", "==", my.key);
    months = climate.sub[1,which.JanDec[1]:which.JanDec[2]];
    months.data = as.numeric(months);
    rectangles = rbind(rectangles, months.data); # for shading
    }

## I have the rectangles ...

   
  upper.rect = rectangles[c(1,rect.mid),];
    upper.rect.max = max(as.numeric(upper.rect));
    upper.rect.med = median(as.numeric(upper.rect));
    upper.rect.min = min(as.numeric(upper.rect));
    upper.rect.col = getColorsFromTemperature(upper.rect.med, temp.range, colors);
  lower.rect = rectangles[c(key.end-1,key.end),];
    lower.rect.max = max(as.numeric(lower.rect));
    lower.rect.med = median(as.numeric(lower.rect));
    lower.rect.min = min(as.numeric(lower.rect));
    lower.rect.col = getColorsFromTemperature(lower.rect.min, temp.range, colors);

## grid lines  
  grid.max = 10*floor(upper.rect.max/10);
  grid.min = 10*floor(lower.rect.min/10);
  grid.steps = seq(grid.min, grid.max, by=10);
  for(grid.step in grid.steps)
    {

    grid.step.color = getColorsFromTemperature(grid.step, temp.range, colors);
    # abline(h=grid.step, col="black", lwd=2);
    abline(h=grid.step, col=grid.step.color, lwd=1);
    
    text(x=0.5,y=-5 + grid.step, labels=grid.step, 
        col=grid.step.color, cex=0.5, pos=3);
    text(x=12.5,y=-5 + grid.step, labels=grid.step, 
        col=grid.step.color, cex=0.5, pos=3);
    }

  ############# grid me ############ 
  
  # months.data = c(-25, 0, 30, 40, 50, 60, 70, 80, 90, 100, 110, 125);
  # getColorsFromTemperature(months.data, temp.range, colors);
  
  
  
## upper polygon
x = c(1:12,12:1); y = c(upper.rect[1,], rev(upper.rect[2,])); 
  polygon(x,y, col=upper.rect.col, border=NA);   
## upper polygon  
x = c(1:12,12:1); y = c(lower.rect[1,], rev(lower.rect[2,])); 
  polygon(x,y, col=lower.rect.col, border=NA);  
    
  
  for(i in 1:n.keys)
    {
    my.key = keys.few[i];
    my.simple = keys.simple[i];
    climate.sub = subsetDataFrame(climate.df, "key", "==", my.key);
    months = climate.sub[1,which.JanDec[1]:which.JanDec[2]];
    months.data = as.numeric(months);

    months.colors = getColorsFromTemperature(months.data, temp.range, colors);
    
    
    point.cex = switch(my.simple,
          "high.max"    = max(months.data),
          "high.avg"    = max(months.data),
          "daily.avg"   = median(months.data),
          "low.avg"     = min(months.data),
          "low.max"     = min(months.data),
         mean(months.data) # default case of switch
        );
    
    line.data = switch(my.simple,
          "high.max"    = max(months.data),
          "high.avg"    = max(months.data),
          "daily.avg"   = median(months.data),
          "low.avg"     = min(months.data),
          "low.max"     = min(months.data),
         mean(months.data) # default case of switch
        );
    line.color = getColorsFromTemperature(line.data, temp.range, colors)
    
    
    par(new=TRUE); # overlay line ... black
    plot(1:12, months.data, 
            ylim = temp.lim, xlim=month.lim,
            pch = 20, cex = 3, 
            col="black", 
            lwd = lwd.bg, type='l',
            ylab = "",
            xlab = "",
            xaxt = 'n', bty = 'n', yaxt = 'n',
            main = ""
            );
    
    if(my.simple != "daily.avg")
      {
      par(new=TRUE); # overlay line with color
      plot(1:12, months.data, 
              ylim = temp.lim, xlim=month.lim,
              pch = 20, cex = 3, 
              col=line.color, 
              lwd = lwd.fg, type='l',
              ylab = "",
              xlab = "",
              xaxt = 'n', bty = 'n', yaxt = 'n',
              main = ""
              );
    
      par(new=TRUE); # overlay big black dots
      plot(1:12, months.data, 
              ylim = temp.lim, xlim=month.lim,
              pch = 20, cex = cex.bg, col="black", 
              ylab = "",
              xlab = "",
              xaxt = 'n', bty = 'n', yaxt = 'n',
              main = ""
              );
      par(new=TRUE); # overlay colored dots
      plot(1:12, months.data, 
              ylim = temp.lim, xlim=month.lim,
              pch = 20, cex = cex.fg, col=months.colors, 
              ylab = "",
              xlab = "",
              xaxt = 'n', bty = 'n', yaxt = 'n',
              main = ""
              );
      }
    
    
    }
  
 ############################# 


  
  }
  
