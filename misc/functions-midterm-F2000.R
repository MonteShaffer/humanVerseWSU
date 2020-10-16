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





getNeighborsFromLatLong = function(my.radius, my.latitude, my.longitude, my.units) 
{
library(RMariaDB); # install.packages("RMariaDB", dependencies=TRUE);

# copy/paste _SECRET_/_SECRET_database_.txt into console...
mysql.connection = mysql.secretConnectionSQL(str="WSU_SANDBOX_", save="wsu");

	my.tablename = "zipcodes";
box = buildBoundingBoxFromRadiusAndGivenLatitudeLongitude(my.radius, my.latitude, my.longitude, my.units);

sql.template = "SELECT * FROM {tablename} WHERE latitude > {latitude.lower} AND latitude < {latitude.upper} AND longitude < {longitude.lower} AND longitude > {longitude.upper} ORDER BY zipcode ASC;";
	keys = c("tablename", "latitude.lower", "latitude.upper", "longitude.lower", "longitude.upper");	
	vals = c(my.tablename, box);	
sql = parseTemplateSQL(sql.template, keys, vals);
neighbors = mysql.fetchAllSQL(mysql.connection, sql);
neighbors;

RMariaDB::dbDisconnect(mysql.connection);


library(geosphere);     # distm
library(measurements);  # conv_unit


copy.neighbors = neighbors;
# add row ... with empty values
copy.neighbors = rbind(copy.neighbors, NA);
  nrows = dim(neighbors)[1];  # let's add our single location to the end
copy.neighbors[1+nrows,2] = cfalls.latitude;
copy.neighbors[1+nrows,3] = cfalls.longitude;
copy.neighbors;


dist.neighbors = conv_unit(  
                      distm( copy.neighbors[,3:2],
                              fun=distHaversine),
                            "m", my.units);

my.distances = dist.neighbors[1+nrows,];

copy.neighbors$dist.from.cfalls = my.distances;

copy.neighbors$dist.within.radius = (my.distances < my.radius);

copy.neighbors = sortDataFrameByNumericColumns(copy.neighbors, "dist.from.cfalls", "ASC");

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
              plot = TRUE, fill = TRUE, myborder=0);
  
  
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
                plot = TRUE, fill = TRUE, myborder=0);
  
  ### plot county of interest
  map('county', paste(state,county,sep=","), 
              col=county.color, bg=state.color,
              plot = TRUE, fill = TRUE, myborder=0);
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
  old.par.mar = par()$mar;
  
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
  par(mar=old.par.mar);
  }



plotXYwithHistograms = function(x, y, ...)
  {
  old.par.mar = par()$mar;
  
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
  par(mar=old.par.mar);
  }

