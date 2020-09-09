
# FROM EXPLORATORY_DATA_ANALYSIS
winvar<-function(x,tr=.2, na.rm=F){
#
# Compute the gamma Winsorized variance for the data in the vector x.
# tr is the amount of Winsorization which defaults to .2.
#
if(na.rm)x<-x[!is.na(x)]
y<-sort(x)
n<-length(x)
ibot<-floor(tr*n)+1
itop<-length(x)-ibot+1
xbot<-y[ibot]
xtop<-y[itop]
y<-ifelse(y<=xbot,xbot,y)
y<-ifelse(y>=xtop,xtop,y)
winvar<-var(y)
winvar
}


#' doStatsSummary
#'
#' @param x a numeric vector
#'
#' @return a list of various statistical summaries
#' @export
#'
#' @examples
#' x.norm = doStatsSummary ( rnorm(100,0,1) );
#' x.unif = doStatsSummary ( runif(100,0,1) );
doStatsSummary = function(x)
	{
	result = list();
		result$length = length(x);
	xx = na.omit(x);
		result$length.na = length(x) - length(xx);
		result$length.good = length(xx);
	result$mean = mean(xx);
	result$mean.trim.05 = mean(xx, trim=0.05);
	result$mean.trim.20 = mean(xx, trim=0.20);
	
	result$median = median(xx);
	result$IQR = IQR(xx);
	result$quartiles = quantile(xx, prob=c(.25,.5,.75));
	result$deciles = quantile(xx, prob=seq(0.1,0.9,by=0.1) );
	result$centiles = quantile(xx, prob=seq(0.01,0.99,by=0.01) );
	
	result$max = max(xx);
	result$min = min(xx);
	result$range = result$max - result$min;
	result$xlim = range(xx);
	
	result$max.idx = whichMax(x);
	result$min.idx = whichMin(x);
	
	result$mode = result$freq.max = doMode(x);  # elements with highest frequency
	result$which.min.freq = doModeOpposite(x);
	
	result$ylim = c( freqMin(xx), freqMax(xx) );
	
	# you could later get indexes of each mode(freq.max)/freq.min using findAllIndexesWithValueInVector
	
	result$sd = sd(xx);
	result$var = var(xx);
	result$var.winsor.05 = winvar(xx, tr=0.05);
	result$var.winsor.20 = winvar(xx, tr=0.20);
	result$var.naive = doSampleVariance(x,"naive");
	result$var.2step = doSampleVariance(x,"2step");
	
	
	## normality
	result$shapiro = shapiro.test(xx);
	result$shapiro.is.normal = list("0.10" = isTRUE(result$shapiro$p.value > 0.10), "0.05" = isTRUE(result$shapiro$p.value > 0.05), "0.01" = isTRUE(result$shapiro$p.value > 0.01) );
	
	result$outliers.z = findOutliersUsingZscores(x);
	result$outliers.IQR = findOutliersUsingIQR(x);
	
	#result$z = calculateZscores(x);
	
	result;
	}

#' doSampleVariance
#'
#' Computes the sample variance with (n-1) ...
#' 
#' @param x numeric vector
#' @param method "two-pass" prevents "naive" floating-point issues
#'
#' @return list (x.bar, s.var, s.sd)
#' @export
#'
#' @examples
#' doSampleVariance( c(1) ); # returns null
#' doSampleVariance( 1:2 ); 
#' 
doSampleVariance = function(x, method="two-pass")
	{
	x = na.omit(x);
	if(method=="naive")
		{
		n = 0;
		sum = 0;
		sum2 = 0;
		
		for(i in 1:length(x))  ## na.omit(x)
			{
			n = n + 1;
			sum = sum + x[i];
			sum2 = sum2 + x[i]*x[i];
			}
		
		if(n < 2) { return(NULL);} # 
			x.bar = sum/n;
			s.var = (sum2 - (sum*sum)/n)/(n-1);

		} else	{
				# two-pass algorithm # testing
				n = sum = sum2 = 0;
				## first pass 
				for(i in 1:length(x))  ## na.omit(x)
					{
					n = n + 1;
					sum = sum + x[i];
					}
		if(n < 2) { return(NULL);} # 
				x.bar = sum/n;
				## second pass 
				for(i in 1:length(x))  ## na.omit(x)
					{
					deviation = x[i] - x.bar;
					sum2 = sum2 + deviation * deviation;
					}
				s.var = sum2/(n-1);
				}

		s.sd = sqrt(s.var);
	list("x.bar"=x.bar,"s.var"=s.var,"s.sd"=s.sd);
	}

#' standardizeToMin
#'
#' Standarize a vector to its minimum
#' 
#' @param x numeric vector
#'
#' @return numeric vector, updated
#' @export
#'
#' @examples
#' standardizeToMin ( c(2: 10) );
#' standardizeToMin ( c(-2, 1: 10) );
#' 
standardizeToMin = function(x)
	{
	x/min(x, na.rm=T);
	}
	
#' standardizeToMax
#'
#' Standarize a vector to its maximum
#' 
#' @param x numeric vector
#'
#' @return numeric vector, updated
#' @export
#'
#' @examples
#' standardizeToMax ( c(2: 10) );
#' standardizeToMax ( c(-2, 1: 10) );
#' 
standardizeToMax = function(x)
	{	
	x/max(x, na.rm=T);
	}

#' standardizeToN
#'
#' Standarize a vector based on it's length
#' 
#' @param x numeric vector
#'
#' @return numeric vector, updated
#' @export
#'
#' @examples
#' standardizeToN ( c(2: 10) ); 
#' standardizeToN ( c(-2, 1: 10) ); 
#' 	
standardizeToN = function(x,count.na=F)  # row-normalization
	{	
	n = length(na.omit(x));
	if(count.na) { n = length(x); }
	x/n;
	}	
	
#' standardizeToSum
#'
#' Standarize a vector based on it's sum
#' 
#' @param x numeric vector
#'
#' @return numeric vector, updated
#' @export
#'
#' @examples
#' s.s = standardizeToSum ( c(2: 10) );    s.s;  
#' sum(s.s);
#' 
#' s.s = standardizeToN ( c(-2, 1: 10) );  s.s;  
#' sum(s.s);
#' 	
standardizeToSum = function(x)  # row-normalization
  {	
  x/sum(x, na.rm=T);
  }	



#' standardizeFromOneRangeToAnother
#'
#' @param x numeric vector
#' @param onerange this is typically the current range of the vector [numeric vector of length 2]
#' @param another this is the new scaled range [numeric vector of length 2]
#'
#' @return numeric vector, updated
#' @export
#'
#' @examples
#' standardizeFromOneRangeToAnother( 1:5, c(0,1) );  # map to [0,1]
#' standardizeFromOneRangeToAnother( runif(20, min=-1, max=1), c(0,1) );  # map to [0,1]
#' standardizeFromOneRangeToAnother( 1:5, c(1,7) );  # e.g., Likert-5 to Likert-7
#' standardizeFromOneRangeToAnother( rep(1,10), c(0,1) );  #NaN ... division by zero
#' 
standardizeFromOneRangeToAnother = function(x, another, onerange=range(x) )
	{
  if(length(another) < 2)  { return (NULL); }
  if(length(onerange) < 2) { return (NULL); }
  
	# https://stats.stackexchange.com/questions/281162/scale-a-number-between-a-range
	rmin = onerange[1]; # min
	rmax = onerange[2]; # max
	
	tmin = another[1]; # min
	tmax = another[2]; # max
	
	# if (rmax-rmin) == 0 ... will return INF or -INF
	
	tmin + (tmax-tmin) * (x - rmin) / (rmax - rmin);
	}
	
#' calculateZscores
#' 
#' Calculate z-scores using formula:  (x - x.bar) / s.hat;
#'
#' @param x numeric vector
#'
#' @return z (numeric vector)
#' @export
#'
#' @examples
#' calculateZscores( 1:5 );
#' calculateZscores( 1:9 );
#' 
calculateZscores = function(x)
	{
	dsv = doSampleVariance(x);
	
	x.bar = dsv$x.bar;
	s.hat = dsv$s.sd;
	
	if(is.null(s.hat)) { return (NULL); }  # we take care of division by zero in our custom sampleVarianceFunction
	
	(x - x.bar) / s.hat;
	}


#' findOutliersUsingZscores
#'
#' @param x numeric vector
#' @param zmin what is the lower-bound cutoff to label an outlier
#' @param zmax what is the upper-bound cutoff to label an outlier
#'
#' @return dataframe, with possibly zero rows
#' @export
#'
#' @examples
#' findOutliersUsingZscores( c(-5, rep(1:3,9), 9) );
#' findOutliersUsingZscores( c(-5,-4, rep(1:3,9), 8,9) );
#' findOutliersUsingZscores( c(-5, rep(1:3,9), 9), -2, 2);
#' findOutliersUsingZscores( c(-5,-4, rep(1:3,9), 8,9), -2, 2 );
#' 
#' # https://statisticsbyjim.com/basics/outliers/	
#' # assuming normality.  Is that a good assumption?

findOutliersUsingZscores = function(x, zmin=-3, zmax=3)
	{
	z = calculateZscores(x);
	outliers = x[z < zmin | z > zmax];
		outliers.lower = x[z < zmin];
			v.lower = rep("lower", length(outliers.lower));
		outliers.upper = x[z > zmax];
			v.upper = rep("upper", length(outliers.upper));
			
	df.lower = cbind(outliers.lower, v.lower);
	df.upper = cbind(outliers.upper, v.upper);
	
	df = as.data.frame(rbind(df.lower,df.upper));
		colnames(df) = c("value","direction");

	df;
	}


#' findOutliersUsingIQR
#'
#' @param x numeric vector
#' @param innerFenceFactor typically 1.5 * IQR
#' @param outerFenceFactor typically 3.0 * IQR
#'
#' @return dataframe, with possibly zero rows
#' @export
#'
#' @examples
#' findOutliersUsingIQR( c(-5, rep(1:3,9), 9) );
#' findOutliersUsingIQR( c(-5,-4, rep(1:3,9), 8,9) );
#' findOutliersUsingIQR( c(-5, rep(1:3,9), 9), 1, 2);  # unchanging 
#' findOutliersUsingIQR( c(-5,-4, rep(1:3,9), 8,9), 1, 2 );  # unchanging 
#' findOutliersUsingIQR( c(-5, rep(1:3,9), 9), 2, 4);  # unchanging 
#' findOutliersUsingIQR( c(-5,-4, rep(1:3,9), 8,9), 2, 4 );  # unchanging 
#' 
#' # https://statisticsbyjim.com/basics/outliers/	
#' # assuming nothing about the data 	
findOutliersUsingIQR = function(x, innerFenceFactor=1.5, outerFenceFactor=3)
	{
	myIQR = IQR(x, na.rm=T);
	myQuartiles = as.numeric( quantile(x, na.rm=T, prob=c(.25,.5,.75)) );
	
	innerFence = myIQR * innerFenceFactor;
	outerFence = myIQR * outerFenceFactor;
	
	Q1.inner = myQuartiles[1] - innerFence;
	Q1.outer = myQuartiles[1] - outerFence;
	
	Q3.inner = myQuartiles[3] + innerFence;
	Q3.outer = myQuartiles[3] + outerFence;
	
	
	# values that fall inside the two inner fences are not outliers ...
	inner = x[x < Q1.inner | x > Q3.inner];	# circles
	outer = x[x < Q1.outer | x > Q3.outer];	# * in boxplot
	# outer and inner may have duplicates, let's remove from inner so they are disjoint ...
	inner = setdiff(inner,outer);
	# I could separate into lower and upper for later manipulation
	
	v.inner = rep("inner", length(inner));
		inner.lower = inner[inner < Q1.inner];
			v.inner.lower = rep("lower", length(inner.lower));
		inner.upper = inner[inner > Q3.inner];
			v.inner.upper = rep("upper", length(inner.upper));
	v.outer = rep("outer", length(outer));
		outer.lower = outer[outer < Q1.outer];
			v.outer.lower = rep("lower", length(outer.lower));
		outer.upper = outer[outer > Q3.outer];
			v.outer.upper = rep("upper", length(outer.upper));
		
	df.inner = cbind( c(inner.lower, inner.upper), v.inner, c(v.inner.lower, v.inner.upper) );
	
	df.outer = cbind( c(outer.lower, outer.upper), v.outer, c(v.outer.lower, v.outer.upper) );
	
	df = as.data.frame(rbind(df.inner,df.outer));
		colnames(df) = c("value","fence","direction");

	df;
	}



#' findAllIndexesWithValueInVector
#'
#' @param x numeric vector
#' @param search single number
#'
#' @return numeric vector that contains the indexes of search %in% x
#' @export
#'
#' @examples
#' findAllIndexesWithValueInVector( 1:9, 5);
#' findAllIndexesWithValueInVector( rep(1:9, 5), 5);
#' findAllIndexesWithValueInVector( sample( rep(1:9, 5) ), 5);
findAllIndexesWithValueInVector = function(x,search)
	{
	nx = length(x);
	mat.x = as.data.frame( cbind(1:nx,x) );
		colnames(mat.x) = c("idx","x");	
	mat.s = mat.x[mat.x$x==search, ];	
	as.numeric( na.omit ( as.numeric( as.vector (mat.s$idx) ) ) );
	}
	

	

#' whichMax
#'
#' behaves like which.max(x) but returns multiple indexes if required.
#' 
#' @param x numeric vector
#'
#' @return numeric vector that contains the indexes of *all* max elements, not just the *first*
#' @export
#'
#' @examples
#' which.max( c(87, presidents[1:30], 87) );
#' whichMax( c(87, presidents[1:30], 87) );
#' 
whichMax = function(x)  
	{
	# behaves like which.max(x) but returns multiple
	x.max = max( x, na.rm=T ); # we remove NA to figure out what to search for, but use original to map indexes 
	findAllIndexesWithValueInVector(x,x.max);
	}

#' whichMin
#'
#' behaves like which.min(x) but returns multiple indexes if required.
#' 
#' @param x numeric vector
#'
#' @return numeric vector that contains the indexes of *all* min elements, not just the *first*
#' @export
#'
#' @examples
#' which.min( c(23, presidents[1:30], 23) );
#' whichMin( c(23, presidents[1:30], 23) );
#' 	
whichMin = function(x)  
	{
	# behaves like which.min(x) but returns multiple
	x.min = min( x, na.rm=T ); # we remove NA to figure out what to search for, but use original to map indexes 
	findAllIndexesWithValueInVector(x,x.min);
	}
	
#' doMode
#'
#' Returns *mode* of a numeric vector x
#' 
#' *mode* is the most frequent value(s) in a set of data
#' 
#' @param x 
#'
#' @return numeric vector that contains *all* values that are modal (could be bimodal)
#' @export
#'
#' @examples
#' doMode( c(1:9) );
#' doMode( c(1, 1:9, 9) );
#' doMode( c(1, 1:9, 9, 9) );
#' 
doMode = function(x) # alias ?
	{
	whichMaxFreq(x);
	}
	
whichMaxFreq = function(x)  # doMode
	{
	x.table = as.data.frame( table(x) );
		freq.max = max( x.table$Freq );
	x.list = x.table[x.table$Freq==freq.max,];	
	xs = as.numeric( as.vector (x.list$x) );
	xs;
	}

#' doModeOpposite
#'
#' Returns *!mode* of a numeric vector x
#' 
#' *!mode* is the least frequent value(s) in a set of data
#' 
#' @param x 
#'
#' @return numeric vector that contains *all* values that are least modal 
#' @export
#'
#' @examples
#' doModeOpposite( c(1:9) );
#' doModeOpposite( c(1, 1:9, 9) );
#' doModeOpposite( c(1, 1:9, 9, 9) );
#' 	
doModeOpposite = function(x)  # alias ?
	{
	whichMinFreq(x);
	}
	
whichMinFreq = function(x) # opposite of doMode
	{
	x.table = as.data.frame( table(x) );
		freq.min = min( x.table$Freq );
	x.list = x.table[x.table$Freq==freq.min,];	
	xs = as.numeric( as.vector (x.list$x) );
	xs;
	}
	




#' freqMax
#'
#' What frequency is maximum in x (occurs the most)?
#'  
#' @param x numeric vector
#'
#' @return count, integer of frequency
#' @export
#'
#' @examples
#' f.max = 1:9; 
#' freqMax( f.max ); # 1, from all of them
#' 
#' f.max = c( rep(1, 3), rep(2:8,5), rep(9,1) );  
#' freqMax( f.max );  # 5, from the 2:8 (ties)
#' 
freqMax = function(x) 
	{
	x.table = as.data.frame( table(x) );
		freq.max = max( x.table$Freq );
	freq.max;
	}

	
#' freqMin
#'
#' What frequency is minimum in x (occurs the least)?
#'  
#' @param x numeric vector
#'
#' @return count, integer of frequency
#' @export
#'
#' @examples
#' f.min = 1:9; 
#' freqMin( f.min ); # 1, from all of them
#' 
#' f.min = c( rep(1, 3), rep(2:8,5), rep(9,1) );  
#' freqMin( f.min ); # 1, from the 9
#' 
freqMin = function(x) 
	{
	x.table = as.data.frame( table(x) );
		freq.min = min( x.table$Freq );
	freq.min;
	}




# x = iris[,2];  # stats functions are more vector functions



		

  #  x
  #  z = (x-xc)/oc ... xc = mean ... oc = sd ... linear ... ...
  # x,y ... plot ( cbind(x,y) ) ;  .... # corr(x,y); # 1
  # abline(v=0);
  # ... one big vector ... hist(everything) ...



# function isClose to wrap isTRUE and all.equal	
# remove lastElementFromVector ... return = T
# x = x[-c(length(x))]
# standardize to min
# standarize to max
# standardize to n ("row-normalization");
# standarize to [0,1] for min/max
# standarize from [a,b] to [c,d] ... [0,1] above is a special case ...
# z-scores 
# nested z-scores (byrow, then bycol)
#					bycol, then by row

# do scatterplot of all z-score / standarize with x ...
#
# Using Z-scores to Detect Outliers
# Using Quantiles to Detect Outliers ...  1.5/3*IQR ... inner/outfence ... https://statisticsbyjim.com/basics/outliers/
# make 1.5 and 3 options of the function ... they are based on distance of the IQR ...



if(FALSE)
	{
	
	
	# have separate function diagnosticGraphs(x)
	# stem(xx);
	# hist(xx);
	# hist(log(xx));
	result$boxplot = boxplot(xx); # str of outliers
	## let's build outliers via z-scores  and IQR ...
	
	library(car);
		qqPlot(xx);
	library(ggpubr)
		ggqqplot(xx);
		ggdensity(xx);
	library(KernSmooth);
		bin.count = dpih(xx);
		#mybreaks = ( max(xx)-min(xx) ) * bin.count;
		mybreaks = 100 * bin.count;
		
		mxlim = c(mean(xx)-3.5*sd(xx) , mean(xx)+3.5*sd(xx) );
		
		
		
		
		
		hist(xx,breaks=mybreaks, xlim=mxlim, ylim=c(0,result$ylim[2]), xlab="", xaxt='n',  );
		par(new=T); # overlay
			# theoretical normal gaussian 
			xt = seq(-3.5,3.5, length=100);
			yt = dnorm(xt);
		
		plot( xt, yt, type="l", lwd=2, axes=F, xlab="",ylab="");
		axis(1, at = -3:3, labels = c( expression("-3"~hat(sigma) ), expression("-2"~sigma ), expression("-1"~hat(s) ), expression(bar(x)), expression("1"~hat(s) ), "2s", c( expression("3"~hat(sigma) ))) );
		#axis(1, at = -3:3, labels = c("-3s", "-2s", "-1s", "hat(mu)", "1s", "2s", "3s"))
		
				
		
		par(new=T);
		plot( density(xx, kernel="epanechnikov") , xlim=mxlim, main="", xlab="", ylab="", xaxt='n', yaxt='n'  );
			abline(v=mean(xx));
			abline(v=mean(xx)-sd(xx), col="green");
			abline(v=mean(xx)+sd(xx), col="green");
			abline(v=mean(xx)-2*sd(xx), col="blue");
			abline(v=mean(xx)+2*sd(xx), col="blue");
			abline(v=mean(xx)-3*sd(xx), col="red");
			abline(v=mean(xx)+3*sd(xx), col="red");

	# https://astrostatistics.psu.edu/su07/R/html/grDevices/html/plotmath.html
	
	## plotmeans ... p = 0.95 (from alpha)


## legend ...




	## dput ...
	## ?par 
	
	}
