
#' doStatsSummary
#'
#' @family Stats
#'
#' @param x a numeric vector
#'
#' @return a list of various statistical summaries
#' @export
#'
#' @examples
#' x.norm = doStatsSummary ( rnorm(100,0,1) );
#' x.unif = doStatsSummary ( runif(100,0,1) );
#'
doStatsSummary = function(x)
	{
	result = list();
		result$length = length(x);
	xx = stats::na.omit(x);
		result$length.na = length(x) - length(xx);
		result$length.good = length(xx);
	result$mean = mean(xx);
	result$mean.trim.05 = mean(xx, trim=0.05);
	result$mean.trim.20 = mean(xx, trim=0.20);

	result$median = stats::median(xx);
	result$MAD = stats::mad(xx);
	result$IQR = stats::IQR(xx);
	result$quartiles = stats::quantile(xx, prob=c(.25,.5,.75), type=1); # tries to use actual data, not averages ...
	result$deciles = stats::quantile(xx, prob=seq(0.1,0.9,by=0.1), type=1 );
	result$centiles = stats::quantile(xx, prob=seq(0.01,0.99,by=0.01), type=1 );

	result$median.weighted = matrixStats::weightedMad(xx);
	result$MAD.weighted = matrixStats::weightedMedian(xx);

	# value from data
	result$myMedian = doMedian(xx,1);
	result$myMean   = doMean(xx);

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

	result$sd = stats::sd(xx);
	result$var = stats::var(xx);

	result$var.naive = doSampleVariance(x,"naive");
	result$var.2step = doSampleVariance(x,"2step");


	## normality
	result$shapiro = stats::shapiro.test(xx);
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
#' @family Stats
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
	x = stats::na.omit(x);
	if(method=="naive")
		{
		n = 0;
		sum = 0;
		sum2 = 0;

		for(i in 1:length(x))  ## stats::na.omit(x)
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
				for(i in 1:length(x))  ## stats::na.omit(x)
					{
					n = n + 1;
					sum = sum + x[i];
					}
		if(n < 2) { return(NULL);} #
				x.bar = sum/n;
				## second pass
				for(i in 1:length(x))  ## stats::na.omit(x)
					{
					deviation = x[i] - x.bar;
					sum2 = sum2 + deviation * deviation;
					}
				s.var = sum2/(n-1);
				}

		s.sd = sqrt(s.var);
	list("x.bar"=x.bar,"s.var"=s.var,"s.sd"=s.sd);
	}


#' calculateZscores
#'
#' Calculate z-scores using formula:  (x - x.bar) / s.hat;
#'
#'
#' @family Stats
#'
#' @param x numeric vector
#' @param x.bar default NULL, you can pass in the parameter
#' @param s.hat default NULL, you can pass in the parameter
#'
#' @return z (numeric vector)
#' @export
#'
#' @examples
#' calculateZscores( 1:5 );
#' calculateZscores( 1:9 );
#'
#' calculateZscores( 1:5, x.bar=3, s.hat=1 );
#' calculateZscores( 1:9, x.bar=3, s.hat=1 );
#'
calculateZscores = function(x, x.bar=NULL, s.hat=NULL)
	{
  if(is.numeric(x.bar) && is.numeric(s.hat)) { return ((x - x.bar) / s.hat);}
  # maybe throw a warning if one is null, but not the other
  if( (is.null(x.bar) + is.null(s.hat)) == 1)
      {
      warning("Only one value was entered for x.bar / s.hat ... Computing these values instead.")
      }


	dsv = doSampleVariance(x);

	x.bar = dsv$x.bar;
	s.hat = dsv$s.sd;

	if(is.null(s.hat)) { return (NULL); }  # we take care of division by zero in our custom sampleVarianceFunction

	(x - x.bar) / s.hat;
	}


#' findOutliersUsingZscores
#'
#'
#' [assuming normality.  Is that a good assumption?]
#' \url{https://statisticsbyjim.com/basics/outliers/}
#'
#' @family Stats
#'
#'
#' @param x numeric vector
#' @param zmin what is the lower-bound cutoff to label an outlier
#' @param zmax what is the upper-bound cutoff to label an outlier
#'
#' @return list of various features of the outliers
#' @export
#'
#' @examples
#' findOutliersUsingZscores( c(-5, rep(1:3,9), 9) );
#' findOutliersUsingZscores( c(-5,-4, rep(1:3,9), 8,9) );
#' findOutliersUsingZscores( c(-5, rep(1:3,9), 9), -2, 2);
#' findOutliersUsingZscores( c(-5,-4, rep(1:3,9), 8,9), -2, 2 );
#'
findOutliersUsingZscores = function(x, zmin=-3, zmax=3)
	{
  result = list();
  result$z = z = calculateZscores(x);
  result$z.min = zmin;
  result$z.max = zmax;

	outliers = x[z < zmin | z > zmax];
	  outliers.lower.which = which(z < zmin);
	  outliers.lower = x[outliers.lower.which];
			v.lower = rep("lower", length(outliers.lower));
		outliers.upper.which = which(z > zmax);
		outliers.upper = x[outliers.upper.which];
			v.upper = rep("upper", length(outliers.upper));

	df.lower = cbind(outliers.lower, v.lower);
	df.upper = cbind(outliers.upper, v.upper);

	df = as.data.frame(rbind(df.lower,df.upper));
		colnames(df) = c("value","direction");

	result$df = df;

	result$z.lower = outliers.lower.which;  # indexes
	result$z.upper = outliers.upper.which;  # indexes

	result;
	}


#' findOutliersUsingIQR
#'
#' [assuming nothing about the data]
#' \url{https://statisticsbyjim.com/basics/outliers/}
#'
#' @family Stats
#'
#'
#' @param x numeric vector
#' @param innerFenceFactor typically 1.5 * IQR
#' @param outerFenceFactor typically 3.0 * IQR
#'
#' @return list of various features of the outliers
#'
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
findOutliersUsingIQR = function(x, innerFenceFactor=1.5, outerFenceFactor=3)
	{
  result = list();

	result$IQR = myIQR = stats::IQR(x, na.rm=TRUE);
	result$Quartiles = myQuartiles = as.numeric( stats::quantile(x, na.rm=TRUE, prob=c(.25,.5,.75)) );

	result$inner.fence = innerFence = myIQR * innerFenceFactor;
	result$outer.fence = outerFence = myIQR * outerFenceFactor;

	result$Q1.inner = Q1.inner = myQuartiles[1] - innerFence;
	result$Q1.outer = Q1.outer = myQuartiles[1] - outerFence;

	result$Q3.inner = Q3.inner = myQuartiles[3] + innerFence;
	result$Q3.outer = Q3.outer = myQuartiles[3] + outerFence;


	# values that fall inside the two inner fences are not outliers ...
	result$inner.which = inner.which = which(x < Q1.inner | x > Q3.inner);
	          inner.which.lower = which(x < Q1.inner);
	          inner.which.upper = which(x > Q3.inner);
	  result$inner = inner = x[inner.which];	# circles
	result$outer.which = outer.which = which(x < Q1.outer | x > Q3.outer);
	          outer.which.lower = which(x < Q1.outer);
	          outer.which.upper = which(x > Q3.outer);
	  result$outer = outer = x[outer.which];	# * in boxplot
	# outer and inner may have duplicates, let's remove from inner so they are disjoint ...
	result$inner.u = inner = setdiff(inner,outer);
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

	df$value = as.numeric(df$value);

	result$df = df;

    # indexes
	result$inner.lower = setdiff(inner.which.lower,outer.which.lower);
	result$inner.upper = setdiff(inner.which.upper,outer.which.upper);

	result$outer.lower = outer.which.lower;
	result$outer.upper = outer.which.upper;

	#list("df" = df, "inner" = c(i.lower,i.upper), "outer" = c(o.lower,o.upper) ); ;
	result;
	}




#' doMode
#'
#' Returns \code{mode} of a numeric vector x
#'
#' \code{mode} is the most frequent value(s) in a set of data
#'
#'
#' @family Stats
#'
#' @param x numeric vector
#'
#' @return numeric vector that contains _all_ values that are modal (could be bimodal)
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



doMedian = function(x, type=1)
  {
  xx = na.omit(x);
  as.numeric(stats::quantile(xx, prob=c(0.5), type=type));
  }

doMean = function(x)
  {
  xx = na.omit(x);
  m = mean(xx);
  deviationFromMean = abs(xx-m);
  xx[ whichMin(deviationFromMean)[1] ];  # value from data
  }

#' doModeOpposite
#'
#' Returns \code{!mode} of a numeric vector x
#'
#' \code{!mode} is the least frequent value(s) in a set of data
#'
#' @family Stats
#'
#' @param x numeric vector
#'
#' @return numeric vector that contains _all_ values that are least modal
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








