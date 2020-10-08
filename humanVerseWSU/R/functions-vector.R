
#' findAllIndexesWithValueInVector
#'
#' @family Vectors
#'
#' @param x numeric vector
#' @param search single number
#'
#' @return NA if not found; otherwisenumeric vector that contains the indexes of search %in% x
#' @export
#'
#' @examples
#' findAllIndexesWithValueInVector( 1:9, 42);  # NA
#' findAllIndexesWithValueInVector( 1:9, 5);
#' findAllIndexesWithValueInVector( rep(1:9, 5), 5);
#' findAllIndexesWithValueInVector( sample( rep(1:9, 5) ), 5);
findAllIndexesWithValueInVector = function(x,search)
	{
	nx = length(x);
	mat.x = as.data.frame( cbind(1:nx,x) );
		colnames(mat.x) = c("idx","x");
	mat.s = mat.x[mat.x$x==search, ];
	result = as.numeric( stats::na.omit ( as.numeric( as.vector (mat.s$idx) ) ) );
	if(length(result) == 0) { return (NA); }
	result;
	}




#' whichMax
#'
#' behaves like which.max(x) but returns multiple indexes if required.
#'
#' @family Vectors
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
#' @family Vectors
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




#' whichMaxFreq (doMode)
#'
#' Returns \code{mode} of a numeric vector x
#'
#' \code{mode} is the most frequent value(s) in a set of data
#'
#' @family Vectors
#'
#' @param x numeric vector
#'
#' @return numeric vector that contains _all_ values that are modal (could be bimodal)
#' @export
#'
#' @examples
#' whichMaxFreq( c(1:9) );
#' whichMaxFreq( c(1, 1:9, 9) );
#' whichMaxFreq( c(1, 1:9, 9, 9) );
#'
whichMaxFreq = function(x)  # doMode
	{
	x.table = as.data.frame( table(x) );
		freq.max = max( x.table$Freq );
	x.list = x.table[x.table$Freq==freq.max,];
	xs = as.numeric( as.vector (x.list$x) );
	xs;
	}




#' whichMinFreq (doModeOpposite)
#'
#' Returns \code{!mode} of a numeric vector x
#'
#' \code{!mode} is the least frequent value(s) in a set of data
#'
#' @family Vectors
#'
#' @param x numeric vector
#'
#' @return numeric vector that contains _all_ values that are least modal
#' @export
#'
#' @examples
#' whichMinFreq( c(1:9) );
#' whichMinFreq( c(1, 1:9, 9) );
#' whichMinFreq( c(1, 1:9, 9, 9) );
#'
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
#' @family Vectors
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
#' @family Vectors
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


#' notDuplicated
#'
#' demonstrate !duplicated() syntax using set notation
#'
#' @family Vectors
#'
#' @param x vector (numeric or character)
#'
#' @return vector, integers of indexes of "unique" or "distinct" values
#' @export
#'
#' @examples
#' notDuplicated( c(1, rep(2:8,5), 9) );
#' notDuplicated( c( rep(1,3), rep(2:8,5), rep(9,7)  ) );
#'
#' library(datasets);
#' data(iris);
#' head(iris);
#' notDuplicated( as.character(iris$Species) );
#'
#'
#' notDuplicated( iris$Sepal.Length );
#' notDuplicated( as.numeric(unlist(iris["Sepal.Length"])) );
#'
notDuplicated = function(x)
  {
  new.x = c();  # new vector
  new.idx = c();
  nx = length(x);
  for(i in 1: nx)
    {
    #if(!(x[i] %in% new.x))  # set notation
    if(!is.element(x[i],new.x))
      {
      new.x = c(new.x, x[i]);
      new.idx = c(new.idx,i);
      }
    }
  new.idx;
  }



#' doUnique
#'
#' demonstrate unique() syntax using set notation
#'
#' @family Vectors
#'
#' @param x vector (numeric or character)
#'
#' @return vector, integers of indexes of "unique" or "distinct" values
#' @export
#'
#' @examples
#' doUnique( c(1, rep(2:8,5), 9) );
#' doUnique( c( rep(1,3), rep(2:8,5), rep(9,7)  ) );
#'
#' library(datasets);
#' data(iris);
#' head(iris);
#' doUnique( as.character(iris$Species) );
#'
#'
#' doUnique( iris$Sepal.Length );
#' doUnique( as.numeric(unlist(iris["Sepal.Length"])) );
#'
doUnique = function(x)
  {
  new.x = c();  # new vector
  nx = length(x);
  for(i in 1: nx)
    {
    #if(!(x[i] %in% new.x))  # set notation
    if(!is.element(x[i],new.x))
      {
      new.x = c(new.x, x[i]);
      }
    }
  new.x;
  }
