
#' removeColumnsFromDataFrame
#'
#' \code{removeColumnsFromDataFrame} removes one or more columns
#' from a dataframe \code{df} based on the names \code{mycols}
#' of said columns.
#'
#' @family DataFrame
#'
#' @param df dataframe
#' @param mycols names of cols to remove ... string or vector of strings will work
#'
#' @return dataframe, updated
#' @export
#'
#' @examples
#' library(datasets);
#' data(iris);
#' head(iris);
#' df = removeColumnsFromDataFrame(iris,"Species");
#' head(df);
removeColumnsFromDataFrame = function(df,mycols)
	{
	for(mycol in mycols)  # mycols could be just a single string, which is automatically treated as a vector of length 1.
		{
		df[mycol] = NULL;
		}
	df;
	}




#' removeDuplicatesFromDataFrame
#'
#' Based on the current order of the dataframe, it will remove
#' duplicate values in the column.
#'
#' @family DataFrame
#'
#' @param df dataframe
#' @param mycolumn name of column to look for unique/distinct values ... string
#'
#' @return dataframe, updated
#' @export
#'
#' @examples
#' library(datasets);
#' data(iris);
#' head(iris);
#' df = removeDuplicatesFromDataFrame(iris,"Species");
#' head(df);
removeDuplicatesFromDataFrame	= function(df,mycolumn)
	{
	# one column at a time
	ndf = df[!duplicated(df[mycolumn]), ];
	}

#' getIndexOfDataFrameColumns
#'
#' @family DataFrame
#'
#' @param df dataframe
#' @param mycols names of cols to find idx's ... string or vector of strings will work
#'
#'
#' @return numeric vector of the same length as mycols, in the same order as mycols
#' @export
#'
#' @examples
#' library(datasets);
#' data(iris);
#' head(iris);
#' mycols = c("Sepal.Width","Petal.Width");
#' getIndexOfDataFrameColumns(iris,mycols);
#'
#' mycols = c("Petal.Length","Sepal.Length");
#' getIndexOfDataFrameColumns(iris,mycols);
#'
getIndexOfDataFrameColumns = function(df,mycols)
	{
	n.cols = length(mycols);
	result = numeric(n.cols);
	for(i in 1:n.cols)
		{
		mycol = mycols[i];
		result[i] = which( names(df)== mycol );
		}
	if(n.cols == 1)
		{
		result[1];
		} else 	{
				result;
				}
	}



#' moveColumnsInDataFrame
#'
#' @family DataFrame
#'
#' @param ndf dataframe
#' @param mycols names of cols to move ... string or vector of strings will work
#' @param where "after" places after anchor; "before" places before anchor
#' @param anchor anchor to which we are moving ... either name or numeric idx
#'
#'
#' @return dataframe, updated
#' @export
#'
#' @examples
#'
#' library(datasets);
#' data(iris);
#' head(iris);
#' df = moveColumnsInDataFrame(iris,"Species","before","Sepal.Length");
#' head(df);
#' df = moveColumnsInDataFrame(iris,c("Sepal.Length","Sepal.Width"),"after","Species");
#' head(df);  # might be a bug if all are moved?
#'
moveColumnsInDataFrame = function(ndf, mycols, where, anchor)
	{
	# anchor is a colname by default, but can be an index (is numeric)
	# mycols are names of the cols ... we will get their locations ...
	# where can be "before" or "after" the anchor
  # some of mycols can be before/after anchor to begin with, doesn't matter ...

	anchor.idx = anchor;
	if(!is.numeric(anchor)) { anchor.idx = getIndexOfDataFrameColumns(ndf, anchor); }

	n.mycols = length(mycols);
	mycols.idx = numeric();
	for(i in 1:n.mycols)
		{
		mycol = mycols[i];
		idx = getIndexOfDataFrameColumns(ndf, mycol);
		if(idx != anchor) # can't have anchor in list ... if so, we don't include it ...
			{
			mycols.idx[i] = idx;
			}
		}
	last.one = dim(ndf)[2];
		order.start = 1:last.one;
	to.move = mycols.idx;
		order.remaining = setdiff(order.start,to.move);

	# moving 5,8,2 to after 7 ...

	new.anchor = which(order.remaining == anchor.idx);  # we could have some cols before/after anchor ...

	if(new.anchor == 1)
		{
		before = NULL;
		} else 	{
				if(where=="before")
					{
					before = order.remaining[1:(new.anchor-1)];
					} else 	{
							# default is after ...
							before = order.remaining[1:(new.anchor)];
							}
				}
	after = setdiff(order.remaining,before);

	reorder = c(before, to.move, after);

	mdf = ndf[, reorder];

	mdf;
	}


#' replaceDateStringWithDateColumns
#'
#' @family DataFrame
#'
#' @param df dataframe
#' @param mycolumn df$mycolumn is a DateString to be replaced
#' @param newcols vector(s) of same length as df$mycolumn that will replace column
#'
#' @return dataframe, updated
#' @export
#'
replaceDateStringWithDateColumns = function(df, mycolumn, newcols)
		{

		date.idx = getIndexOfDataFrameColumns(df,mycolumn);	# we have the anchor ...
			ndf = cbind(df,newcols);
				mycols = colnames(newcols); # they must already be named ...
		ndf = moveColumnsInDataFrame(ndf, mycols, date.idx, "after"); # could be "before", we are going to kill it on the next line ...
		ndf = removeColumnsFromDataFrame(ndf,mycolumn);
		ndf;
		}











#' sortDataFrameByNumericColumns
#'
#' @family DataFrame
#'
#' @param df dataframe
#' @param mycols names of cols to sort on
#' @param direction direction of sort "ASC" or "DESC" ... if a vector, must be of same length as mycols
#'
#' @return dataframe, updated
#' @export
#'
#' @examples
#' library(datasets);
#' data(iris);
#' df = iris[1:10,];
#' df$date.strings = c("3/24/2010 18:33", "9/3/2009 17:28", "10/14/2009 11:40",
#' "7/3/2015 11:16","11/18/2010 1:29","4/23/2011 0:08","10/6/2010 11:13",
#' "7/26/2009 13:23","4/9/2008 13:40","8/20/2008 11:32");
#' df$year = convertDateStringToFormat(df$date.strings,"%Y","%m/%d/%Y %H:%M");
#' df$week = convertDateStringToFormat(df$date.strings,"%W","%m/%d/%Y %H:%M");
#' df$day = convertDateStringToFormat(df$date.strings,"%j","%m/%d/%Y %H:%M");
#'
#' df = removeColumnsFromDataFrame(df,"date.strings");
#'     mycols = c("year","week", "day");
#' sortDataFrameByNumericColumns(df,mycols,"ASC");
#' sortDataFrameByNumericColumns(df,mycols,"DESC");
#'
#'     mydirs = c("ASC","DESC","ASC");
#' sortDataFrameByNumericColumns(df, mycols, mydirs );
#'
#' sortDataFrameByNumericColumns(df,sample(mycols),sample(mydirs) );
#'
sortDataFrameByNumericColumns = function (df, mycols, direction="DESC")
	{
	# if direction.length is not n.cols, we will only use the first element ...

	n.cols = length(mycols);
	n.dirs = length(direction);

	if(n.dirs < n.cols) { direction = direction[1]; n.dirs = 1;} # we need to have at least n.cols number of directions, or we will assume it is only one ...

	vecs = matrix(NA, nrow=dim(df)[1],ncol=n.cols);


	for(i in 1:n.cols)
		{
		idx = which( names(df)== mycols[i] );
		dir = if(n.dirs==1) { direction } else { direction[i] };

		if(dir == "ASC")
			{
			vecs[,i] = df[,idx];
			} else {
					# DESC
					vecs[,i] = -df[,idx];
					}
		}
	# df[order( vecs[,1],vecs[,2],vecs[,3] ), ]; # hacked
	df[callOrderFunctionWithMatrixInput(vecs),]; # Thanks Allan
	}

