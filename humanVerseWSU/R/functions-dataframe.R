## TODO
#     trim(col) before searching for it ... maybe extra white space ...


#' subsetDataFrame
#'
#' @family DataFrame
#'
#' @param df dataframe
#' @param mycols names of cols to find idx's ... string or vector of strings will work
#'
#' @param myvals values for the cols to subset
#' @param verbose if TRUE, will print messages regarding subsetting
#' @param comparison by default "==" equality comparison, LHS is df and RHS is myval
#'
#' @return NA if not correctly specified; otherwise dataframe of subset
#' @export
#'
#' @examples
#' library(datasets);
#' data(iris);
#' head(iris);
#' mycols = c("Sepal.Width","Petal.Width");
#' myvals = c(3.0, 0.2);
#' subsetDataFrame(iris, mycols, "==", myvals);
#' subsetDataFrame(iris, mycols, "==", myvals, verbose=TRUE);
#'
#' comparison = c(">=", "<=");
#' subsetDataFrame(iris, mycols, comparison=comparison, myvals, verbose=TRUE);
#'
#' myvals = c(1492, 1991);
#' subsetDataFrame(iris, mycols, "==", myvals);  # not found, returns zero rows ...
#'
#' dim( subsetDataFrame(iris,"Petal.Length", "==", 1.4) );
#' dim( subsetDataFrame(iris,"Petal.Length", "~=", 1.4) );
#'
#' df = iris;
#' df$sinpi = sin(pi);
#' dim( subsetDataFrame(df,"sinpi", "==", 0) );
#' dim( subsetDataFrame(df,"sinpi", "~=", 0) );  # addresses floating point issues with isClose
#'
#'
#' subsetDataFrame(iris,"column-does-not-exist", "==", 123);  # Throws warning, returns NA
#'
subsetDataFrame = function(df, mycols=mycols, comparison="==", myvals=myvals, verbose=FALSE)
  {
  # also not logic ....
  # currently this is AND logic
  # how to also make it possible or logic ... one long or statement ...
  n = nrow(df);
  n.cols = length(mycols);
  n.vals = length(myvals);
  if(n.cols != n.vals)  # if n.vals = 1, can I not just do a rep(myvals, times=n.cols)...
    {
    warning("Something wrong in subsetDataFrame ... mycols and myvals are of different lengths");
    return (NA);
    }

## a comparison per field
  n.com = length(comparison);
  if(n.com == 1)
    {
    comparisons = rep(comparison, times=n.cols);
    } else {
            if(n.cols != n.com)
              {
              warning("Something wrong in subsetDataFrame ... mycols and comparison are of different lengths");
              return (NA);
              } else { comparisons = comparison; }
            }



  idxs = getIndexOfDataFrameColumns(df,mycols);
  if(anyNA(idxs))
    {
    warning("One or more columns of mycols is not found!");
    return (NA);  # example won't run if I put "stop" when devtools::check();
    }
  ndf = df;
  for(i in 1:n.cols)
    {


    my.comparison = trimMe(tolower(comparisons[i]));
    allowed = c("==", "=", "~=", "=~", ">", ">=", "=>", "<", "<=", "=<");
    if(!is.element(my.comparison, allowed))
      {
      warning( paste0( "Comparison:    ", my.comparison, "     NOT FOUND!") );
      print("Replacing with the default comparison:           ==      ");
      my.comparison = "==";
      }

    if(verbose)
      {
      print( paste0( "Subsetting column [",mycols[i],"] ", my.comparison ," [",myvals[i],"]" ) );
      }




    ndf = switch(my.comparison,
          "=="    = ndf[ndf[,idxs[i]] == myvals[i], ],
          "="     = ndf[ndf[,idxs[i]] == myvals[i], ], # bad form, but will work

          "~="    = ndf[isClose(ndf[,idxs[i]] , myvals[i]), ], # maybe jaro-winkler if a string
          "=~"    = ndf[isClose(ndf[,idxs[i]] , myvals[i]), ], # approximate


          ">"     = ndf[ndf[,idxs[i]] > myvals[i], ],
          ">="    = ndf[ndf[,idxs[i]] >= myvals[i], ],
          "=>"    = ndf[ndf[,idxs[i]] >= myvals[i], ], # bad form, but will work

          "<"     = ndf[ndf[,idxs[i]] < myvals[i], ],
          "<="    = ndf[ndf[,idxs[i]] <= myvals[i], ],
          "=<"    = ndf[ndf[,idxs[i]] <= myvals[i], ], # bad form, but will work

         ndf[ndf[,idxs[i]] == myvals[i], ] # default case of switch
        );
    #ndf = ndf[ndf[,idxs[i]] == myvals[i], ];
    }
  ndf;
  }



mergeDataFrames = function(df1,df2,merge.col,extra="")
  {
  # MORE TODO HERE
  # https://www.datasciencemadesimple.com/join-in-r-merge-in-r/
  # extra == "cross-join"



  }

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

#' removeAllColumnsBut
#'
#' Remove all columns in the data frame but (except for)
#'  those columns listed.
#'
#' Useful when you have a df with lots of columns.
#'
#' @param df dataframe
#' @param mycols names of cols to keep
#'
#' @return dataframe, updated
#' @export
#'
#' @aliases removeAllColumnsExcept
#'
#' @examples
#' library(datasets);
#' data(iris);
#' head(iris);
#' dim(iris);
#' ndf = removeAllColumnsBut(iris, c("Petal.Length","Petal.Width"));
#' head(ndf);
#' dim(ndf);
removeAllColumnsBut = function(df,mycols)
	{
  ndf = NULL;
  # mycols could be just a single string,
  # which is automatically treated as a vector of length 1.
	for(mycol in mycols)
		{
		ndf[mycol] = df[mycol];
		}
	as.data.frame(ndf);
  }


removeNAsFromDataFrame = function(df,mycols=NULL)
  {
  # MORE TODO HERE
  # if the given column is NA, we remove the row
  # this enables us to build a balanced df
  # if NULL, it will loop over all columsn and do a full cleanse by column ... very conservative approach
  # imdb ... has-millions?
  # nrows = dim(df)[1];
  # ncols = dim(df)[2];
  # https://stackoverflow.com/questions/4862178/remove-rows-with-all-or-some-nas-missing-values-in-data-frame
  # complete.cases?
  stats::na.omit(df);
}




#' removeDuplicatesFromDataFrameAllColumns
#'
#' Based on the current order of the dataframe, it will remove
#' duplicate values in ALL columns.
#'
#' @family DataFrame
#'
#' @param df dataframe
#' @return dataframe, updated
#' @export
#'
#' @examples
#' library(datasets);
#' data(iris);
#' head(iris);
#' dim(iris);
#' iris[c(102,143),]; # is this a duplicate or replicate?
#' #if a different plant, NO (generally)
#' #if an error in data collection (double-submission), YES
#'
#' df = removeDuplicatesFromDataFrameAllColumns(iris); # personality raw
#' head(df);
#' dim(df);
removeDuplicatesFromDataFrameAllColumns	= function(df)
	{
  nrows = dim(df)[1];
  ncols = dim(df)[2];

  df.str = c();
  for(i in 1:nrows)
    {
    row = df[i,];
    row.str = paste(as.character( unlist(row) ) ,collapse="-");
    df.str = c(df.str,row.str);
    }

  #duplicated(df.str);
  ndf = df[!duplicated(df.str), ];

  ## below was a truth table by cols with sum ...
#   truth.table = matrix(0, nrow=nrows, ncol=(ncols+2));
#     colnames(truth.table) = c( names(df), "Sum", "Result");
# 	# one column at a time
#   for(j in 1:ncols)
#     {
#     mycolumn = names(df)[j];
#     truth.table[,j] = duplicated(df[mycolumn]);
#     }
#   truth.table[,(ncols+1)] = rowSums(truth.table[,1:ncols]);
# 	#ndf = df[!duplicated(df[mycolumn]), ];
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
#' dim(iris);
#' df = removeDuplicatesFromDataFrame(iris,"Species");
#' head(df);
#' dim(df);
removeDuplicatesFromDataFrame	= function(df,mycolumn)
	{
	# one column at a time
	ndf = df[!duplicated(df[mycolumn]), ];
  }



#' removeDuplicatesFromDataFrameUnique
#'
#' Based on the current (sort) order of the dataframe, it will remove
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
#' dim(iris);
#'
#' df = removeDuplicatesFromDataFrameUnique(iris,"Species");
#' head(df);
#' dim(df);
#'
#' df = removeDuplicatesFromDataFrameUnique(iris,"Petal.Width");
#' head(df);
#' dim(df);
removeDuplicatesFromDataFrameUnique = function(df, mycolumn)
  {
  ncols = dim(df)[2];
  ndf = as.data.frame( matrix(NA, nrow=0,ncol=ncols) );
    colnames(ndf) = colnames(df); # empty new data frame

  # could be factors
  u = unique( df[,mycolumn]); # str(u); # class(u);
  nu = length(u);
  for(i in 1:nu)
    {
    mykey = u[i];
    # get all rows with unique key
    rows = df[df[,mycolumn]==mykey,];
    row = rows[1,]; # first row
    ndf = rbind(ndf,row);
    }
  ndf;
  }


#' getIndexOfDataFrameRows
#'
#' @family DataFrame
#'
#' @param df dataframe
#' @param mycols names of cols to find idx's ... string or vector of strings will work
#'
#' @param myvals values for the cols (like subsetting, but just getting row indexes)
#'
#' @return NA if not found; otherwise numeric vector of the same length as mycols, in the same order as mycols
#' @export
#'
#' @examples
#' library(datasets);
#' data(iris);
#' head(iris);
#' mycols = c("Sepal.Width","Petal.Width");
#' myvals = c(3.0, 0.2);
#' getIndexOfDataFrameRows(iris,mycols,myvals);
#'
#' myvals = c(1492, 1991);
#' getIndexOfDataFrameRows(iris,mycols,myvals);
#'
#'
#' getIndexOfDataFrameRows(iris,"column-does-not-exist", 123);  # Throws error
#'
getIndexOfDataFrameRows = function(df,mycols,myvals)
	{
  n = nrow(df);
  n.cols = length(mycols);
  n.vals = length(myvals);
  if(n.cols != n.vals)
    {
    warning("Something wrong in getIndexOfDataFrameRows ... mycols and myvals are of different lengths");
    return (NA);
    }

  idxs = getIndexOfDataFrameColumns(df,mycols);
  if(anyNA(idxs))
    {
    warning("One or more columns of mycols is not found!");
    return (NA);  # example won't run if I put "stop" when devtools::check();
    }

  truth = matrix(FALSE, ncol = n, nrow = n.cols);
  for(i in 1:length(mycols) )
    {
    truth[i, ] = df[,idxs[i] ] == myvals[i] ;
    }
  mySums = colSums(truth) == n.cols;

  result = which(mySums);
  if(length(result) == 0) { return (NA); }
  result;
  }

#' getIndexOfDataFrameColumns
#'
#' @family DataFrame
#'
#' @param df dataframe
#' @param mycols names of cols to find idx's ... string or vector of strings will work
#'
#'
#' @return NA if not found; otherwise numeric vector of the same length as mycols, in the same order as mycols
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
#' getIndexOfDataFrameColumns(iris,"column-does-not-exist");  # NA
#'
getIndexOfDataFrameColumns = function(df,mycols)
	{
	n.cols = length(mycols);
	result = numeric(n.cols);
	for(i in 1:n.cols)
		{
		mycol = mycols[i];
		count = sum( names(df)== mycol );
		if(count == 0)
		  {
		  result[i] = NA;
		  } else {
		          result[i] = which( names(df)== mycol );
		          }
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
#'
#' df = moveColumnsInDataFrame(iris,c("Sepal.Length","Sepal.Width"),"after","Species");
#' head(df);  # might be a bug if all are moved?
#'
#' head(iris[,1:2]);
#' df = moveColumnsInDataFrame(iris[,1:2],"Sepal.Width","before","Sepal.Length");
#' head(df);
#'
#' head(iris[,1:2]);
#' df = moveColumnsInDataFrame(iris[,1:2],"Sepal.Length","after","Sepal.Width");
#' head(df);
#'
#' head(iris[,1:2]);
#' df = moveColumnsInDataFrame(iris[,1:2],"Sepal.Width","after","Sepal.Length");
#' head(df);
moveColumnsInDataFrame = function(ndf, mycols, where, anchor)
	{
	# anchor is a colname by default, but can be an index (is numeric)
	# mycols are names of the cols ... we will get their locations ...
	# where can be "before" or "after" the anchor
  # some of mycols can be before/after anchor to begin with, doesn't matter ...
  ncols = ncol(ndf);

	anchor.idx = anchor;
	if(!is.numeric(anchor)) { anchor.idx = getIndexOfDataFrameColumns(ndf, anchor); }
  if(is.na(anchor.idx))
    {
    warning("anchor not found!");
    return (ndf);
    }

	if(where == "after" && ncols == 2 && anchor.idx == 2)
	  {
	  return ( ndf[, c(2,1)]);
	  }
	if(where == "after" && ncols == 2 && anchor.idx == 1)
	  {
	  return ( ndf[, c(1,2)]); # no change
	  }

  ## above was simple case that was breaking ...
	## are there others ...

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
	  # set notation
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
#' @examples
#' date.strings = c("3/24/2010 18:33", "9/3/2009 17:28", "10/14/2009 11:40",
#'                  "7/3/2015 11:16", "11/18/2010 1:29", "4/23/2011 0:08",
#'                  "10/6/2010 11:13", "7/26/2009 13:23","4/9/2008 13:40",
#'                  "8/20/2008 11:32");
#'
#' library(datasets);
#' data(iris);
#' df = iris[1:10,];
#' df$date.strings = date.strings;
#' df;
#' df = moveColumnsInDataFrame(df, "date.strings", "after", "Sepal.Width");
#' df;
#'
#'
#'
#' ywd = convertDateStringToFormat( date.strings,
#'                              c("%Y","%W","%j"), c("year","week","day"),
#'                                                            "%m/%d/%Y %H:%M");
#'
#' udf = replaceDateStringWithDateColumns(df,"date.strings",ywd);
#'
#'
replaceDateStringWithDateColumns = function(df, mycolumn, newcols)
		{
    # we have the anchor ...
		date.idx = getIndexOfDataFrameColumns(df,mycolumn);
			ndf = cbind(df,newcols);
			  # they must already be named ...
				mycolnames = colnames(newcols);
		ndf = moveColumnsInDataFrame(ndf, mycolnames, "after", date.idx);
		ndf = removeColumnsFromDataFrame(ndf,mycolumn);
		ndf;
		}











#' sortDataFrameByNumericColumns
#'
#' Does it have to be numeric?
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
#'
#' library(datasets);
#' data(iris);
#' df = iris[1:10,];
#' sortDataFrameByNumericColumns(df,"Petal.Length","ASC");
#' sortDataFrameByNumericColumns(df,"Petal.Length","DESC");
#' sortDataFrameByNumericColumns(df, c("Petal.Length","Petal.Width") , "DESC");
#' sortDataFrameByNumericColumns(df, c("Petal.Length","Petal.Width") , c("ASC","DESC"));
#'
#'
#' date.strings = c("3/24/2010 18:33", "9/3/2009 17:28", "10/14/2009 11:40",
#'                  "7/3/2015 11:16", "11/18/2010 1:29", "4/23/2011 0:08",
#'                  "10/6/2010 11:13", "7/26/2009 13:23","4/9/2008 13:40",
#'                  "8/20/2008 11:32");
#'
#' df$date.strings = date.strings;
#' df;
#' df = moveColumnsInDataFrame(df, "date.strings", "after", "Sepal.Width");
#' df;
#'
#'
#'
#' ywd = convertDateStringToFormat( date.strings,
#'                              c("%Y","%W","%j"), c("year","week","day"),
#'                                                            "%m/%d/%Y %H:%M");
#'
#' udf = replaceDateStringWithDateColumns(df,"date.strings",ywd);
#'
#'     mycols = c("year","week", "day");
#' sortDataFrameByNumericColumns(udf,mycols,"ASC");
#' sortDataFrameByNumericColumns(udf,mycols,"DESC");
#'
#'     mydirections = c("ASC","DESC","ASC");
#' sortDataFrameByNumericColumns(udf, mycols, mydirections );
#'
#' sortDataFrameByNumericColumns(udf,sample(mycols),sample(mydirections) );
#'
#'
sortDataFrameByNumericColumns = function (df, mycols, direction="DESC")
	{
	# if direction.length is not n.cols, we will only use the first element ...
  # sort.data.frame(dd,by = ~ -z + b)
  # if the df only has one dimension, doesn't work as expected "membership"
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
	# function callOrderFunctionWithMatrixInput is in functions-sort.R
	# df[order( vecs[,1],vecs[,2],vecs[,3] ), ]; # hacked
	df[callOrderFunctionWithMatrixInput(vecs),]; # Thanks Allan
	}





##############################


















#' replaceFactorColumnWithIndicatorVariables
#'
#' I have a column with factors or string values, and I create
#'  indicator variables to binary-ize those factors
#'
#' @param df Data frame
#'
#' @param source.column Where the factors live, it could be exhaustive
#' or not the function deals with that.
#'
#' @param sep If multiple factors are in the one column, what is the separator.
#' Useful as this will call getKeysFromStringWithSeparator (which maybe
#'  should live in functions-str.R)
#'
#' @param new.column The new column name will have `.i` appended to it for each
#'  indicator variable.
#'
#' @param use.boolean If TRUE, TRUE/FALSE otherwise 1/0.
#'
#' @param remove.original If TRUE, original column is removed
#'
#' @param lower.case If TRUE, indicator keys are converted to lowercase.
#'
#' @return
#' @export
#'
#' @examples
#' library(datasets);
#' data(iris);
#' head(iris);
#' dim(iris);
#'
#' ndf = replaceFactorColumnWithIndicatorVariables(iris, "Species", "is");
#' head(ndf);
#' dim(ndf);
#'
#' ndf = replaceFactorColumnWithIndicatorVariables(iris, "Species", "is", FALSE);
#' head(ndf);
#' dim(ndf);
replaceFactorColumnWithIndicatorVariables = function(df, source.column,
                    new.column=source.column, use.boolean=TRUE, sep=",",
                     remove.original = FALSE, lower.case=TRUE)
    {

    # "genre" becomes genre_comedy as indicator ...
    # grab all unique indicators, alphabetize, and add to dataframe
    sidx = getIndexOfDataFrameColumns(df, source.column);
    u.df = unlist( unique(df[source.column]) );
    u.keys = c();
    u.df.length = length(u.df);

    my.false = 0;
    my.true = 1;

    if(use.boolean) { my.false = FALSE; }
    if(use.boolean) { my.true = TRUE; }

    # get all possible keys
    for(i in 1: u.df.length)
      {
      u.row = as.character(u.df[i]);
      u.vals = getKeysFromStringWithSeparator(u.row,sep=sep,lower.case=lower.case);
      u.keys = c(u.keys,u.vals);
      }

    # can I force a numeric sort?
    # getting delta.1, delta.10
    # want delta.1, delta.2 ... delta.10
    mykeys = sort( stats::na.omit( unique(u.keys) ) );  # 26 keys
    mycols = c();
    for(mykey in mykeys)
      {
      mycol = paste0(new.column,".",mykey);
      mycols = c(mycols,mycol);
      df[mycol] = my.false;
      }
    # loop over dataframe adding values
    df.n = dim(df)[1];
    for(i in 1:df.n)
      {
      #print(i); flush.console();
      r.val = df[i,sidx];
      u.vals = getKeysFromStringWithSeparator(r.val,sep=sep,lower.case=lower.case);
      for(u.val in u.vals)
        {
        if(!is.na(u.val))
          {
          mycol = paste0(new.column,".",u.val);
          cidx = getIndexOfDataFrameColumns(df, mycol);
          df[i,cidx] = my.true;
          }
        }
      }

    df = moveColumnsInDataFrame(df, mycols, "after", source.column);

    if(remove.original)
      {
      df = removeColumnsFromDataFrame(df, source.column);
      }
    df;
    }


#updateDataFrameWithUniqueNewElementsIndicated = function(row,  stack.gem$movies, "source.gem");
updateDataFrameWithUniqueNewElementsIndicated = function(df.existing, mycolumn, df.new, indicator, replace=TRUE)
  {
  # TODO ... this is a unique case where you want to merge
  # different sourced dataframes that are identical but
  # add indicator variables to annotate the source ...

  # assume elements in df.existing are unique
  ndf = df.existing;
      n.existing = dim(ndf)[1];
  # mycolumn = "ttid";
  toadd = df.new;
      n.toadd = dim(toadd)[1];

      # indicator = "source.gem";
  cidx = getIndexOfDataFrameColumns(ndf, indicator);
  # update the key of the indictor to true for that element [DEFAULT]
  # or update to another value, such as "rank" of toadd dataframe ..
  replace.cidx = NULL;
  if(!isTRUE(replace))
    {
    replace.cidx = getIndexOfDataFrameColumns(toadd, replace);
    }

  for(i in 1:n.toadd)
    {
    r = toadd[i,];
    k = as.character(r[mycolumn]);  # tt0270321

    idx = findAllIndexesWithValueInVector( as.vector(unlist(ndf[mycolumn])), k);

    #print( paste("i= ",i, " idx info") );     print(idx);

    idx.length = length(idx); # this should be zero or one ...
    if(idx.length == 0)
      {
      # not found, let's just add the row to the dataframe, as-is
      ndf = rbind(ndf,r);
      #print( paste0("key not found, added: ",k) );
      }
      else
        {
        for(j in 1:idx.length) # should be one
          {
          myidx = idx[j];


          ndf[myidx,cidx] = if(is.null(replace.cidx)) { TRUE; } else { toadd[i,replace.cidx]; }


         # print( paste0("key found at myidx: ",myidx," true to cidx: ",cidx) );
          }
        }

    }
  ndf;

  }



