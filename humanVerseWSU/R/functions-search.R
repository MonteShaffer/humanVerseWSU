#' wildcardSearch
#'
#' By default searches a dataframe string column based on a
#'  wildcard `*` operator.
#'
#' Can also just search a single character vector.
#'
#' @param str Search string with basic wildcard `*` operator
#' @param col.name The column to perform search on [or a character vector]
#' @param return.cols list of columns you want to display, by default all
#'
#' @param ignore.case Defaults to TRUE, matches "mont" and "Mont"
#' @param perl Defaults to FALSE, convert string query to PERL regex?
#' @param df Dataframe to search [column]

#'
#' @return dataframe of results, empty dataframe is possible
#' @export
wildcardSearch = function(str, col.name, return.cols=NULL, ignore.case=TRUE, perl=FALSE, df=NULL)
	{
  # if is.null(df), we are just searching a character vector ...
  # else, we are using the df[col] to do the search ...
    # https://stackoverflow.com/questions/5823503/
  if(is.null(df))
    {
    # col.name is a character vector
    grx = utils::glob2rx(str);
    grx.grep = grep(grx, col.name, ignore.case=ignore.case, perl=perl);
    rows = col.name[grx.grep];
    if(length(rows) == 0) { return (NA); }  # empty set ... from vector
    return (rows);
    } else
          {
          grx = utils::glob2rx(str);
          # just one column for now
          cidx = getIndexOfDataFrameColumns(df, col.name);
          if(is.na(cidx)) { cidx = -1; }
          # -1 will return an empty-set on the grep call
          grx.grep = grep(grx, df[,cidx], ignore.case=ignore.case, perl=perl);
          rows = df[grx.grep, ];
          if(is.null(return.cols)) { rows; } else { rows[, return.cols]; }
          }
  }
