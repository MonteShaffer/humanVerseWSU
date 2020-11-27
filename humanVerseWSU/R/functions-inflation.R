

#' loadInflationData
#'
#' This data was pulled from
#' \url{https://www.officialdata.org/us/inflation/2000?endYear=1920&amount=1000000}
#' in September 2020.
#'
#' It contains dollar information from 1920 to 2020, 101 observations.
#'
#' Likely with throw an error if you specify a value not in that range.
#'
#' @family Inflation
#'
#'   inflation.df global gets assigned to the inflation.rds (equivalent to
#'   inflation.txt)
#' @export
#' @aliases loadDataInflation

loadInflationData = function()
  {
  idf = readRDS( system.file("extdata", "inflation.rds", package="humanVerseWSU") );
  #assign("inflation.df",idf);
  .GlobalEnv$inflation.df = idf;
  }


#' adjustDollarForInflation
#'
#' @family Inflation
#'
#' @param mydollar current dollar value
#' @param myyear current year value (4-digit format, 1920 to 2020)
#' @param newyear new year value (4-digit format, 1920 to 2020)
#' @param idf inflation data frame
#'
#' @return dollar, updated (adjusted from myyear to newyear)
#' @export
#' @examples
#' loadInflationData();
#' adjustDollarForInflation( 123, 1943, 2000 ); # $123 in 1943 is about $1224.31 in 2000
#' adjustDollarForInflation( 123, 2000, 1943 ); # $123 in 2000 is about $12.36 in 1943
#'
adjustDollarForInflation = function(mydollar,myyear,newyear,idf=inflation.df)
	{
	# use basic ratio
	dollar.myyear = lookupInflationDollar(myyear,idf);
	dollar.newyear = lookupInflationDollar(newyear,idf);

	ratio = dollar.newyear/dollar.myyear;

	mydollar*ratio;
	}





#' standardizeDollarsInDataFrame
#'
#' @family Inflation
#'
#' @param df dataframe containing dollar.source and year.source
#' @param anchor.year base year to convert all dollars to
#' @param dollar.source column name (in df) with raw dollars
#' @param year.source column name (in df) with 4-digit years (1920 - 2020)
#' @param dollar.out new column name (in df) to be created
#' @param idf inflation data frame
#'
#' @return dataframe, updated
#' @export
#' @examples
#' loadInflationData();
#' # todo once I get Will/Denzel data ...
standardizeDollarsInDataFrame = function(df, anchor.year, dollar.source, year.source, dollar.out, idf=inflation.df)
	{
	dollars = as.numeric( unlist(df[dollar.source]) ); # we assume this is numeric ...
	years = as.numeric( unlist(df[year.source]) ); # I could do unique on years, to speed this up slightly

	# not efficient #

	dollars.n = length(dollars);

	newdollars = numeric(dollars.n);

	for(i in 1:dollars.n)
		{
		mydollar = dollars[i];
		myyear = years[i];
		nd = NA;
		if(!is.na(mydollar)) { nd = adjustDollarForInflation(mydollar,myyear,anchor.year,idf=inflation.df); }

		newdollars[i] = nd;
		}

	df[dollar.out] = newdollars;

	df;
	}


#' lookupInflationDollar
#'
#' @family Inflation
#'
#' @param year numeric, 4-digit year   (1920 - 2020)
#' @param idf inflation data frame
#'
#' @return numeric dollar from idf table for that year
#' @export
#' @examples
#' loadInflationData();  # does lookup to create ratios
#' lookupInflationDollar( 1943 ); # $  865,000
#' lookupInflationDollar( 2000 ); # $8,610,000
#'
lookupInflationDollar = function(year,idf=inflation.df)
	{
	idf[idf$year==year,2];  # single form ...
	}






#' grabInflationURL
#'
#'
#' @family Inflation-Raw
#'
#' @return inflation.url
#' @export
#'
grabInflationURL = function()
  {
  return( "https://www.officialdata.org/us/inflation/2000?endYear=1920&amount=1000000" );
  }


#' grabInflationDataFrame
#'
#' This has caching mechanisms in the local.path environment.
#'
#' local.path must be defined in the global scope for this function to use it
#'
#' local.data.path must be defined in the global scope for this function to use it
#'
#' Every month it will lookup the values again.
#'
#' @family Inflation-Raw
#'
#' @return inflation data frame (idf)
#' @export
#' @examples
#' inflation.df = grabInflationDataFrame();
grabInflationDataFrame = function()
	{
  inflation.url = grabInflationURL();
	# let's return the table if we have it locally.
	myfile = NULL;
	if(exists("local.path"))
		{
		# we need these if not already called ...
		source( paste0(local.path,"functions/functions-file.R"), local=TRUE );

		cached = paste0(local.path,"datasets");
			createDir(cached);
		cached = paste0(cached,"/inflation");
			createDir(cached);

		myfile = paste0(cached,"/","recent.txt");

		# if it is a new month, should we grab that?
		}


	if(exists("local.data.path")) # is this available from the global scope?
		{
			createDir(local.data.path);
		mypath = paste0(local.data.path,"modules");
			createDir(mypath);
		mypath = paste0(mypath,"/inflation");
			createDir(mypath);

		# let's update every month ...
		mycache = format(Sys.Date(), "%Y-%m");

		mytxt = paste0(mypath,"/",mycache,".txt");

		if(file.exists(mytxt))
			{
			# we have the most recent month in the data.cache, so the "recent.txt" is up-to date.
			return ( utils::read.csv(myfile, header=TRUE, sep="|") );
			} else 	{
					htmlfile = gsub(".txt",".html", mytxt, fixed=TRUE);


					# this can be a generic function ...
					rawHTML = RCurl::getURL( inflation.url );

					storeToFile(rawHTML,htmlfile);

					return ( parseStoreReturnInflationData(rawHTML,myfile,mytxt) );
					}
		} else 	{
				# let's check and see if "recent.txt" is in the github source ...


				if(exists("github.path"))
					{
					remote = paste0(github.path,"datasets");
					remote = paste0(remote,"/inflation");
					remotefile = paste0(remote,"/","recent.txt");

					return ( utils::read.csv(remotefile, header=TRUE, sep="|") );

					} else	{
							return ( parseStoreReturnInflationData(inflation.url,myfile) );
							}
				}
	}


#' parseStoreReturnInflationData
#'
#' @family Inflation-Raw
#'
#' @param raw HTML string harvested from URL
#' @param myfile recent.txt
#' @param mytxt YYYY-MM.txt as we update lookup every month (if running locally)
#'
#' @return inflation data frame (idf)
#' @export
parseStoreReturnInflationData = function (raw,myfile=NULL,mytxt=NULL)
	{
	# this will parse, and possible also cache ...

	rvest.html = xml2::read_html(raw);
	rvest.data = parseInflationTableHTML(rvest.html);

	if(!is.null(myfile)) # both are available ...
		{
		utils::write.table(rvest.data, file=myfile, quote=FALSE, col.names=TRUE, row.names=FALSE, sep="|");
		utils::write.table(rvest.data, file=mytxt, quote=FALSE, col.names=TRUE, row.names=FALSE, sep="|");
		}
	rvest.data; # return to a return
	}

#' parseInflationTableHTML
#'
#' @family Inflation-Raw
#'
#' @param infl.html \code{rvest} object of HTML with inflation data
#'
#' @return inflation data frame (idf)
#' @export
#' @importFrom magrittr %>%
parseInflationTableHTML = function (infl.html)
	{
  # @importFrom rlang .data  ... this allows the piping (chaining to work)
	# ... there is a faster table parsing component in rvest ...
	infl.table = infl.html %>%
		rvest::html_node(".expand-table-parent") %>%
		rvest::html_node(".table-striped") %>%
		rvest::html_node("tbody") %>%
		rvest::html_nodes("tr");

	result = data.frame( matrix(nrow=length(infl.table), ncol=3));
		colnames(result) = c("year","dollar","inflation");

		for(i in 1:length(infl.table) )
			{
			infl.row = infl.table[i]	%>%
			  rvest::html_nodes("td") %>%
			  rvest::html_text();

			year = as.numeric(infl.row[1]);
				temp = gsub('$','',infl.row[2],fixed=T);
				temp = gsub(',','',temp,fixed=T);
			dollar = as.numeric(temp);
				temp = gsub('%','',infl.row[3],fixed=T);
			inflation = as.numeric(temp);

			result$year[i] = year;
			result$dollar[i] = dollar;
			result$inflation[i] = inflation;
			}
	result;
	}

