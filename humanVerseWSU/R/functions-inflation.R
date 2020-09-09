# we can include libraries in a functions header
# duplicate calls to libraries won't break anything

#library(RCurl); # rvest may be using this, but rvest doesn't let me directly just save the .html file
				# readLines is another solution
				# https://www.programmingr.com/content/webscraping-using-readlines-and-rcurl/
				
library(rvest);
inflation.url = "https://www.officialdata.org/us/inflation/2000?endYear=1920&amount=1000000";

# we should get this from a govt source ...

library(RCurl);

# we will store the most recent file in the "code-base" on the C:\ drive 
# the other code/html will be stored in the R:\ data path ...


grabInflationTable = function()
	{
	# let's return the table if we have it locally.
	myfile = NULL;
	if(exists("local.path"))
		{
		# we need these if not already called ...
		source( paste0(local.path,"functions/functions-file.R"), local=T );
		
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
			return ( read.csv(myfile, header=T, sep="|") );
			} else 	{
					htmlfile = gsub(".txt",".html", mytxt, fixed=T);
					
					rawHTML = getURL(inflation.url); 
					
					storeToFile(rawHTML,htmlfile);
					
					return ( parseInflation(rawHTML,myfile,mytxt) );
					}
		} else 	{
				# let's check and see if "recent.txt" is in the github source ...
				
				
				if(exists("github.path"))
					{					
					remote = paste0(github.path,"datasets");
					remote = paste0(remote,"/inflation");				
					remotefile = paste0(remote,"/","recent.txt");	

					return ( read.csv(remotefile, header=T, sep="|") );

					} else	{				
							return ( parseInflation(inflation.url,myfile) );
							}
				}
	}
	
	
parseInflation = function (raw,myfile=NULL,mytxt=NULL)
	{
	# this will parse, and possible also cache ...
	
	rvest.html = read_html(raw);
	rvest.data = parseInflationTable(rvest.html);
	
	if(!is.null(myfile)) # both are available ...
		{
		write.table(rvest.data, file=myfile, col.names=T, row.names=F, sep="|");
		write.table(rvest.data, file=mytxt, col.names=T, row.names=F, sep="|");
		}
	rvest.data; # return to a return 
	}
	
parseInflationTable = function (infl.html)
	{	
	# ... there is a faster table parsing component in rvest ...
	infl.table = infl.html %>%
		html_node(".expand-table-parent") %>%
		html_node(".table-striped") %>%
		html_node("tbody") %>%
		html_nodes("tr");
		
	result = data.frame( matrix(nrow=length(infl.table), ncol=3));
		colnames(result) = c("year","dollar","inflation");

		for(i in 1:length(infl.table) )
			{
			infl.row = infl.table[i]	%>% 
				html_nodes("td") %>%
				html_text();
				
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

adjustForInflation = function(mydollar,myyear,newyear,inflation.data)
	{
	# use basic ratio
	dollar.myyear = lookupInflationDollar(myyear,inflation.data);
	dollar.newyear = lookupInflationDollar(newyear,inflation.data);
	
	ratio = dollar.newyear/dollar.myyear;
	
	mydollar*ratio;
	}
	
	



standardizeDollars = function(df, anchor.year, dollar.src, year.source, idf)
	{
	dollars = as.numeric( unlist(df[dollar.src]) ); # we assume this is numeric ...
	years = as.numeric( unlist(df[year.source]) ); # I could do unique on years, to speed this up slightly 
	
	# not efficient #
	
	dollars.n = length(dollars);
	
	newdollars = numeric(dollars.n);
	
	for(i in 1:dollars.n)
		{
		mydollar = dollars[i];
		myyear = years[i];
		
		newdollars[i] = adjustForInflation(mydollar,myyear,anchor.year,idf);
		}
		
	newdollars;
	}


	
adjustForInflation = function(mydollar,myyear,newyear,idf)
	{	
	dollar.myyear = lookupInflationDollar(myyear,idf);
	dollar.newyear = lookupInflationDollar(newyear,idf);
	
	ratio = dollar.newyear/dollar.myyear;
	
	mydollar*ratio;
	}
	
lookupInflationDollar = function(year,idf)
	{
	idf[idf$year==year,2];  # single form ...
	}
	




if(FALSE)
	{	
	# usage 	
	inflation.data = grabInflationTable();
	dollar.1921 = lookupInflationDollar(1921,inflation.data);
	dollar.2000 = lookupInflationDollar(2000,inflation.data);

	# $1 in 1921 is how many dollars in 2000?
	adjustForInflation(1,1921,2000,inflation.data);

	# can I write a data.frame vector form of this?  I think it has to be "apply" or a for loop ...

	will$movies.50$newmillions = standardizeDollars(will$movies.50, 2000, "millions", "year", inflation.data);


	# I could create a moveTo (df, colnames, "before/after", anchorcolname) ... # general data.frame object ...  moveTo(df, "year", "after", "dollar") ... after/before would be one single anchorcolnam but those being moved could be more than one column ...
	}