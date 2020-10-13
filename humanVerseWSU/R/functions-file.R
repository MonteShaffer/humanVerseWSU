

#' loadRDS
#'
#' loads an internal dataset by merely passing the string
#'
#' @param str string as the stem of the filename.rds
#'
#' @return dataframe if the dataset exists
#' @export
#'
#' @examples
#' personality = loadRDS("personality-raw");
#' inflation = loadRDS("inflation");
#' # cars = loadRDS("cars");
#' # protein = loadRDS("protein");
loadRDS = function(str)
  {
  df = readRDS( system.file("extdata", paste0(str,".rds"), package="humanVerseWSU") );
  df;
  }

#' createDirRecursive
#'
#' dir.create has a recursive option, so you may want to use that
#' THIS version was written years ago in another C language and ported to R.
#'
#' @param folder the folder to be created
#' @param verbose if true details will be printed
#' @param skip how many base levels to skip, we can't createDir("C:\")
#'
#' @return
#' @export
#'
#' @examples
#' createDirRecursive("R:/monte/says/hi/", verbose=TRUE);
#' createDirRecursive("R:/monte/says/hi/", verbose=TRUE);
createDirRecursive = function(folder, verbose=FALSE, skip=1)
  {
  # # skip is the level of subfolders to not createDir,
  # e.g., $skip=1 on Windows will not do createDir("C:\");
  # skip must be 1 or greater ...
  temp = base::strsplit(folder,"/",fixed=T);
  stems = temp[[1]];
  ns = length(stems);

  if(skip < 1) { stop("skip must be 1 or greater ..."); }

  path = "";
  msg = c();
  for(i in 1:skip)
    {
    path = paste0(path,stems[i],"/");
    }
  for(j in (i+1):ns)
    {
    path = paste0(path,stems[j],"/");
    msg = c(msg, createDir(path, verbose=verbose));
    }
  if(verbose)
    {
    out = folder;
    print( paste0("Creating folder: ",out) );
    print( msg );
    }
  }

#' createDir
#'
#' dir.create has a recursive option, so you may want to use that
#' THIS version was written years ago in another C language and ported to R.
#'
#' @param folder the folder to be created
#' @param verbose if true details will be printed
#'
#' @return
#' @export
#'
#' @examples
#' createDir("R:/monte/says/hi/again/", verbose=TRUE);
#' createDir("R:/monte/says/hi/again/", verbose=TRUE);
createDir = function (folder,verbose=TRUE)
  {
  if(verbose)
      {
      msg =paste0("Folder [ ",folder," ] exists already");
      } else {
              msg = "EXISTS";
              }
  ifelse(!dir.exists(folder), dir.create(folder), msg);
  }

# logging functions ... functions-log.R


#' writeLine
#'
#' This function writes a single character string to a file.
#' Very useful for simulations and building data one line at a time.
#'
#' @param str The character string to be written
#' @param append If TRUE, will append to the end of the file, otherwise it will overwrite an existing file
#' @param end EOL character to finish the line; the line separator
#' @param encoding by default UTF-8 ... Windows is ANSI_X3.4-1986 or RStudio default is ISO-8859-1
#' @param file The file to store the (str) line
#'
#' @return
#' @export
#'
#' @examples
#' writeLine("hello there", file="R:/monte/says/hi/again/my.log", append=FALSE);
#' writeLine("hi again", file="R:/monte/says/hi/again/my.log");
writeLine = function(str, file, append=TRUE, end="\n", encoding="UTF-8")
  {
  # wrapper for cat
  # cat( paste(str,end,sep=""),
  #     file=file(file, encoding = encoding),
  #       sep="", append=append );

  cat( paste(str,end,sep=""),
      file=file,
        sep="", append=append );
  }


#' storeToFile
#'
#' Store a string to a file (e.g., an HTML page downloaded).
#'
#' @param str The string to store
#' @param file The file to store the string (it will override).
#'
#' @return
#' @export
storeToFile = function (str, file, encoding="UTF-8")
	{
	#cat(str, file=file(file, encoding = encoding) ,append=FALSE);
  cat(str, file=file, append=FALSE);
	}


#' grabHTML
#'
#' This grabs an HTML file (or any web TXT file) and caches locally.
#' There needs to be a global variable `local.data.path` assigned so
#' this function understands it is running in a local environment where
#' it can actually save files.  As opposed to `github.data.path` I can't
#' source this file in a remote repository if I can't store files there.
#'
#' @param htmlfile If cached, we can return this file.  If not, we can store
#'  the url contents to a file.  (rvest doesn't collect the original data
#'  source to possibly do offline re-parsing.  bad data provenance.)
#'
#' @param htmlurl Using RCurl, grab the raw contents off the web.
#'
#' @param return.raw If TRUE, will return the raw string
#' @param verbose If TRUE, will provide some verbosity
#' @param encoding by default UTF-8 ... Windows is ANSI_X3.4-1986 or RStudio default is ISO-8859-1
#'
#' @return
#' @export
grabHTML = function(htmlfile, htmlurl, verbose=TRUE, return.raw=TRUE, encoding="UTF-8")
  {
  if( file.exists(htmlfile) )
      {
      if(verbose)
        {
        print( paste0("grabHTML() ... from cache ... ", htmlfile) );
        }
      if(return.raw)
        {
        raw.html = base::readChar( file(htmlfile, encoding=encoding), nchars=9724129);
        # raw.html = base::readLines( file(htmlfile, encoding=encoding) );
        return(raw.html);
        }
      } else  {  # file does not exist
              if( exists("local.data.path") ) # is this available from the global scope?
		            {
                # r = GET( "https://en.wikipedia.org/wiki/Ober%C3%A1" );
                r = httr::GET( htmlurl );
                if(r$status_code == 200)
                  {
                  raw.binary = httr::content(r, "raw");
                  url.encoding = stringi::stri_enc_detect(raw.binary)[[1]]$Encoding[1];
                  raw.html = httr::content(r, "text", encoding = url.encoding);
                  #raw.binary = httr::content(r, "raw");
                  #writeBin(raw.binary, file(htmlfile, encoding=encoding) );
                  storeToFile(raw.html, htmlfile, encoding=encoding);
                  if(return.raw)
                    {
                    return(raw.html);
                    }
                  }
                return( httr::http_status(r) );
                #encoding.guess = stringi::stri_enc_detect(content(r, "raw"))[[1]]$Encoding[1];
                # r$status_code
                # storeToFile
                # utils::download.file( url(htmlurl, encoding=encoding), htmlfile, method="curl");
                } else  {
                        #raw.html = curl::curl( htmlurl );
                        r = httr::GET( htmlurl );
                        if(r$status_code == 200)
                          {
                          raw.binary = httr::content(r, "raw");
                          url.encoding = stringi::stri_enc_detect(raw.binary)[[1]]$Encoding[1];
                          raw.html = httr::content(r, "text", encoding = url.encoding);
                          #raw.binary = httr::content(r, "raw");
                          #writeBin(raw.binary, file(htmlfile, encoding=encoding) );
                          return(raw.html);
                          }
                        return( httr::http_status(r) );
                        }
              }

  # rawHTML = RCurl::getURL( htmlurl );
  # rawHTML = RCurl::getURLContent ( htmlurl );
  # rawHTML = curl::curl( htmlurl );
  # storeToFile(rawHTML,htmlfile);

  }





# read triangular correlation table ...
#' readTriangularCorrelationTable
#'
#' @param file file with triangular correlation table
#'
#' @return correlation matrix
#' @export
#'
#' @examples
#' d.cor   = readTriangularCorrelationTable("http://md5.mshaffer.com/WSU_STATS419/DRUG_USE.txt");
#' d.names = c( "Cigarette", "Beer", "Wine", "Liquor", "Cocaine",
#'              "Tranquilizer", "Medication", "Heroin", "Marijuana",
#'              "Hashish", "Inhalant", "Hallucinogen", "Amphetamine");
#' colnames(d.cor) = d.names;
#' rownames(d.cor) = d.names;
#' d.cor;
readTriangularCorrelationTable = function(file)
  {
  x = scan(file);
  lx = length(x);
  d = (sqrt(8*lx+1)-1)/2;
  m = matrix(0, d, d);
  m[upper.tri(m, T)] = x;
  m = m + t(m) - diag(diag(m));
  return(m);
  }
