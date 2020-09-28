


createDirRecursive = function(folder, verbose=TRUE, skip=1)
  {
  # # skip is the level of subfolders to not createDir,
  # e.g., $skip=1 on Windows will not do createDir("C:\");
  # skip must be 1 or greater ...
  temp = strsplit(folder,"/",fixed=T);
  stems = temp[[1]];
  ns = length(stems);

  #msgs = c();
  #paths = c();

  path = "";
  for(i in 1:skip)
    {
    path = paste0(path,stems[i],"/");
    }
  for(j in (i+1):ns)
    {
    path = paste0(path,stems[j],"/");
    #print(path);
    msg = createDir(path, verbose=verbose);
    #print(msg);

    #msgs = c(msgs,msg);
    #paths = c(paths,path);
    }
  if(verbose)
    {
    #out = paste0("createDirRecursive for \n\t\t\t",folder,"\n");
    #out = paste0(paths);
    out = folder;
    print(out);
    }
  }

# this is "not" recursive at the moment ...
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


writeLine = function(str, file=file, append=TRUE, end="\n")
  {
  # wrapper for cat
  cat( paste(str,end,sep=""), file=file, sep="", append=append );
}


storeToFile = function (str,myfile)
	{
	cat(str, file=myfile,append=FALSE);
	}


grabHTML = function(htmlfile,htmlurl,return.raw=TRUE)
  {
  if(exists("local.data.path")) # is this available from the global scope?
		{
    if(file.exists(htmlfile))
      {
      print("grabHTML() ... from cache ...");
      print(htmlfile);
      if(return.raw)
        {
        #rawHTML = readtext::readtext(htmlfile);
        rawHTML = readChar(htmlfile, nchars=9724129);  # piping string ...
        return (rawHTML);
        } else { return (TRUE); }

      }
    }
  print("grabHTML() ... from RCurl ...");
  rawHTML = RCurl::getURL( htmlurl );
  if(exists("local.data.path")) # is this available from the global scope?
		{
	  storeToFile(rawHTML,htmlfile);
    print("Grabbed from RCurl ... stored");
    } else {
            if(return.raw)
              {
              rawHTML;
              } else { print("Grabbed from RCurl, now what?"); }
            }
  }


numberPadLeft = function(n, w, c="0")
  {
  str_pad(n,w,"left",c);
  }
