
# this is "not" recursive at the moment ...
createDir = function (folder)
  {
  ifelse(!dir.exists(folder), dir.create(folder), "Folder exists already");
  }

# logging functions ... functions-log.R


storeToFile = function (str,myfile)
	{
	cat(str, file=myfile,append=F);	
	}
	
	
