cleanupHTMLentities = function(str, find="fs", replace="rs") 
  {
  fs = c("&lsquo;", "&rsquo;", "&mdash;");  # mdash is not a hyphen.
  rs = c("'"      , "'"      , " -- ");       # ASCII replaces
  cs = c("^[^"    , "^]^"    , " ^-^ ");      # custom replaces   
  nfs = length(fs);
  for(i in 1:nfs)
    {
    myf = eval(parse(text = find));
    myr = eval(parse(text = replace));
    
    str = gsub(myf[i],myr[i], str, fixed=TRUE);
    }
  str;
  }


strip_tags <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

