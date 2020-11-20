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


cleanupAndPerformReadability = function(str)
{
 # Micro$oft 
  str = gsub("’","'",str,fixed=TRUE);
  str = gsub("”",'"',str,fixed=TRUE);
  str = gsub("“",'"',str,fixed=TRUE);
  periods = length(strsplit(str,".",fixed=TRUE)[[1]]);
  str = removePunctuationFromString(str);
  str = removeWhiteSpace(str);
  
  
  str.vec  = strsplit(str," ",fixed=TRUE)[[1]];


  str.s = countSyllablesInWord(str.vec);
  str.r = computeReadability(periods,str.s);  # one sentence

  
  
str.r;
}

strip_tags <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

prepStringGram = function(str, rm.pipe=FALSE, extra=NULL)
  {
  str = gsub("^[^","'",str,fixed=TRUE);
  str = gsub("^]^","'",str,fixed=TRUE);
  str = gsub("^-^"," | ",str,fixed=TRUE);
  str = gsub("-"," ",str,fixed=TRUE);  # should wood-cutter become wood cutter or woodcutter
  str = gsub("\r\r\n"," | ",str,fixed=TRUE);
  str = gsub("\r\n"," | ",str,fixed=TRUE);
  str = gsub("\n"," | ",str,fixed=TRUE);
  
  ne = length(extra);
  if(ne > 0)
    {
    for(i in 1:ne)
      {
      str = gsub(extra[i]," | ",str,fixed=TRUE);
      }
    }
  
  if(rm.pipe)
    {
    str = gsub("|"," ",str,fixed=TRUE);
    }
  str = removeWhiteSpace(str); 
  str;
  }
