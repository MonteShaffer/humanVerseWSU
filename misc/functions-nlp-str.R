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
  
  
  str = gsub("’","'",str,fixed=TRUE);
  str = gsub("”",'"',str,fixed=TRUE);
  str = gsub("“",'"',str,fixed=TRUE);
  
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





cleanupTitles = function(strvec)
  {
  n = length(strvec);
  res = c();
  for(j in 1:n)
    {
    str = strvec[j];
    
    fi = c("-"," ",",");
    for(f in fi)
      {
      str = gsub(f,".",str,fixed=TRUE);
      }
    for(i in 1:5)
      {
      str = gsub("..",".",str,fixed=TRUE);  
      }
      #str;
    res = c(res,str);
    }
  res;
  }



prepareOneStory = function(df.grimm, path.to.grimm,
                          title, title.f,
                          my.stopwords = NULL
                          )
  {
  timer.start = as.numeric(Sys.time());
      df.story = subsetDataFrame(df.grimm, "title", "==", title);
        chap.n = df.story$chap.n[1];
      out.txt = paste0(path.to.grimm, title.f, ".txt");
      out.rds = paste0(path.to.grimm, title.f, ".rds"); # stats summary
  
  if(!file.exists(out.rds))
    {
      my.story = paste0(df.story$para.text, collapse=" \r\n ");
      if(!file.exists(out.txt))
        {
        writeLine(my.story, out.txt, append=FALSE);
        }
  
      words.r = getRawWords(my.story);
  
      timer.txt = as.numeric(Sys.time());
      elapsed = round( (timer.txt - timer.start), 2);
      print(paste0("story: ... ", title.f, " ... [txt] in ",elapsed," secs"));
      
      info.s = list();
        info.s$general = list();
        info.s$general$sentiment = doSentimentAnalysis(my.story); # expensive
        info.s$general$readability = as.data.frame(t(unlist(cleanupAndPerformReadability(my.story))));
        info.s$general$punctuation = countPunctuation(my.story);
        info.s$general$case = as.data.frame(t(unlist(countWordsInString(my.story))));
        info.s$general$PP = countPersonalPronouns(words.r);
        info.s$general$GENDER = countGenderLanguage(words.r);
        info.s$general$GRIM = countCustomWordList(words.r);
      
      timer.general = as.numeric(Sys.time());
      elapsed = round( (timer.general - timer.txt), 2);
      print(paste0("story: ... ", title.f, " ... [general] in ",elapsed," secs"));
      
        info.s$sentences = buildNgrams(my.story,5, my.stopwords = my.stopwords);   
      
      timer.ngrams = as.numeric(Sys.time());
      elapsed = round( (timer.ngrams - timer.general), 2);
      print(paste0("story: ... ", title.f, " ... [ngrams] in ",elapsed," secs"));
      
      # save File
      
      my.result = info.s;
        saveRDS(my.result, out.rds);
      } else { my.result = readRDS(out.rds); }


  timer.end = as.numeric(Sys.time());
  elapsed = round( (timer.end - timer.start), 2);
  print(paste0("story: ... ", title.f, " ... [--TOTAL--] in ",elapsed," secs"));
  my.result;
  }












# 
# 
# 
# <PRE>
# 9  ... SENTENCE 17?
# 35 ... 20
# 36
# 
# [1] "-----------------> Dobbin!"
# Error: $ operator is invalid for atomic vectors
# > traceback()
# 3: computeReadability(1, rinfo.s) at functions-nlp.R#102
# 2: buildNgrams(my.story, 5, my.stopwords = stop.snowball) at file8b007e952fa7#141
# 1: prepareOneStory(df.grimm, path.to.grimm, title, title.f)
# 
# "THE.DOG.AND.THE.SPARROW.txt"
# 
#  Error: $ operator is invalid for atomic vectors 
# 3.
# computeReadability(1, rinfo.s) at functions-nlp.R#101
# 2.
# buildNgrams(my.story, 5, my.stopwords = stop.snowball) at functions-nlp-str.R#141
# 1.
# prepareOneStory(df.grimm, path.to.grimm, title, title.f) 
# </PRE>
# 
# 
# <PRE>
# 11  ... SENTENCE 17?
#   
# THE.FISHERMAN.AND.HIS.WIFE
# 
# Error in next.word$features[[1]] : subscript out of bounds
# 
# > traceback()
# 2: buildNgrams(my.story, 5, my.stopwords = stop.snowball) at functions-nlp-str.R#141
# 1: prepareOneStory(df.grimm, path.to.grimm, title, title.f)
# 
# </PRE>
# 
# <PRE>
# 19 ...
# 22 ...
# 25 ...
# 41 ...
# 
# Error in next.word$features[[1]] : subscript out of bounds
# </PRE>
# 
# <PRE>
# 30
# 
# [1] "Sentence [3] of 58"
# [1] "-----------------> 'Kate!"
#  Error in names(x) <- value : 
#   'names' attribute [2] must be the same length as the vector [1] 
# 4.
# `colnames<-`(`*tmp*`, value = c("tags", "count")) at C:\Users\ALEXAN~1\AppData\Local\Temp\RtmpcjaDE6\filebba81bda29d6#69
# 3.
# tabularizePOS(my.words, my.words.idx) at functions-nlp.R#91
# 2.
# buildNgrams(my.story, 5, my.stopwords = stop.snowball) at functions-nlp-str.R#141
# 1.
# prepareOneStory(df.grimm, path.to.grimm, title, title.f) 
# 
# </PRE>

