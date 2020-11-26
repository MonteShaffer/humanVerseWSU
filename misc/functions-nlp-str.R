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
 
  #str = gsub("{``}",'"',str,fixed=TRUE);
  #str = gsub("``",'"',str,fixed=TRUE);
  #str = gsub("`","'",str,fixed=TRUE);
  str = gsub("—"," -- ",str,fixed=TRUE);
  # \u0097
  
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
  stop.key = paste(my.stopwords, collapse="-");
  # no stopwords, no md5 key, just a hyphen
  stop.md5 = md5(stop.key);  if(stop.key == "") { stop.md5 = ""; }
      out.rds = paste0(path.to.grimm, title.f, "-", stop.md5 , ".rds"); # stats summary
      out.start = gsub(".rds",".has-started", out.rds);
      
  print(out.rds);    
  if(!file.exists(out.rds))  
    {
    Sys.sleep(runif(1,0.2,1.3));
    print(out.start);
    if(file.exists(out.start)) { return(FALSE); } # skip
    writeLine("hello there friend!", out.start, append=FALSE);
    
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


summarizeGENDER = function(GENDER)
  {
  # one-row dataframe
  # CROSS-TABS
  genders = c("male", "female");
  numbers = c("singular","plural");
  
  keys = "ALL";
  vals = sum(GENDER$count, na.rm=TRUE);
  for(gender in genders)
    {
    sub = subsetDataFrame(GENDER, "gender", "==", gender);
    keys = c(keys, gender);
    vals = c(vals, sum(sub$count, na.rm=TRUE) );
    }
  for(number in numbers)
    {
    sub = subsetDataFrame(GENDER, "number", "==", number);
    keys = c(keys, number);
    vals = c(vals, sum(sub$count, na.rm=TRUE) );
    }
  
  df = as.data.frame( rbind(NULL,vals));
    rownames(df) = NULL;
    colnames(df) = paste0("GENDER.",keys);
  df;
  }

summarizePP = function(PP)
  {
  # one-row dataframe
  # CROSS-TABS
  persons = c("first","second","third");
  numbers = c("singular","plural");
  
  keys = "ALL";
  vals = sum(PP$count, na.rm=TRUE);
  for(person in persons)
    {
    sub = subsetDataFrame(PP, "person", "==", person);
    keys = c(keys, person);
    vals = c(vals, sum(sub$count, na.rm=TRUE) );
    }
  for(number in numbers)
    {
    sub = subsetDataFrame(PP, "number", "==", number);
    keys = c(keys, number);
    vals = c(vals, sum(sub$count, na.rm=TRUE) );
    }
  
  df = as.data.frame( rbind(NULL,vals));
    rownames(df) = NULL;
    colnames(df) = paste0("PP.",keys);
  df;
  }
 
summarizeGeneral = function(which="ALL", df.grimm, path.to.grimm,
                            my.stopwords = NULL
                              )
  {
  titles = unique(df.grimm$title);
  titles.f = cleanupTitles(titles);
    nt = length(titles);
    
  if(is.character(which)) 
    {
    if(which == "ALL") 
      { 
      idx = 1:nt; 
      } else { 
              idx.t = which(titles == which);
              idx.f = which(titles.f == which);
              
              idx = unique( c(idx.t, idx.f) )[1];
              }
    } else { idx = which; } # numeric
    
    ## idx = 20; # "HANSEL AND GRETEL";
  ni = length(idx);
  
  s.df = NULL;
  for(i in 1:ni)
    {
    my.idx = idx[i];
    title = titles[my.idx];
    title.f = titles.f[my.idx];
  
    one = prepareOneStory(df.grimm, path.to.grimm, 
                          title, title.f,
                          my.stopwords = my.stopwords
                          );
    
    # names(one$general);
    
    count.sentences = nrow(one$sentences$sentiment);
    

      
    my.sentiment = as.data.frame( rbind(NULL,one$general$sentiment));
        rownames(my.sentiment) = NULL;
    colnames(my.sentiment) = c("S.POS","S.NEG");
      
    
    row = cbind(my.idx, title, 
                    count.sentences,
                    one$general$case,
                    one$general$readability
                  );
    
    # the other was from raw text, this is from "clean" text
      clean.FK = unlist(computeFleshKincaid(row[3], row[4], row[10]));
    new.FK = as.data.frame( rbind(NULL,  clean.FK ));
        rownames(new.FK) = NULL;
    colnames(new.FK) = c("FRE.N","FKGL.N");
    
    row = cbind(row, new.FK,
                    one$general$punctuation,
                    summarizePP(one$general$PP),
                    summarizeGENDER(one$general$GENDER),
                    my.sentiment);
  
    s.df = rbind(s.df, row);
    }
    
  s.df = as.data.frame(s.df);
  rownames(s.df) = NULL;
  
  s.df;
  }



element.exists <- function(var, element)
{
  tryCatch({
    if(length(var[[element]]) > -1)
      return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

parseGutenberg.GRIMM = function(path.to.grimm,
                        file.stem = "fairytales",
                        txt.file.remote = "https://www.gutenberg.org/files/2591/2591-0.txt",
                        html.file.remote = "https://www.gutenberg.org/files/2591/2591-h/2591-h.htm",
                        my.local.path = path.to.gutenberg
      )
  {
  if(!element.exists(local.data.path))
    {
    # must exist for current grabHTML to work
    .GlobalEnv$local.data.path = my.local.path;
    }
  
  txt.file.local = paste0(path.to.grimm, file.stem, ".txt");
    my.txt = grabHTML(txt.file.local, txt.file.remote);
  html.file.local = paste0(path.to.grimm, file.stem, ".html");
    my.html = grabHTML(html.file.local, html.file.remote);
  
  cache.file.local = paste0(path.to.grimm, file.stem, ".rds");
  
  timer.start = as.numeric(Sys.time());


if(!file.exists(cache.file.local))
{
  df.grimm = NULL;
chap.n = 0;
para.n = 1;
title = "THE BROTHERS GRIMM FAIRY TALES";
what  = "-INTRO-";

### let's trim around gutenberg stuff
start = "*** START OF THIS PROJECT GUTENBERG EBOOK GRIMMS' FAIRY TALES ***";
end = "End of Project Gutenberg";

tmp = strsplit(my.html, end, fixed=TRUE)[[1]];     # str(tmp);
tmp1 = strsplit(tmp[1],  start, fixed=TRUE)[[1]];  # str(tmp1);

n.html = tmp1[2];

# there is one note ...  <div class="mynote">
start = '<div class="mynote">';
end = '</div>';

tmp = strsplit(n.html, start, fixed=TRUE)[[1]];     # str(tmp);
f.html = tmp[2];
tmp1 = strsplit(tmp[3],  end, fixed=TRUE)[[1]];     # str(tmp1);





note = cleanupHTMLentities(tmp1[1],replace="cs");
note = strip_tags(note);
# note = removeWhiteSpace(note);
note = trimMe(note);
type = "note";

row = c(chap.n, what, title, para.n, type, note);
df.grimm = rbind(df.grimm, row);
  chap.n = 1+chap.n;
  para.n = 1;


### h2
tmp = strsplit(f.html,"<h2>", fixed=TRUE)[[1]];
idx.stories = 2:63

for(i in idx.stories)
  {
  story = tmp[i];
  what = paste0("STORY-",chap.n);
    story = cleanupHTMLentities(story,replace="cs");
    #story = removeWhiteSpace(story);
    story = trimMe(story);
  # story title
  stmp = strsplit(story, "</h2>", fixed=TRUE)[[1]]; 
  story.title = trimMe(stmp[1]);
  
  story = trimMe(stmp[2]);
  # let's keep paragraphs for a minute ...  <p> and <pre>
  # because of location of <pre>, I will explode on ending </p>
  # we will count <pre> as a paragraph with a poem flag ... # NOT PERFECT: it doesn't allow for multiple poems within a paragraph, see JORINDA.
  ptmp = strsplit(story, "</p>", fixed=TRUE)[[1]]; 
  
  # paragraphs = list();
  n.p = length(ptmp);
  k = 1;
  for(j in 1:n.p)  # i = 3; j = 12
    {
    para = ptmp[j];
    qtmp = strsplit(para, "<p>", fixed=TRUE)[[1]]; 
      poem = trimMe(strip_tags(qtmp[1]));
      if(is.na(poem)) { poem = ""; }
      if(poem != "")
        {
        # pinfo = list("type" = "poem", "text" = poem);
        # paragraphs[[k]] = pinfo;
        row = c(chap.n, what, story.title, k, "poem", poem);
        df.grimm = rbind(df.grimm, row);
        k = 1 + k;
        }
      prose = trimMe(strip_tags(qtmp[2]));
      if(is.na(prose)) { prose = ""; }
      if(prose != "")
        {
        prose = removeWhiteSpace(prose); # poems will keep line breaks
        # pinfo = list("type" = "prose", "text" = prose);
        # paragraphs[[k]] = pinfo;
        row = c(chap.n, what, story.title, k, "prose", prose);
        df.grimm = rbind(df.grimm, row);
        k = 1 + k;
        }
    ###
    }
  ### end of single story ... 
  chap.n = 1+chap.n;
  }


df = as.data.frame(df.grimm);
  colnames(df) = c("chap.n", "chap.type", "title", "para.n", "para.type", "para.text");
  rownames(df) = paste0(df$chap.n,"-",df$para.n);
df$chap.n = as.numeric(df$chap.n);
df$para.n = as.numeric(df$para.n);

df.grimm = df;
saveRDS(df.grimm, cache.file.local);
    } else { df.grimm = readRDS(cache.file.local); }

timer.end = as.numeric(Sys.time());
timer.elapse = timer.end-timer.start;
print(paste0("Time elapsed: ", round(timer.elapse,2) ));
  
df.grimm;
  
  }
## 









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

