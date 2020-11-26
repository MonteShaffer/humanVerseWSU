  
# ngrams = buildNgrams(my.story, my.stopwords=stop.snowball);

library(tm);
# tm::stemDocument(c("computers", "computation", "complicated", "complication"));
# map POS ... to element for LSA prep ...
buildNgrams = function(str, n=5, 
                    do.pos = TRUE,
                    verbose = FALSE,
                    do.stemming = TRUE, # tm::stemDocument ... not hard to ADD
                    do.variants = FALSE, # TODO  
                                      # final.rows = cbind(i, final.words, final.tags);
                                      # above would allow for modifier inflation ...
                                      # would have to look forward/backward to do
                    inflate = FALSE, # n-grams and modifiers
                                    # necessarily a 5 gram will have its children
                    my.stopwords = NULL # my.stopwords = stop.snowball
                                      ) # preferably one sentence at a time, already with a stop break
  {
timer.start = as.numeric(Sys.time());
########## start of function #######  
  
  # first 3 are required ...
  gram.types = c("words", ".tags", "|simple",
                        "words.tags", "words|simple",
                "word+",
                        "word+.tags", "word+|simple"
                                    );
  # if POS exists, we can perform the variants by switching modifiers ...
  grams = initGrams(n,gram.types);
  ####  TODO ::: after walk 
  # if we run across punctuation, we treat as a stop ...
  # what to do about contractions can't and quotations ?
### str = my.story;
  # str = "Can't you get anything right?";
  str = prepStringGram(str, TRUE);
  # if(do.pos)
    {
    my.pos = performPOS(str);
    }
  
  # test = "Monte's friend and son Alexander can't read well but he is not yet five years old.";
  # t.pos = performPOS(test);
  # substr(test, 6,7); ## drop POS from choices as a word
  # https://stackoverflow.com/questions/28720174/negation-handling-in-nlp
  # substr(test, 34,35);    ## ca   [MD] 
  # substr(test, 36,38);    ## n't  [RB]
  ##  CC is "and" and "but"
  ##  RB is "not" and "n't" and "yet"
  # substr(test, 60,62);    ## not  [RB]
  # substr(test, 68,71);    ## five  [CD]
  
  
  my.sentences   = my.pos$sentences;
  my.words       = my.pos$words;
  n.sentences    = length(my.sentences);

  tags.info = setupTags();
                    
  # I could replace a stop-word with its generic POS
  # list$pos

  
  case = NULL;
  punctuation = NULL;
  PP = NULL;
  GENDER = NULL;
  GRIMM = NULL;
  
  readability = NULL;
  sentiment = NULL;
  mytags = list();  # raw, we don't know if we have all the keys, and if they are unique
  mytags.s = list(); # simple "keys"  adj
  final = NULL;
  
########## start of sentences #######   
  for(i in 1:n.sentences)
    {
    print(paste0("Sentence [",i,"] of ",n.sentences));
    flush.console();
    
    my.sentence = as.data.frame(my.sentences[i]);
    
      my.id = my.sentence$id;
      idx.start =  my.sentence$start;
      idx.end   =  my.sentence$end;
    s.sentence = substr(str, idx.start, idx.end);
 
  print(paste0("-----------------> ",s.sentence)); 
  flush.console();
    
        my.words.idx = my.sentence$features[[1]]$constituents;
    mytags[[i]] = tabularizePOS(my.words,my.words.idx); # still in unique data format
    # mytags.s[[i]] = simplifyPOS(mytags[[i]],tags,tags.keyed);
    mytags.s[[i]] = simplifyPOS(mytags[[i]],tags.info);
    
    # words from POS may be different than actual words; e.g., "can't"
    words.r = getRawWords(s.sentence); 
    
    
      sinfo = doSentimentAnalysis(s.sentence); # expensive
    sentiment = rbind(sentiment, c(i, sinfo) );
        rinfo.s = countSyllablesInWord(words.r);
        # print(rinfo.s);
        rinfo = computeReadability(1,rinfo.s);
        # countSyllablesInWord words
    readability = rbind(readability, c(i, unlist(rinfo)) );
        info.c = countWordsInString(s.sentence);
    case = rbind(case, c(i, unlist(info.c)) ); 
        info.p = countPunctuation(s.sentence);
    punctuation = rbind(punctuation, c(i, unlist(info.p)) );
        info.pp = countPersonalPronouns(words.r);
    PP = rbind(PP, cbind(i, rownames(info.pp), info.pp) );
        info.ge = cbind(i, countGenderLanguage(words.r));
    GENDER = rbind(GENDER, info.ge );
        info.gr = cbind(i, countCustomWordList(words.r));
    GRIMM = rbind(GRIMM, info.gr );

    
    r = 1;
    # we will use these to compare ...  
    # no punctuation, lower case ... no knowledge of POS
    
    nw = length(my.words.idx);
    w = 1;
    #for(w in 1:nw)
    # "Can't you get anything right?" ... 67% positive
    my.stack = initStack(gram.types);
    final.words = c(); # full list, unstacked ... these would allow to later matching
    final.tags = c();  # full list, unstacked
            # grams = initGrams(gram.types);       
    pos.stopped = TRUE;
    word.stopped = FALSE;
    has.skipped = FALSE;
    left.quote = FALSE;
    right.quote = FALSE;
##################################################    
    while(w <= nw)
      {
      
      
########## start of while #######      
      
      my.word.idx = my.words.idx[w];
     
      my.word = as.data.frame( subset(my.words, id==my.word.idx) );
        my.feature = my.word$features[[1]]$POS;
          f.word = paste0(".",my.feature);
          p.word = paste0("|",tags.info$tags.keyed[[my.feature]]);
          s.word = substr(str, my.word$start, my.word$end);
          # "asleep." in sentence 58
          # POS "period" should be taken care of here ... bug
          s.word = gsub(".", "", s.word, fixed=TRUE);
          
          r.word = words.r[r];
          r.len = nchar(r.word);
              if(is.na(my.word.idx)) { break; }
              if(is.na(r.word)) { break; }

      skip.me = (s.word == "'" && ( has.skipped || pos.stopped || word.stopped) );
      if(skip.me)
        {
        if(charAt(r.word,1) == "'")
          { 
          r.word = substr(r.word,2,r.len);
          left.quote = TRUE;  # "'what" in sentence 4 of HANSEL
          }
        }
      if(left.quote)
        {
        if(charAt(r.word,1) == "'")
          {
          r.word = substr(r.word,2,r.len);
          }
        }
      if(charAt(r.word,r.len) == "'") # "ourselves'" in sentence 5 of HANSEL
          { 
          r.word = substr(r.word,1,(r.len-1));
          right.quote = TRUE;  
          }
                                      
      
      
      
if(verbose) 
{
print(paste0("[",w,"] ... word: [",s.word, "] r: [",r.word,"] --> f: [",f.word,"] p: [",p.word,"] ==> ", tm::stemDocument(r.word) ));
flush.console();
}    
      if(is.element(my.feature, tags.info$skip ) || skip.me)  # skip this ...
        {
        w = 1 + w;
        has.skipped = TRUE;
if(verbose) 
{
        print("SKIPPING");
        print(s.word);
}  
        } else {
                has.skipped = FALSE;
      
      
      
      
              if(is.element(my.feature, tags.info$stop ))
                {
                # hard stop ... 
                ginfo = updateGrams(grams, my.stack, "ALL", tags.info, do.stemming);
                  grams = ginfo$grams;
                my.stack = initStack(gram.types);
                pos.stopped = TRUE;
                } else {
                        pos.stopped = FALSE;
                        r = 1 + r;
                        if(!is.element(my.feature, tags.info$tags$proper)) { s.word = tolower(s.word); } # NNP are proper nouns # lower case ...
                        n.word = s.word;
                        if(tolower(s.word) != tolower(r.word))
                          {
                          # "can't" is now "cant" [words.r] ... and linked to POS "ca" and "n't"
                          # we skip one of the idx and update "cant" back to "can't"
                          ## # stop("monte"); 
                          #w = 1 + w;
                            next.word.idx = my.words.idx[w+1];
                            next.word = as.data.frame( subset(my.words, id==next.word.idx) );
                            if(nrow(next.word) == 0) 
                              { 
                              #if(verbose)
                                {
                                print(paste0("we are not finding a next word, using: ", n.word));
                                }
                              } else {
                            
                                          if(verbose)
                                          {
                                          #print("monte");
                                          print(next.word); 
                                          }
                                      next.s.word = substr(str, next.word$start, next.word$end);
                                          if(verbose)
                                          {
                                          print(paste0("====> ", s.word, " ... ", next.s.word, " *** ",  paste(next.word$features, collapse=" :: ")));
                                          #print(element.exists(next.word$features[[1]]$POS));
                                          }
                                      next.feature = next.word$features[[1]]$POS;
                                        next.f.word = paste0(".",next.feature);
                                        next.p.word = paste0("|",tags.info$tags.keyed[[next.feature]]);
                                        
                                    # "I'll" ...
                                    n.word = paste0(s.word,next.s.word);  
                                    
                                    f.word = paste0(f.word,"-",next.f.word);
                                    p.word = paste0(p.word,"-",next.p.word);
                            ###stop("monte");  
                                    # append -feautre to previous
                                    w = 1 + w; # let's skip ... 
                                    }
                          }
                        
                        
                        # let's try and keep "auto-pop" from happening
                        # I could just let "stop" words do it's thing 
                        # and at the end do the combos ... so let's say it is 13 long
                        # I could just build all 5-grams and pairs at ends 
                        # do it this way for now ...
                        ginfo = updateGramsIfMax(n, grams, my.stack, "ALL", tags.info, do.stemming);
                            grams    = ginfo$grams;
                            my.stack = ginfo$my.stack;
                        
                        # what if it is a stop word? ... 
                        # that will only update the regular stack
                        # not the POS stacks
                        final.words = c(final.words, n.word);
                        
                        if(is.element(n.word, my.stopwords))
                            {
                            for(gram.type in c("words","words.tags","words|simple",
                                               "word+","word+.tags","word+|simple"))
                              {
                              ginfo = updateGrams(grams, my.stack, gram.type, tags.info, do.stemming);
                                grams    = ginfo$grams;
                                my.stack = ginfo$my.stack;
                              my.stack = resetStackElement(my.stack, gram.type);
                              }
                            word.stopped = TRUE;
                            
                            } else {
                                    word.stopped = FALSE;
                                    my.stack[["words"]] = pushVector(n.word, my.stack[["words"]]$vec);
                                    my.stack[["words.tags"]] = pushVector(paste0(n.word,f.word), my.stack[["words.tags"]]$vec);
                                    my.stack[["words|simple"]] = pushVector(paste0(n.word,p.word), my.stack[["words|simple"]]$vec);
                                    
                                    t.word = tm::stemDocument(n.word);
                                    # print(paste0("stemmed: ", t.word));
                                    my.stack[["word+"]] = pushVector(t.word, my.stack[["word+"]]$vec);
                                    my.stack[["word+.tags"]] = pushVector(paste0(t.word,f.word), my.stack[["word+.tags"]]$vec);
                                    my.stack[["word+|simple"]] = pushVector(paste0(t.word,p.word), my.stack[["word+|simple"]]$vec);
                                    }
                        # gram.types[gram.types != "words"]
                        final.tags = c(final.tags, f.word);
                        my.stack[[".tags"]] = pushVector(f.word, my.stack[[".tags"]]$vec);
                        my.stack[["|simple"]] = pushVector(p.word, my.stack[["|simple"]]$vec);
                                    
                        
                        
                        
                        
                        # ... soft stop will update my.stack but not my tags
                        # use push/pop when > 5 ?
                        # https://stackoverflow.com/questions/28687806/a-better-way-to-push-and-pop-to-from-lists-in-r
                        # push/pop list vs vector ... just NULL and append to vecotr?
                        
                        ### stack full = update grams, otherwise just update stack ...
                        # is.stop = 
                  
                  
                  
                        }
                w = 1 + w;
                }

## print(paste0("[",w,"] ... word: [",s.word, "] r: [",r.word,"] --> f: [",f.word,"] p: [",p.word,"]"));
########## end of while ####### 
        
        }
      # hard stop ... end of sentence, just to be certain ... 
      # If a POS . this was already taken care of
      ginfo = updateGrams(grams, my.stack, "ALL", tags.info, do.stemming);
        grams = ginfo$grams;
      my.stack = initStack(gram.types);
      
#       story = 30
#       "Sentence [3] of 58"
# [1] "-----------------> 'Kate!"
      # print(final.tags);
      # print(final.words);
      
      if(length(final.words) > 0)
      {
      if(length(final.words) == length(final.tags))
      {
      final.rows = cbind(i, final.words, final.tags);
      final = rbind(final, final.rows);
      }
      }
      # print(final.rows);
      
      
      #stop("final");
    
      ### end of sentences ...
      }
  
  final = as.data.frame(final);
    rownames(final) = NULL;
    colnames(final)[1] = c("sentence");
  
  sentiment = as.data.frame(sentiment);
    rownames(sentiment) = NULL;
    colnames(sentiment) = c("sentence", "positive", "negative");
  readability = as.data.frame(readability);
    rownames(readability) = NULL;
    colnames(readability)[1] = c("sentence");
  
  case = as.data.frame(case);
    rownames(case) = NULL;
    colnames(case)[1] = c("sentence");
  
  punctuation = as.data.frame(punctuation);
    rownames(punctuation) = NULL;
    colnames(punctuation)[1] = c("sentence");
  
  PP = as.data.frame(PP);
    rownames(PP) = NULL;
    colnames(PP)[1] = c("sentence");
    colnames(PP)[2] = c("row.type");
  
  
  GENDER = as.data.frame(GENDER);
    rownames(GENDER) = NULL;
    colnames(GENDER)[1] = c("sentence");
  
  GRIMM = as.data.frame(GRIMM);
    rownames(GRIMM) = NULL;
    colnames(GRIMM)[1] = c("sentence");
  
  
  # case = NULL;
  # punctuation = NULL;
  # PP = NULL;
  # GENDER = NULL;
  # GRIMM = NULL;
  
   
timer.end = as.numeric(Sys.time());
elapsed = round( (timer.end - timer.start), 2);
  #print(paste0("[-- EVERYTHING --] in ",elapsed," secs"));
  # pause = c(".",";",":",",","?","!","^[^","^]^");  # Let's only do this if POS exists
  # can I simplify POS, more basic?
  
  list("grams" = grams, "final" = final, "time" = elapsed,
        "sentiment" = sentiment, "readability" = readability,
        "mytags" = mytags, "mytags.s" = mytags.s,
        "case" = case, "punctuation" = punctuation,  "PP" = PP,
        "GENDER" = GENDER, "GRIMM" = GRIMM
        );
  }













truncateWordVector = function(words, cut = 3) # words is vector, in order
  {
  words.lower = tolower(words);
  # bag of words, order doesn't matter
  words.table = as.data.frame( sort(table(words.lower),decreasing = TRUE));
    colnames(words.table) = c("word","count");
  
  comparison = words.table$count > cut;  
  new.table = words.table[comparison,]; 
  new.table$word;  # just return the vector of words that meet criteria ...
  }


library(tm);
library(SentimentAnalysis);

doSentimentAnalysis = function(str, qdap=FALSE)
  {
  # https://opendatascience.com/sentiment-analysis-in-r-made-simple/
  sentiment = SentimentAnalysis::analyzeSentiment(str);
  
  
  # convertToBinaryResponse(sentiment)$SentimentGI 
  # convertToBinaryResponse(sentiment)$SentimentHE
  # convertToBinaryResponse(sentiment)$SentimentLM
  # convertToBinaryResponse(sentiment)$SentimentQDAP
  
  # seems to be a subtraction of negative and positive, 
  # so let's return both
  
  pos = 0;
  neg = 0;
  
  keys = c("GI","HE","LM"); # ,"QDAP");  # "Can't you get anything right?"
  if(qdap) { keys = c("GI","HE","LM","QDAP"); }
  
  for(key in keys)
    {
    pos = pos + sentiment[[paste0("Positivity",key)]];
    neg = neg + sentiment[[paste0("Negativity",key)]]; 
    }
  
  # let's norm to 1, so it is 50-50 for perfect balance
  pos.norm = pos / (pos + neg);
  neg.norm = neg / (pos + neg);
  
  # doesn't capture "dark"?
  c(pos.norm, neg.norm);
  }


