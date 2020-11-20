library(openNLP);
library(NLP);


subsetPOS = function(apos, sub=c(1))
  {
  # just grab indexes found in sub ... 
  # because of custom structure, not so easy
  true.idx = c();
  n.apos = length(apos);
  remaining = length(sub);
  for(i in 1:n.apos)
    {
    row.apos = as.data.frame(apos[i]);
    if(is.element(row.apos$id, sub))
      {
      true.idx = c(true.idx, i);
      remaining = remaining - 1;
      if(remaining == 0) { break; }
      }
    }
  apos[true.idx];
  }


simplifyPOS = function(my.table, tags.info)
  {
  tags = tags.info$tags;
  tags.keyed = tags.info$tags.keyed;
  keys = sort(names(tags));
  n.keys = length(keys)
  count.keys = list();
  for(i in 1:n.keys)
    {
    key = keys[i];
    count.keys[[key]] = 0;
    }
  nt = nrow(my.table);
  for(j in 1:nt)
    {
    row = my.table[j,];
    tag = row$tags;
    count = row$count;
    key = tags.keyed[[tag]];
    if(!is.null(key))
      {
      count.keys[[key]] = count + count.keys[[key]];
      }
    }
  my.counts = unlist(count.keys);
  my.names = names(my.counts);
  
  n.table = as.data.frame(t(my.counts));
    colnames(n.table) = my.names;
  #n.table = as.data.frame(cbind(my.names, my.counts));
    #colnames(n.table) = c("keys", "count");
    #rownames(n.table) = NULL;
  
  n.table;
  }
tabularizePOS = function(apos,sub=NULL)
  {
  if(!is.null(sub))
    {
    apos = subsetPOS(apos,sub);
    }
  tags = sapply(apos$features, `[[`, "POS");
  my.table = as.data.frame( sort(table(tags),decreasing = TRUE) );
    colnames(my.table) = c("tags","count");
    my.table$tags = as.character(my.table$tags);
  my.table;
  }
performPOS = function(str)  # preferably on a single sentence ...
  {
  # ";" counts as colon ...
  # str = "My tale is done, there runs a mouse; whosoever catches it, may make himself a big fur cap out of it.";
  sentence.init = Maxent_Sent_Token_Annotator();
  word.init     = Maxent_Word_Token_Annotator();
  sentence.word.a   = annotate(str, list(sentence.init, word.init));
  
  pos.init = Maxent_POS_Tag_Annotator(); # Maxent_POS_Tag_Annotator(probs=TRUE);
  sentence.word.apos = annotate(str, pos.init, sentence.word.a);
  
  sentences.apos = subset(sentence.word.apos, type=="sentence");
  
  words.apos = subset(sentence.word.apos, type=="word");
  my.table = tabularizePOS(words.apos);
  # tags = sapply(words.apos$features, `[[`, "POS");
  # my.table = as.data.frame( sort(table(tags),decreasing = TRUE) );
  #   colnames(my.table) = c("tags","count");
  #   my.table$tags = as.character(my.table$tags);
    
  list("table" = my.table, "words" = words.apos, "sentences" = sentences.apos);
  }

getRawWords = function(str, lower=TRUE)
  {
  str = removePunctuationFromString(str);
  if(lower){ str = tolower(str); }
  str = removeWhiteSpace(str);
  strsplit(str," ",fixed=TRUE)[[1]];
  }
  
  
countPunctuation = function(sentence)
  {
  myCounts = c();

  countMe = c(".",";",":",",","?","!","^[^","^]^");
  labelMe = c("P.per","P.semi","P.colon","P.comma","P.quest","P.exclam","P.left","P.right");

  cn = length(countMe);
  countR = c();
  for(i in 1:cn)
    {
    tmp = strsplit(sentence,countMe[i],fixed=TRUE)[[1]];
    countR[i] = length(tmp);
    }
  names(countR) = labelMe;
  countR = as.data.frame(t(countR));
  
  countR$P.apost = countR$P.right - countR$P.left; # apostrophes ... some poetry elemenents may have LSQ RSQ and technically be an apostrophe, see... ‘O’er hill and o’er dale
  # ‘I’ll tell you what, husband,’ ... grabbing "conversation" may be a bit tricky ...
  
  countR;
  }


removePunctuationFromString = function(str)
  {
  countMe = c(".",";",":",",","?","!","^[^","^]^");
  cn = length(countMe);
  for(i in 1:cn)
    {
    str = gsub(countMe[i],"",str,fixed=TRUE);
    }
  str = gsub("-"," ",str,fixed=TRUE); # let's break apart hyphenated words
  str = removeWhiteSpace(str);
  str;
  }

countWordsInString = function(str)  # preferably a sentence, this is large ...
  {
  str = removePunctuationFromString(str);
  words = strsplit(str," ",fixed=TRUE)[[1]];
  count.words = wc = length(words); # word count
  
  
  count.lower = count.upper = count.ucfirst = 0;

  details = list( "lower" = c(), 
                  "upper" = c(), 
                  "ucfirst" = c());
  
  ####syllables = list();
  
  for(i in 1:wc)
    {
    word = words[i];
    ####syllables[[i]] = countSyllablesInWord(word);
    # https://en.wikipedia.org/wiki/Gunning_fog_index
    
    word.lower = tolower(word);
    word.upper = toupper(word);
    
    if(word == word.lower) 
      { 
      count.lower = 1 + count.lower; 
      details$lower = c(details$lower,word);
      } else if(word == word.upper) 
          { 
          count.upper = 1 + count.upper; 
          details$upper = c(details$upper,word);
          } else { 
                  count.ucfirst = 1 + count.ucfirst; 
                  details$ucfirst = c(details$ucfirst,word);
                  }
    }

  # count.sentences = 1;
  
  # list("words" = words, "details" = details, "syllables" = syllables,
  #     "readability" = computeReadability(count.sentences, syllables),
  #     "count.words" = count.words, "count.lower" = count.lower, 
  #     "count.upper" = count.upper, "count.ucfirst" = count.ucfirst);

  list("count.words" = count.words, "count.lower" = count.lower, 
      "count.upper" = count.upper, "count.ucfirst" = count.ucfirst);
  }




countCustomWordList = function(words, all.words = buildCustomWordList())
  {
  words.lower = tolower(words);
  # bag of words, order doesn't matter
  words.table = as.data.frame( sort(table(words.lower),decreasing = TRUE));
    colnames(words.table) = c("word","count");
    
  my.grimm = NULL;
  for(a in 1:length(all.words))
    {
    # could do wildcard ...
    lookup = subsetDataFrame(words.table, "word", "==", all.words[a]);
    lookup.count = 0;
    if(dim(lookup)[1] == 1) { lookup.count = lookup$count; }
    row = c(all.words[a],lookup.count);
    my.grimm = rbind(my.grimm,row);
    }   
  
  cnames = c("word","count");  # "group" would be nice
  
  my.grimm = as.data.frame(my.grimm);
    rownames(my.grimm) = NULL;
    colnames(my.grimm) = cnames;
  
  my.grimm$count = as.numeric(my.grimm$count);
  
  my.grimm;
  }

buildCustomWordList = function(grimms = c("wolf/wolves", "bear/s", "fox/es", "bird/s", "forest/s", "iron/s", "fish/ed/es/fishing/fisherman/fishermen/fishpond/s",  "tree/s", "house/s", "garden/s", "bread/s", "water/s", "hand/s", "pure", "lock/s", "finger/s", "toe/s", "leg/s", "arm/s", "head/s", "foot/feet/footed", "bar/s", "stomach/e/belly", "walk/s/ed", "run/runs/ran", "loaf/loaves", "search/searching/searches/searched", "sea/s", "prophet/s/prophecy/prophecies", "small/large", "emperor/s", "pope/s", "fly/flies/flew", "child/children", "dwarf/dwarves", "witch/es", "moon/s", "raven/s", "cake/s", "sweet/s", "apple/s", "lake/s", "axe/s", "wood/s", "hill/s", "hat/s", "goose/geese", "box/es", "wheel/s", "tower/s", "boil/boiled/boils", "lamb/s", "tail/s", "grove/s", "field/s", "voice/s", "cheese", "coat/s", "rose/s/y", "bushe/s", "feast/s", "wake/s/awaken/rise/n/arose", "enemy/enemies", "better/best", "worse/worst", "pocket/s", "coffin/s", "hungry/hunger/weary/weariness", "cage/s", "chicken/s", "light/s", "dark/darken/darkness", "board/s", "candle/s", "eye/s", "girdle/s", "sleep/asleep/sleeps/rest/rests/rested/restful", "round/s", "circle/s/ed", "square/s/ed", "eat/ate/eaten/eats/", "fire/s", "yard/s", "breast/s", "pigeon/s", "cook/cooked/cooking/cooks", "wild", "beast/s", "devour/s", "god/less", "winter/s", "yes", "no", "soldier/s", "prison/s", "prisoner/s", "grass/es", "rope/roped/ropes", "shoulder/s", "art/s", "cunning/think/thought", "friend/ly/s", "leaf/leaves", "whistle/whistling/whistled/whistles", "year/s", "month/s", "week/s", "dream/s", "gay/happy/sad/unhappy", "sword/s", "air/s", "sing/s/singing/sung", "bone/s", "silk", "well/s",  "day/s",  "evening/s", "morning/s",  "night/s",  "castle/s",  "door/s", "marble/s", "gold/golden/silver/pearls/jewels",  "good/peace", "wicked/evil", "cow/s", "goat/s", "bee/s", "milk/honey", "window/s", "marry/marries/married", "kid/s", "bed/s", "key/s", "giant/s", "today/tomorrow/yesterday", "wish/wished/wishes", "priest/s/priesthood", "sun/s", "frog/s", "fog/s", "summer/s", "spring/s", "fall/s", "autumn/s", "young/younger/youngest", "old/older/oldest/eldest", "leather", "peasant/s", "shepherd", "mayor/s", "hole/s", "barrel/s", "straw", "devil", "servant/s", "sheep", "flock", "cottage/s", "bedchamber/s", "room/s", "kitchen", "courtyard/s", "cabbage/s", "chain/s", "hot/cold/warm", "spirit/s", "red/blue/white/green/s/black/yellow/purple", "blood/bleeding/bleeds", "pig/s", "horse/s", "dog/s", "die/death/dying/died/dies/dead", "kill/killed/kills", "birth/born", "fallen", "handsome/handsomest /beautiful/lovely", "glass", "ball/s", "mouse/mice/mouses/", "cat/s", "mountain/s", "pot/s", "fat/s", "on/off",  "live/lives/life/lived/alive", "early/late", "shoe/s", "stone/s", "pebble/s", "drawer/s", "table/s", "plate/s", "bowl/s", "piece/s", "body/bodies", "salt/pepper", "left/right", "top/bottom", "cut/s", "wine/s", "home/s", "knife/knives", "fork/forks", "spoon/s/spoonful", "cloth/es/clothing", "treasure/s", "guest/s",  "war/wars/battle/battles/battlefield", "animal/s", "pillow/s", "food/feed/breakfast/s/lunch/es/dinner/s/supper/s", "beam/s", "robber/s", "monster/s", "tooth/teeth", "music/musician", "donkey/s", "bit/bite/bites/bitten", "club/s", "stick/s", "storm/s/y", "shelter/s/ed", "drink/drunk/drinks/drunken", "meadow/s", "branch/es", "ring/s",  "cry/cries/cried/weep/wept/", "snow/s/snowflake/s", "rain/s", "heart/s", "love/lover/loved/loves", "country/countryside", "land/s", "little/big", "duck/s", "or/and/but", "also/too", "thirsty/drink", "wren/s", "nest/s", "egg/s",  "one/two/three/four/five/six/seven/eight/nine/ten/eleven/twelve/thirteen", "ones/twos/threes/fours/fives/sixes/sevens/eights/nines/tens/elevens/twelves/thirteens", "church/es", "lip/s", "half/whole", "first/second/third/fourth/fifth/sixth/seventh/eighth/ninth/tenth/eleventh/twelfth/thirteenth",  "hundred/s", "thousand/s", "heaven/s", "heavy", "wind/y", "poor/rich", "mile/s" ))
  {
  all.words = c();
  for(g in 1:length(grimms))
    {
    sub.g = strsplit(grimms[g],"/",fixed=TRUE)[[1]];
    n.g = length(sub.g);
    sub.previous = NULL;
    for(i in 1:n.g)
      {
      s = sub.g[i];
      if(i > 1)
        {
        s.n = nchar(s);
        if(s.n < 3) # we have a token-stem
          {
          all.words = c(all.words, paste0(sub.previous,s)); # append
          } else {
                  all.words = c(all.words, s); # next one
                  sub.previous = s;
                  }
        } else {
                all.words = c(all.words, s); # first one
                sub.previous = s;
                }
      }
    #
    }
  all.words;
  }


setupTags = function()
  {
  # https://www.sketchengine.eu/penn-treebank-tagset/ 
# https://stackoverflow.com/questions/1833252/java-stanford-nlp-part-of-speech-labels#1833718  
  
  # all.tags = NLP::Penn_Treebank_POS_tags;
  # all.tags[,1:2];
  stop.tags = c(":", "."); # hard stops ... 
  # https://catalog.ldc.upenn.edu/docs/LDC95T7/cl93.html
  # ? NLP::Penn_Treebank_POS_tags
  #   
  # do I have all.tags?
  skip.tags = c(",", "``", "''","$","(",")","-","LS");
  # "," comma (e.g., Oxford comma) maybe is a pause ...
  # update classifier to separate intent? Or maybe it already is...
  # "CC" as FANBOYS ... https://www.thoughtco.com/coordinating-conjunction-grammar-1689929
  tags = list(
          "noun" = c("NN", "NNS"),
          "pron" = c("PP", "WP"),
          "proper" = c("NNP", "NNPS"),
          "verb" = c("VB", "VBD", "VBG", "VBN", "VBP", "VBZ", # similar to :: https://arxiv.org/pdf/1104.2086.pdf
                      "VH", "VHD", "VHG", "VHN", "VHP", "VHZ", 
                      "VV", "VVD", "VVG", "VVN", "VVP", "VVZ"), # some of these are not in this implementation
          "aux" = c("TO", "MD"),
                      # "to" as preposition or infinitive marker
                      # modal auxiliary:: can, could, may, might, must, ought, shall, should, will, and would.
          "adj" = c("JJ","JJR","JJS"),
          "adv" = c("RB", "RBR", "RBS", "WRB"),
          "number" = c("CD"),
          "conj" = c("CC"), # conjunction, coordinating
          "foreign" = c("FW"),
          "det" = c("DT", "PDT", "WDT"),  # WH-determiner
          "prep" = c("IN"), # preposition or conjunction, subordinating ... why not separate?
          "intj" = c("UH"),
          "there" = c("EX"),
          "poss" = c("POS", "PPZ", "WP$", "PRP$")
              );
  tags.keyed = keyMyTags(tags);  # reverse keyed, faster lookup
  
  
  tags.info = list("stop" = stop.tags, "skip" = skip.tags,
                    "tags" = tags, "tags.keyed" = tags.keyed);
  
  tags.info;
  }

keyMyTags = function(tags)
  {
  nam = names(tags);
  res = list();
  for(na in nam)
    {
    ta = tags[[na]];
    for(tag in ta)
      {
      res[[tag]] = na;
      }
    }
  res;
  }
  
  

countGenderLanguage = function(words) # words is vector, in order
  {
  words.lower = tolower(words);
  # bag of words, order doesn't matter
  words.table = as.data.frame( sort(table(words.lower),decreasing = TRUE));
    colnames(words.table) = c("word","count");
  my.gender = NULL;

  g.person = "male";
  g.number = "singular";
  genders = c("he", "him", "his", "himself", "man", "boy", "king", "prince", "son", "father", "dad", "daddy", "fatherhood", "brother", "godfather", "gentleman", "huntsman", "fisherman", "groom", "husband");
  
  for(g in 1:length(genders))
    {
    lookup = subsetDataFrame(words.table, "word", "==", genders[g]);
    lookup.count = 0;
    if(dim(lookup)[1] == 1) { lookup.count = lookup$count; }
    row = c(g.person,g.number,genders[g],lookup.count);
    my.gender = rbind(my.gender,row);
    }
  
  g.person = "male";
  g.number = "plural";  # plural may be capturing possessive "father's"?
  genders = c("men", "boys", "kings", "princes", "sons", "fathers", "dads", "daddies", "brothers", "brethren", "brotherhood", "godfathers", "gentlemen", "huntsmen", "fishermen", "husbands");
  
  for(g in 1:length(genders))
    {
    lookup = subsetDataFrame(words.table, "word", "==", genders[g]);
    lookup.count = 0;
    if(dim(lookup)[1] == 1) { lookup.count = lookup$count; }
    row = c(g.person,g.number,genders[g],lookup.count);
    my.gender = rbind(my.gender,row);
    }
  
  g.person = "female";
  g.number = "singular";
  genders = c("she", "her", "herself", "woman", "girl", "queen", "princess", "daughter", "mother", "mom", "mommy", "motherhood", "sister", "godmother", "lady", "maid", "maiden", "bride");
  
  for(g in 1:length(genders))
    {
    lookup = subsetDataFrame(words.table, "word", "==", genders[g]);
    lookup.count = 0;
    if(dim(lookup)[1] == 1) { lookup.count = lookup$count; }
    row = c(g.person,g.number,genders[g],lookup.count);
    my.gender = rbind(my.gender,row);
    }
  
  g.person = "female";
  g.number = "plural";
  genders = c("women", "girls", "queens", "princesses", "daughters", "mothers", "moms", "mommies", "sisters", "sisterhood", "godmothers", "ladies", "maids", "maidens");
  
  for(g in 1:length(genders))
    {
    lookup = subsetDataFrame(words.table, "word", "==", genders[g]);
    lookup.count = 0;
    if(dim(lookup)[1] == 1) { lookup.count = lookup$count; }
    row = c(g.person,g.number,genders[g],lookup.count);
    my.gender = rbind(my.gender,row);
    }
  
  
  
  
  
    
  cnames = c("gender","number","word","count");
  
  my.gender = as.data.frame(my.gender);
    rownames(my.gender) = NULL;
    colnames(my.gender) = cnames;
  
  my.gender$count = as.numeric(my.gender$count);
  
  my.gender;
  }

countPersonalPronouns = function(words) # words is vector, in order
  {
  words.lower = tolower(words);
  # bag of words, order doesn't matter
  words.table = as.data.frame( sort(table(words.lower),decreasing = TRUE));
    colnames(words.table) = c("word","count");
    
  my.pronouns = NULL;
  
  pr.person = "first";
  pr.number = "singular";
  pronouns = c("i", "me", "my", "mine", "myself");
  for(p in 1:length(pronouns))
    {
    lookup = subsetDataFrame(words.table, "word", "==", pronouns[p]);
    lookup.count = 0;
    if(dim(lookup)[1] == 1) { lookup.count = lookup$count; }
    row = c(pr.person,pr.number,pronouns[p],lookup.count);
    my.pronouns = rbind(my.pronouns,row);
    }
  
  pr.person = "first";
  pr.number = "plural";
  pronouns = c("we", "us", "our", "ours", "ourselves");
  for(p in 1:length(pronouns))
    {
    lookup = subsetDataFrame(words.table, "word", "==", pronouns[p]);
    lookup.count = 0;
    if(dim(lookup)[1] == 1) { lookup.count = lookup$count; }
    row = c(pr.person,pr.number,pronouns[p],lookup.count);
    my.pronouns = rbind(my.pronouns,row);
    }
  
  
  pr.person = "second";
  pr.number = "singular";
  pronouns = c("you", "you", "your", "yours", "yourself");
  for(p in 1:length(pronouns))
    {
    lookup = subsetDataFrame(words.table, "word", "==", pronouns[p]);
    lookup.count = 0;
    if(dim(lookup)[1] == 1) { lookup.count = lookup$count; }
    row = c(pr.person,pr.number,pronouns[p],lookup.count);
    my.pronouns = rbind(my.pronouns,row);
    }
  
  pr.person = "second";
  pr.number = "plural";
  pronouns = c("you", "you", "your", "yours", "yourselves");
  for(p in 1:length(pronouns))
    {
    lookup = subsetDataFrame(words.table, "word", "==", pronouns[p]);
    lookup.count = 0;
    if(dim(lookup)[1] == 1) { lookup.count = lookup$count; }
    row = c(pr.person,pr.number,pronouns[p],lookup.count);
    my.pronouns = rbind(my.pronouns,row);
    }
  
  pr.person = "third";
  pr.number = "singular";
  pronouns = c("he", "him", "his", "his", "himself");
  for(p in 1:length(pronouns))
    {
    lookup = subsetDataFrame(words.table, "word", "==", pronouns[p]);
    lookup.count = 0;
    if(dim(lookup)[1] == 1) { lookup.count = lookup$count; }
    row = c(pr.person,pr.number,pronouns[p],lookup.count);
    my.pronouns = rbind(my.pronouns,row);
    }
  
  pr.person = "third";
  pr.number = "singular";
  pronouns = c("she", "her", "her", "hers", "herself");
  for(p in 1:length(pronouns))
    {
    lookup = subsetDataFrame(words.table, "word", "==", pronouns[p]);
    lookup.count = 0;
    if(dim(lookup)[1] == 1) { lookup.count = lookup$count; }
    row = c(pr.person,pr.number,pronouns[p],lookup.count);
    my.pronouns = rbind(my.pronouns,row);
    }
  pr.person = "third";
  pr.number = "singular";
  pronouns = c("it", "it", "its", NA, "itself");
  for(p in 1:length(pronouns))
    {
    lookup = subsetDataFrame(words.table, "word", "==", pronouns[p]);
    lookup.count = 0;
    if(dim(lookup)[1] == 1) { lookup.count = lookup$count; }
    row = c(pr.person,pr.number,pronouns[p],lookup.count);
    my.pronouns = rbind(my.pronouns,row);
    }
  
  pr.person = "third";
  pr.number = "plural";
  pronouns = c("they", "them", "their", "theirs", "themself", "themselves");
  for(p in 1:length(pronouns))
    {
    lookup = subsetDataFrame(words.table, "word", "==", pronouns[p]);
    lookup.count = 0;
    if(dim(lookup)[1] == 1) { lookup.count = lookup$count; }
    row = c(pr.person,pr.number,pronouns[p],lookup.count);
    my.pronouns = rbind(my.pronouns,row);
    }
  
  
    pn = dim(my.pronouns)[1];
  rnames = c("subject","object","determiner","independent","reflexive");
    rn = length(rnames);
    gn = (floor(pn/rn));
  rnames.all = c();  
  for(g in 1:gn)
    {
    rnames.all = c(rnames.all, paste0(rnames,"-",g));
    }
    rnames.all = c(rnames.all, paste0("reflexive.b-",g));
    
    length(rnames.all);
  cnames = c("person","number","word","count");
  
  my.pronouns = as.data.frame(my.pronouns);
    rownames(my.pronouns) = rnames.all;
    colnames(my.pronouns) = cnames;
  
  my.pronouns$count = as.numeric(my.pronouns$count);
  
  my.pronouns;
  }
  

countVowelsInString = function(str)
  {
  vowels = c("a","e","i","o","u","y");
  
  str.vec = strsplit(str,"")[[1]];
  str.vec;
  
  is.vowel = is.element(tolower(str.vec), vowels);
  n.vowels = sum(is.vowel);
  n.vowels;
  }

computeReadability = function(n.sentences, syllables=NULL)
  {
  n.words = length(syllables);
  n.syllables = 0;
  for(i in 1:n.words)
    {
    my.syllable = syllables[[i]];
    n.syllables = my.syllable$syllables + n.syllables;
    }
  
  if(n.sentences == 0) { stop("zero sentences not allowed!"); }
  if(n.words == 0) { stop("zero words not allowed!"); }
  
  # Flesch Reading Ease (FRE):
  FRE = 206.835 - 1.015 * (n.words/n.sentences) - 84.6 * (n.syllables/n.words);
  # Flesh-Kincaid Grade Level (FKGL):
  FKGL = 0.39 * (n.words/n.sentences) + 11.8 * (n.syllables/n.words) - 15.59; 
  # FKGL = -0.384236 * FRE - 20.7164 * (n.syllables/n.words) + 63.88355;
  # FKGL = -0.13948  * FRE + 0.24843 * (n.words/n.sentences) + 13.25934;
  
  list("FRE" = FRE, "FKGL" = FKGL, "syllables" = n.syllables, "words" = n.words); 
  }



# https://en.wikipedia.org/wiki/Gunning_fog_index
# THIS is a CLASSIFIER PROBLEM ...
# https://stackoverflow.com/questions/405161/detecting-syllables-in-a-word
countSyllablesInWord = function(words)
  {
  #word = "super";
  n.words = length(words);
  result = list();
  for(j in 1:n.words)
    {
    word = words[j];
    vowels = c("a","e","i","o","u","y");
    
    word.vec = strsplit(word,"")[[1]];
    word.vec;
    
    n.char = length(word.vec);
    
    is.vowel = is.element(tolower(word.vec), vowels);
    n.vowels = sum(is.vowel);
    
    
    # nontrivial problem 
    if(n.vowels <= 1)
      {
      syllables = 1;
      str = word;
      } else {
              # syllables = 0;
              previous = "C";
              # on average ? 
              str = "";
              n.hyphen = 0;
        
              for(i in 1:n.char)
                {
                my.char = word.vec[i];
                my.vowel = is.vowel[i];
                if(my.vowel)
                  {
                  if(previous == "C")
                    {
                    if(i == 1)
                      {
                      str = paste0(my.char, "-");
                      n.hyphen = 1 + n.hyphen;
                      } else {
                              if(i < n.char)
                                {
                                if(n.vowels > (n.hyphen + 1))
                                  {
                                  str = paste0(str, my.char, "-");
                                  n.hyphen = 1 + n.hyphen;
                                  } else {
                                           str = paste0(str, my.char);
                                          }
                                } else {
                                        str = paste0(str, my.char);
                                        }
                              }
                     # syllables = 1 + syllables;
                     previous = "V";
                    } else {  # "VV"
                          # assume what  ?  vowel team?
                          str = paste0(str, my.char);
                          }
            
                } else {
                            str = paste0(str, my.char);
                            previous = "C";
                            }
                #
                }
        
              syllables = 1 + n.hyphen;
              }
  
      result[[j]] = list("syllables" = syllables, "vowels" = n.vowels, "word" = str);
      }
  
  if(n.words == 1) { result[[1]]; } else { result; }
  }

################ hackathon #######
# http://www.speech.cs.cmu.edu/cgi-bin/cmudict
# http://www.syllablecount.com/syllables/


  # https://enchantedlearning.com/consonantblends/index.shtml
  # start.digraphs = c("bl", "br", "ch", "cl", "cr", "dr", 
  #                   "fl", "fr", "gl", "gr", "pl", "pr",
  #                   "sc", "sh", "sk", "sl", "sm", "sn",
  #                   "sp", "st", "sw", "th", "tr", "tw",
  #                   "wh", "wr");
  # start.trigraphs = c("sch", "scr", "shr", "sph", "spl",
  #                     "spr", "squ", "str", "thr");
  # 
  # 
  # 
  # end.digraphs = c("ch","sh","th","ng","dge","tch");
  # 
  # ile
  # 
  # farmer
  # ar er
  # 
  # vowel teams ... beaver1
  # 
  # 
  # # "able"
  # # http://www.abcfastphonics.com/letter-blends/blend-cial.html
  # blends = c("augh", "ough", "tien", "ture", "tion", "cial", "cian", 
  #             "ck", "ct", "dge", "dis", "ed", "ex", "ful", 
  #             "gh", "ng", "ous", "kn", "ment", "mis", );
  # 
  # glue = c("ld", "st", "nd", "ld", "ng", "nk", 
  #           "lk", "lm", "lp", "lt", "ly", "mp", "nce", "nch", 
  #           "nse", "nt", "ph", "psy", "pt", "re", )
  # 
  # 
  # start.graphs = c("bl, br, ch, ck, cl, cr, dr, fl, fr, gh, gl, gr, ng, ph, pl, pr, qu, sc, sh, sk, sl, sm, sn, sp, st, sw, th, tr, tw, wh, wr");
  # 
  # # https://mantra4changeblog.wordpress.com/2017/05/01/consonant-digraphs/
  # digraphs.start = c("ch","sh","th","wh","ph","qu");
  # digraphs.end = c("ch","sh","th","ng","dge","tch");
  # # https://www.education.com/worksheet/article/beginning-consonant-blends/
  # blends.start = c("pl", "gr", "gl", "pr",
  #                 
  # blends.end = c("lk","nk","nt",
  # 
  # 
  # # https://sarahsnippets.com/wp-content/uploads/2019/07/ScreenShot2019-07-08at8.24.51PM-817x1024.png
  # # Monte     Mon-te
  # # Sophia    So-phi-a
  # # American  A-mer-i-can
  # 
  # n.vowels = 0;
  # for(i in 1:n.char)
  #   {
  #   my.char = word.vec[i];
  # 
  # 
  # 
  # 
  # 
  # n.syll = 0;
  # str = "";
  # 
  # previous = "C"; # consonant vs "V" vowel
  # 
  # for(i in 1:n.char)
  #   {
  #   my.char = word.vec[i];
  #   
  #   my.vowel = is.element(tolower(my.char), vowels);
  #   if(my.vowel)
  #     {
  #     n.vowels = 1 + n.vowels;
  #     if(previous == "C")
  #       {
  #       if(i == 1)
  #         {
  #         str = paste0(my.char, "-");
  #         } else {
  #                 if(n.syll > 1)
  #                   {
  #                   str = paste0(str, "-", my.char);
  #                   } else {
  #                          str = paste0(str, my.char);
  #                         }
  #                 }
  #        n.syll = 1 + n.syll;
  #        previous = "V";
  #       } 
  #     
  #   } else {
  #               str = paste0(str, my.char);
  #               previous = "C";
  #               }
  #   #
  #   }
  # 
  # 
  # 
  # 
## https://jzimba.blogspot.com/2017/07/an-algorithm-for-counting-syllables.html
# AIDE   1
# IDEA   3
# IDEAS  2
# IDEE   2
# IDE   1
# AIDA   2
# PROUSTIAN 3
# CHRISTIAN 3
# CLICHE  1
# HALIDE  2
# TELEPHONE 3
# TELEPHONY 4
# DUE   1
# IDEAL  2
# DEE   1
# UREA  3
# VACUO  3
# SEANCE  1
# SAILED  1
# RIBBED  1
# MOPED  1
# BLESSED  1
# AGED  1
# TOTED  2
# WARRED  1
# UNDERFED 2
# JADED  2
# INBRED  2
# BRED  1
# RED   1
# STATES  1
# TASTES  1
# TESTES  1
# UTILIZES  4
