rowMaxColumn = function(df)
  {
  nrow = nrow(df);
  rmax = numeric(0);
  for(i in 1:nrow)
    {
    row = df[i, ];
    rmax[i] = which.max(row); # downward biased
    }
  rmax;
  }

rowMax = function(df)
  {
  nrow = nrow(df);
  rmax = numeric(0);
  for(i in 1:nrow)
    {
    row = df[i, ];
    rm = max(row, na.rm=TRUE);
    rmax[i] = rm;
    }
  rmax;
  }





# # https://stackoverflow.com/questions/28687806/a-better-way-to-push-and-pop-to-from-lists-in-r
                
pushList = function(nlist, veclist, n=length(vlist))
  {
  
  }
popList = function(nlist, veclist, n=length(vlist))
  {
  
  }

# https://stackoverflow.com/questions/2805102/how-is-pushing-and-popping-defined
# vec = 1: 10;
# popVector(vec)
# popVector(vec, method="LIFO-LILO")
popVector = function(vec, idx=1, method="FIFO")  # Inf would work for unlimited stack
  {
  if(method=="FIFO")   # QUEUING
    {
    val = vec[idx];  
    vec = vec[-c(idx)];
    } else {
            n = length(vec) + 1 - idx;
            val = vec[n];
            vec = vec[-c(n)];
            }
  list("val" = val, "vec" = vec, "popped" = NULL); # updated ...
  }

# vec = 1: 10;
# popVector(pushVector(13, vec)$vec)
# pushVector(13, vec, n.max=5)
# pushVector(13, vec, n.max=5, method="LIFO-LILO")
pushVector = function(val, vec, n.max=1+length(vec), method="FIFO")
  {
  # n.max is max size, so vals popped may return ...
  n = length(vec);
  popped = NULL;
  if(method=="FIFO")  # in this model, new values are added to end
    {
    if(n < n.max)
      {
      vec = c(vec,val);
      } else {
              vec = c(vec,val);
              nn = 1 + n;
              nd = nn - n.max;
              if(nd > 0)
                {
                popped = vec[1:nd];
                vec = vec[(1+nd):nn];
                }
              }
    } else {        # in this model, new values are added to beginning
            if(n < n.max)
              {
              vec = c(val,vec);
              } else {
                      vec = c(val,vec);
                      nn = 1 + n;
                      if(nn > (1+n.max))
                        {
                        popped = vec[(1+n.max):nn];  # off the end ?
                        vec = vec[1:n.max];
                        }
                      }
            }
  list("vec" = vec, "popped" = popped, "val"=val); 
  }

 
initGrams = function(n,gram.types)
  {
  grams = list();
  
  for(gram.type in gram.types)
    { 
    grams[[gram.type]] = list();
    for(i in 1:n) 
      {
      grams[[gram.type]][[i]] = character();
      }
    }
    # grams[[i]] = list("disjoint" = list(), 
    #                   "inflated" = list(),
    #                   "variants" = list(),  
    #                   "pos" = list()
    #   
    #               );  # add stemmed? variants? inflation?
    #                   # replace with GENERIC if stop word
    
  grams;
  }
    

    # my.stack = c();   # words  ... we will STEM at last minute ...
    # my.stack.t = c(); # tags  ... NN
    # my.stack.ts = c(); # tags.simple  ... noun
initStack = function(gram.types)
  { 
  my.stack = list();
  for(gram.type in gram.types)
    {
    my.stack[[gram.type]] = list("vec" = NULL, "popped" = NULL, "val" = NULL);
    }
  my.stack;
  }

resetStackElement = function(my.stack, gram.type)
  {
  my.stack[[gram.type]] = list("vec" = NULL, "popped" = NULL, "val" = NULL);
  my.stack;
  }
 
# gvec = c("monte","says","hi","to","his","son","alex");
# gramCombinations(gvec)
gramCombinations = function(gvec, nv = length(gvec), sep="*")
  { 
 # print(gvec);
  # return all permutations ... maintain order ... create as a list of each length
  result = list();
  result[[1]] = gvec;
  if(nv > 2)
    {
    # for(c in 2:(nv-1))
    for(c in 2:nv)
      {
      # nvec = combn(1:nv, c);  # numeric ... we will subset if they are not adjacent
      cvec = combn(gvec, c);  # loop over columns, store previous, if new, we add ...
      #cvec;
      
      res = paste0(cvec[,1],collapse=sep);
      previous = cvec[1,1];
      nc = ncol(cvec);
      if(nc > 1)
        {
        for(col in 2: nc)
          {
          current = cvec[1,col];
          if(current != previous)
            {
            res = c(res, paste0(cvec[,col],collapse=sep) );
            previous = current;
            }
          }
        }
      #res;
      
      
      
      result[[c]] = res;
      }
    }
    # result[[nv]] = paste0(gvec,collapse=sep);
    # print(result[[nv]]);
  result;
  }

# if updateGrams is called, that means the current stack is good-to-go
# therefore, we will popVector
updateGrams = function(grams, my.stack, which="ALL", tags.info, do.stemming=TRUE)  
  {
  gram.types = names(my.stack);
  if(which != "ALL") { gram.types = gram.types[ which(gram.types == which) ];}
  for(gram.type in gram.types)  # gram.type = gram.types[1]
    {
    # do something here ... 
    # print(gram.type);
    my.vec = my.stack[[gram.type]]$vec;
    #nv = length(my.vec);  # if 5, we want to return, 5,4,3,2,1 ... disjoint combos
                          # or we could just return 5 ... EASIEST, largest gram found
    
    if(!is.null(my.vec)) # back-to-back stop words?
      {
      my.str = gramCombinations(my.vec); 
      ns = length(my.str);
      for(i in 1:ns)
        {
        nsd = length(my.str[[i]]);
        for(j in 1:nsd)
          {
          # print(my.str[[i]][j]);
          grams[[gram.type]][[i]] = c(grams[[gram.type]][[i]], my.str[[i]][j]);
          }
        }
      
      # popVector
      my.stack[[gram.type]] = popVector(my.stack[[gram.type]]$vec);
      }
    }
  list("grams" = grams, "my.stack" = my.stack);
  }



updateGramsIfMax = function(n, grams, my.stack, which="ALL", tags.info, do.stemming)
  {
  # n is maximum number of grams ...
  gram.types = names(my.stack);
  if(which != "ALL") { gram.types = gram.types[[which]];}
  for(gram.type in gram.types)
    {
    # do  something here ... 
    my.vec = my.stack[[gram.type]]$vec;
    if(length(my.vec) == n) # can't be greater than?
      {
      ginfo = updateGrams(grams, my.stack, which=gram.type, tags.info, do.stemming);  
        grams    = ginfo$grams;
        my.stack = ginfo$my.stack;
      }
    
    }
  
  list("grams" = grams, "my.stack" = my.stack);
  }
                    

# if(length(my.stack$vec) == n)
#                   {
#                   ginfo = updateGrams(grams, my.stack, "default", tags.info, do.stemming); #
#                   grams = ginfo$grams;
#                   pinfo = popVector(my.stack$vec);
#                   my.stack$vec = pinfo$vec; pval = pinfo$val;
#                   }


