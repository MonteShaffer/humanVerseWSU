
#' callOrderFunctionWithMatrixInput
#'
#' See \url{https://stackoverflow.com/questions/63801018/}
#'
#' @family Sort
#'
#' @param mat matrix
#'
#' @return this is placed in a dataframe to re-order it.
#' @export
#'
callOrderFunctionWithMatrixInput = function(mat)
	{
	do.call(order, split(mat, (seq(mat) - 1) %/% nrow(mat)));
	}




## sortMe.base
## sortMe.selection  ... min-max
## sortMe.bubble     ... swap-meet
## sortMe.insertion  ... laundrey

########## to understand the nature of sort and complexity
sortMe.base = function(vec, verbose = TRUE, timings = TRUE)
  {
  # radix for short, "shell" for other
  # short is 2^31 ... that is about 2 billion

  start = as.numeric(Sys.time());
  nvec = sort(vec);
  end = as.numeric(Sys.time());
  elapse = end-start;
  bigO = n*log(n);

  if(verbose)
  {
  print(paste0("(base::sort) Time elapsed: ", round(elapse,2) ));
  print( bigO );
  }


  if(timings)
    {
    list("time" = elapse, "complexity" = bigO, "sort" = nvec);
    } else { nvec; }
  }

# { set.seed(123); vec=sample(1:5); }
sortMe.selection = function(vec, verbose=TRUE, timings=TRUE)
  {
  # set.seed(123); vec=sample(1:6);
  # copy of vec + 10 elements

  # min-max
  start = as.numeric(Sys.time());
  nvec = NA*vec;
  n = length(vec);

    min.n = 1;
    max.n = n;

  for(j in 1:n)
    {
  # loop to find min (and max)
  mymin = NA;  min.idx = NA;
  mymax = NA;  max.idx = NA;
  for(i in 1:n)
    {

    myval = vec[i];
    if(!is.na(myval))
      {
      ##########  min
      if(is.na(mymin))
        {
        mymin = myval; min.idx=i;
        } else {
                if(myval < mymin) { mymin = myval; min.idx=i; }
                }
      ##########  max
      if(is.na(mymax))
        {
        mymax = myval; max.idx=i;
        } else {
                # if the max's are tied, this will grab the natural ordered max
                if(myval >= mymax) { mymax = myval; max.idx=i; }
                }
      }

    if(verbose)
      {
      print(paste0("j -->", j, " i: ",i, " ... mymin: ",mymin, " [",min.idx,"]",
                          " ... mymax: ",mymax, " [",max.idx,"]" ));
      #print(paste0("j -->", j, " i: ",i, " ... vec: ", paste0(vec, collapse=","), "\n",
      #                                  " ... nvec: ", paste0(nvec, collapse=",") ) );
      }
    }
   # i loop
      if(!is.na(mymin) && !is.na(mymax))
        {
        if(!is.na(mymin))
          {
          nvec[min.n] = mymin;  min.n = 1 + min.n;
            vec[min.idx] = NA;
          }
        if(!is.na(mymax))
          {
          nvec[max.n] = mymax;  max.n = max.n - 1;
            vec[max.idx] = NA;
          }
        if(verbose)
          {
          print(vec); print(nvec);
          }
        } else { break; }



    }
  # j loop
    if(verbose)
  {
  print("################# END ################");
  print(vec); print(nvec);
    }
  end = as.numeric(Sys.time());
  elapse = end-start;
  bigO = n^2;
  bigO.a = j * n;  # this is when we break out of the loop
  if(verbose)
  {
  print(paste0("(myFunction) Time elapsed: ", round(elapse,2) ));
  print( bigO );
  }


  if(timings)
    {
    list("time" = elapse, "bigO" = bigO,  "bigO.a" = bigO.a, "sort" = nvec);
    } else { nvec; }
  }




sortMe.insertion = function(vec, verbose=TRUE, timings=TRUE)
  {
  start = as.numeric(Sys.time());
  n = length(vec);
  # create a new bucket, place in order every time ...
  # sorting laundry... everything out of basket onto bed in sorted "piles"

  nvec = c();
  for(i in 1:n)
    {
    myval = vec[i];
    if(length(nvec) == 0)
        {

        nvec = c(myval);
        # print(nvec);

        } else {
                before = c();
                after = c();
                foundval = FALSE;
                for(j in 1:length(nvec))
                  {
                  cval = nvec[j]; # comparison val ... already in nvec ...

################
if(verbose)
  {
  myBefore = "";
  if(length(before) > 0)
    {
    myBefore = paste0(before, collapse=",");
    }
  }
################

                if(isFALSE(foundval))
                {
                if(myval < cval)
                    {
                    foundval = myval;
                    after = c(after, cval);
                    } else {
                          before = c(before, cval);
                          }
                } else {
                        after = c(after, cval);
                       }

   if(verbose)
  {
                    print(paste0("i -->", i, " j: ",j,  " . myval: ", myval,
                                              " . cval: ", cval,
                                              " . foundval: ", foundval,
                        " .. before: ", paste0(before, collapse=","),
                        " .. after: ", paste0(after, collapse=","),
                        " ... vec: ", paste0(vec, collapse=","),
                        " ... nvec: ", paste0(nvec, collapse=",")     ) ); #,
                        # " ... vec: ", paste0(vec, collapse=",") ) );

   }


                  }
                nvec = c(before, myval, after );
                #nvec = c(before, myval, setdiff(nvec,before) );  # cheating, continue the loop and store "after"
                }

    }


   if(verbose)
  {
  print("################# END ################");
  print(vec); print(nvec);
   }


  end = as.numeric(Sys.time());
  elapse = end-start;
  bigO = n^2;
  bigO.a = j * n;  # this is when we break out of the loop
  if(verbose)
  {
  print(paste0("(myFunction) Time elapsed: ", round(elapse,2) ));
  print( bigO );
  }


    if(timings)
    {
    list("time" = elapse, "bigO" = bigO,  "bigO.a" = bigO.a, "sort" = nvec);
    } else { nvec; }
  }

# set.seed(123); vec=sample(1:6);
sortMe.bubble = function(vec, verbose=TRUE, timings=TRUE)
  {
  # swap-me algorithm ...
  # copy + 7 elements
  start = as.numeric(Sys.time());
  n = length(vec);

  swapMe = function(a,a.idx, b,b.idx, nvec)
    {
    nvec[b.idx] = a;
    nvec[a.idx] = b;
    nvec;
    }

  nvec = vec; # copy
  # use previous ...
  for(j in 1:n)
    {
  nswap = 0;
  #print("original");print(vec);
  for(i in 2:n)
    {
    a.idx = i-1; a = nvec[a.idx];
    b.idx = i;   b = nvec[b.idx];
    if(a > b) { nvec = swapMe(a,a.idx, b,b.idx, nvec); nswap = 1 + nswap;}

    #print("swap");print(nvec);

    if(verbose)
      {
      print(paste0("j -->", j, " i: ",i, " ... original: ", paste0(vec, collapse=","), "\n",
                                        " ... swapped: ", paste0(nvec, collapse=",") ) );
      }

    }



  if(nswap == 0) { break; }   # 3 6 2 4 5 1
    }

   if(verbose)
  {
  print("################# END ################");
  print(vec); print(nvec);
   }

  end = as.numeric(Sys.time());
  elapse = end-start;
  bigO = n^2;
  bigO.a = j * n;  # this is when we break out of the loop
  if(verbose)
  {
  print(paste0("(myFunction) Time elapsed: ", round(elapse,2) ));
  print( bigO );
  }


    if(timings)
    {
    list("time" = elapse, "bigO" = bigO,  "bigO.a" = bigO.a, "sort" = nvec);
    } else { nvec; }
  }
