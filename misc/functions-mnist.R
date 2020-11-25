
 
colSDs = function(df)
  {
  df = as.matrix(df);
  #print(df);
  cs = ncol(df);
  res = c();
  for(c in 1:cs)
    {
    xx = na.omit(df[,c]);
    s = sd(xx);
    res = c(res,s);
    }
  res;
  }

rowZs = function(df)
  {
  df = as.matrix(df);
  
  m = rowMeans(df);
  sd = rowSDs(df);
  
  rs = nrow(df);
  res = c();
  for(r in 1:rs)
    {
    xx = na.omit(df[r,]);
    z = (xx - m[r]) / sd[r];
    res = c(res,z);
    }
  res;
  }

rowSDs = function(df)
  {
  df = as.matrix(df);
  #print(df);
  rs = nrow(df);
  res = c();
  for(r in 1:rs)
    {
    xx = na.omit(df[r,]);
    s = sd(xx);
    res = c(res,s);
    }
  res;
  }



colMedians = function(df, method="default")
  {
  df = as.matrix(df);
  #print(df);
  cs = ncol(df);
  res = c();
  for(c in 1:cs)
    {
    xx = na.omit(df[,c]);
    #print(xx);
    if(method == "default")
      {
      m = median(xx);
      } else {
              m = as.numeric( stats::quantile(xx, 
                              prob=c(0.5), type=method)); # I prefer 1
              }
    #print(paste0("r: ",r," --> ",m));
    res = c(res,m);
    }
  res;
  }

rowMedians = function(df, method="default")
  {
  df = as.matrix(df);
  #print(df);
  rs = nrow(df);
  res = c();
  for(r in 1:rs)
    {
    xx = na.omit(df[r,]);
    #print(xx);
    if(method == "default")
      {
      m = median(xx);
      } else {
              m = as.numeric( stats::quantile(xx, 
                              prob=c(0.5), type=method)); # I prefer 1
              }
    #print(paste0("r: ",r," --> ",m));
    res = c(res,m);
    }
  res;
  }

# https://stackoverflow.com/questions/7719741/how-to-test-if-list-element-exists#50538644
element.exists <- function(var, element)
{
  tryCatch({
    if(length(var[[element]]) > -1)
      return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

# ? is.integer
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)
  {
  abs(x - round(x)) < tol;
  }

library(stringi);
library(magick);
### image.magick helper

im.printImageMatrix = function(matr, split=1, raw=FALSE, raw.r="X")
  {
  rs = nrow(matr); rsplit = floor(rs/split);
  cs = ncol(matr); csplit = floor(cs/split);
   
  nmatr = matrix(NA, nrow=rs+(split-1),ncol=1);
  info = ceiling(as.matrix(matr));
  if(raw)
    {
    info = floor( 10 * round(as.matrix(matr), 1) );
    info[info == 10] = raw.r;
    }
 info[is.na(info)] = ".";
  
  i = 1;
  for(r in 1:rs)
    {
    row = paste0(info[r,], collapse="");
    # https://stackoverflow.com/questions/32398301/
    # library(stringi); stri_extract_all_regex(str1, '.{1,3}')
    row.s = stri_extract_all_regex(row, paste0('.{1,',csplit,'}') )[[1]];
    nmatr[i] = paste0(row.s, collapse="|");
    i = 1 + i;
    if(i > (rs+(split-1)) ) { break; }
    if(r %% rsplit == 0) 
      { 
      row = paste0(rep("-", times=(rs+(split-1))), collapse="");  
      row.s = stri_extract_all_regex(row, paste0('.{1,',csplit,'}') )[[1]];
      my.row = paste0(row.s, collapse="+");
      nmatr[i] = substr(my.row,1, (cs+(split-1)) );
      i = 1 + i; 
      }
    }
  print(nmatr);
  }

# https://github.com/ropensci/magick/issues/154
im.image_content = function(x, ...)
  {
  x = image_data(x, ...);
  as.integer(x);
  }

im.getMatrixFromImage = function(img, dims=2, scale=255)
  {
  img.matrix = im.image_content(img);
  
  if(dims == 2)
    {
    img.matrix = as.matrix(img.matrix[,,1]);
    }
  if(!is.null(scale))
    {
    img.matrix = img.matrix/scale;
    }
  img.matrix;  
  }


#### mnist helper functions
  
# matr = img.matrix.c;
mnist.countMineSweepMoves = function(matr) # nice idea harrison
  {
  mineSweep = function(matr, r_=1, c_=1)
    {
    nrow = nrow(matr);
    ncol = ncol(matr);
    matr[r_,c_] = NA; 

    i = 1;
    adjs = list();
    adjs[[i]] = c(r_,c_);
    # let's replace all nearest neighbors "0" with "Inf", 
    #  get their r,c indexes
    # add to adjs list
    # replace Inf with NA
    while( (n=length(adjs)) > 0)
      {
      for(j in 1:n)
        {
        if(!is.null(adjs[[j]])) { break; }  # find a remaining "j"
        }
      r = adjs[[j]][1];
      c = adjs[[j]][2];
        matr[r,c] = NA; 
        adjs[[j]] = NULL;
      
      rows = c(r - 1, r, r + 1);
      cols = c(c - 1, c, c + 1);
      
      for(row in rows)
        {
        if(row > 0 && row <= nrow)
          {
          for(col in cols)
            {
            if(col > 0 && col <= ncol)
              {
              # print(paste0("row: ", row, " -> col: ", col,  "  ==> val: ", val));
              val = matr[row,col];
              
              if(!is.na(val))
                {
                if(val == 0) 
                  { 
                  matr[row,col] = NA; 
                  i = length(adjs) + 1;
                  #print(paste0("row: ", row, " -> col: ", col,  "  ==> i: ", i, " ... j: ",j));
                  adjs[[i]] = c(row,col);
                  }
                }
              # stop("monte");
              }
            }
          }
        }
      #
      }
    matr;
    }
  my.count = 0;
  nrow = nrow(matr);
  ncol = ncol(matr);
  firsts = c();
  # which runs "by cols" ... down, then across
  while(length( which(matr == 0) > 0))
    {
    idx = which(matr == 0)[1];  # first one ... 
    firsts = c(firsts, idx);
    d = idx / nrow; # division
    c = ceiling(d); # which column
    r = round( (1-(c - d))*nrow, 0); # remainder is row [+1 ?]
    if(r > nrow) { r = nrow; }
    
    matr = mineSweep(matr,r=r,c=c);
    my.count = 1 + my.count;
    }
  
  
  list("count" = my.count, "elements" = firsts, "matrix" = matr);
  }
mnist.zeroFillMatrix = function(img.matrix.t, fdim=c(24,24), priority="top-left")
  {
  tmp = strsplit(tolower(priority),"-",fixed=TRUE)[[1]];
  # this could possibly truncate the matrix ???
  tdim = dim(img.matrix.t);  
  
  img.matrix.n = img.matrix.t;
  
  priority.rows = tmp[1];
  row.zeroes = rep(0, times=tdim[2]);
  rows.to.add = fdim[1] - tdim[1];
  rows.half = floor(rows.to.add/2);
  # add rows
  if(rows.half > 0)
    {
    for(i in 1:rows.half)
      {
      img.matrix.n = rbind( row.zeroes, img.matrix.n, row.zeroes );
      }
    }
  
  rows.remainder = rows.to.add - 2*rows.half;
  if(rows.remainder > 0)
    {
    if(priority.rows == "bottom")
      {
      img.matrix.n = rbind(img.matrix.n, row.zeroes ); 
      } else { img.matrix.n = rbind( row.zeroes, img.matrix.n); }
    }
  
  
  # we have an update on the dimensions
  ndim = dim(img.matrix.n); 
  
  
  priority.cols = tmp[2];
  col.zeroes = rep(0, times=ndim[1]);
  cols.to.add = fdim[2] - tdim[2];
  cols.half = floor(cols.to.add/2);
  # add cols
  if(cols.half > 0)
    {
    for(i in 1:cols.half)
      {
      img.matrix.n = cbind( col.zeroes, img.matrix.n, col.zeroes );
      }
    }
  
  cols.remainder = cols.to.add - 2*cols.half;
  if(cols.remainder > 0)
    {
    if(priority.cols == "right")
      {
      img.matrix.n = cbind(img.matrix.n, col.zeroes ); 
      } else { img.matrix.n = cbind( col.zeroes, img.matrix.n); }
    }
  
  rownames(img.matrix.n) = colnames(img.matrix.n) = NULL;
  
  # we will now just truncate ? truncate on "center" or "top-left" or "as-is"?
  img.matrix.n[1:fdim[1],1:fdim[2]]; # as-is
  }


mnist.trimMatrix = function(img.matrix)
  {
  rs = rowSums(img.matrix);
  nr = nrow(img.matrix);
  rtrim = c();
    rs.first = which(rs > 0)[1] - 1;
    rs.last  = nr - ( which(rev(rs) > 0)[1] - 1 ) + 1;
    # trim
    if(rs.first > 0) { rtrim = c(rtrim, 1:rs.first); }
    # trim
    if(rs.last <= nr) { rtrim = c(rtrim, rs.last:nr); }
    
    
  cs = colSums(img.matrix);
  nc = ncol(img.matrix);
  ctrim = c();
    cs.first = which(cs > 0)[1] - 1;
    cs.last  = nc - ( which(rev(cs) > 0)[1] - 1 ) + 1;
    # trim
    if(cs.first > 0) { ctrim = c(ctrim, 1:cs.first); }
    # trim
    if(cs.last <= nc) { ctrim = c(ctrim, cs.last:nc); }
  img.matrix[-rtrim,-ctrim];
  }

mnist.centerMatrix = function(img.matrix, fdim=c(24,24), priority="top-left")
  {
  img.matrix.t = mnist.trimMatrix(img.matrix);
  mnist.zeroFillMatrix(img.matrix.t, fdim=fdim, priority=priority);
  }


mnist.grabMatrixFromImage = function(img.file, path.to.image.file)
  {
  img.png = paste0(path.to.image.file,img.file);
  img =  image_read(img.png); # read.svg is a different function?
  img.matrix = im.getMatrixFromImage(img);
  img.matrix;
  }


mnist.resultSummary = function(final, option)
  {
  # independent comparisons
  result = list();
  for(matrix.size in option$matrix.size)
    {
    # final$data$data.id
    sub = wildcardSearch( paste0(matrix.size,".*"), "data.id", df=final$data);
    test1 = sum(sub$true == sub$best) / nrow(sub); 
    result[[paste0("s.",matrix.size)]] = test1;
    # outcome.details = as.numeric(unlist(result.one[,6:length(result.one)]));
    }
  for(data.form in option$data.form)
    {
    # final$data$data.id
    sub = wildcardSearch( paste0("*.",data.form,".*"), "data.id", df=final$data);
    test1 = sum(sub$true == sub$best) / nrow(sub); 
    result[[data.form]] = test1;
    # outcome.details = as.numeric(unlist(result.one[,6:length(result.one)]));
    }
  for(matrix.division in option$matrix.division)
    {
    # final$data$data.id
    sub = wildcardSearch( paste0("*.",matrix.division,"_*"), "data.id", df=final$data);
    test1 = sum(sub$true == sub$best) / nrow(sub); 
    result[[paste0("n",matrix.division)]] = test1;
    # outcome.details = as.numeric(unlist(result.one[,6:length(result.one)]));
    }
  for(matrix.features in option$matrix.features)
    {
    # final$data$data.id
    sub = wildcardSearch( paste0("*_",matrix.features), "data.id", df=final$data);
    test1 = sum(sub$true == sub$best) / nrow(sub); 
    result[[matrix.features]] = test1;
    # outcome.details = as.numeric(unlist(result.one[,6:length(result.one)]));
    }
  ## this is a different column
  for(comparison.methods in option$comparison.methods)
    {
    sub = subsetDataFrame( final$data, "comparison.method", "==" , comparison.methods);
    test1 = sum(sub$true == sub$best) / nrow(sub); 
    result[[comparison.methods]] = test1;
    }
  ## these are their own columns
  for(decision.stats in option$decision.stats)
    {
    key.name = paste0("stats.",decision.stats);
    name.idx = which(names(final$data) == key.name );
    sub = final$data[,c(1:2,name.idx)];
    #test1 = sum(sub$true == sub$best) / nrow(sub); 
    test1 = sum(sub[,1] == sub[,3]) / nrow(sub); # compare to individual test...
    result[[key.name]] = test1;
    }
  
  # as.data.frame(result);
  result;
  }

mnist.performAllTest = function(testing.files, training.data,
                  path.to.mnist, folder.mnist, option)
  {
  timer.start = as.numeric(Sys.time());
  cache.md5 = paste0(
                  paste0( unlist(option), collapse="-"),
                  paste0( unlist(testing.files), collapse="^"),
                  paste0( names(training.data), collapse="::") );
  
  cache.me = paste0("testing-", md5(cache.md5), ".rds");
  print(paste0("function mnist.performAllTest with cache.file: ", cache.me ));
  
  cache.file = paste0(path.to.mnist, cache.me);
  if(!file.exists(cache.file))
  #if(TRUE)
    {
    n = length(testing.files);
    m = length(testing.files[[1]]); # we assume balanced panel
    total = n * m;
  
    final = NULL;
    final.best = c();
    true.val = c();
    for(d in 1:n) # true digit
        {
        num = d; if(num == 10) { num = 0; }
        for(r in 1:m) # replicate
          {
          true.val = c(true.val, num);
          result.one = mnist.performOneTest(testing.files, training.data, 
                                    d=d, r=r, 
                                    path.to.mnist, folder.mnist, option);
          final = rbind(final,result.one);
          best.of.best =  whichMaxFreq(result.one$best);
          #best.of.best = paste0(best.of.best,collapse=",");  # store both as char if tied
          best.of.best = sample(best.of.best,1);  # pick one randomly
          final.best = c(final.best, best.of.best);
          }
        }
      my.result = list("data" = final, "true" = true.val, "predicted" = final.best);
      saveRDS(my.result, cache.file);
    } else { my.result = readRDS(cache.file); }
  
  
  timer.end = as.numeric(Sys.time());
  elapsed = round((timer.end - timer.start), 1);
  print(paste0("time elapsed: ",elapsed, " secs"));
  my.result;      
  }

mnist.performOneTest = function(testing.files, training.data, 
                  d=1, r=1, 
                  path.to.mnist, folder.mnist, option)
  {
  testing.one = mnist.prepareOneTest(testing.files, 
                  d=d, r=d, 
                  path.to.mnist, folder.mnist, option);
  # we need an X with all of the inputs ... calculations
  # we need a Y that is a current outcome for the given "X" computation
  # maybe not panel, but one long vector ?
  # option$comparison.methods = c("cosine","euclidean");
  # option$decision.stats = c("mean", "median", "top^1+mean+median", "top^1+median", "top^1", "top^5+mean+median", "top^5+median",  "top^5");
  # Y.true, Y.predicted, "method" as [[myname]], X_cosine:mean:0-c?
  num = d; if(num == 10) { num = 0; }
  myLabel = paste0("T_",num,"-",letters[r]);
  
  results = NULL;
  
  m = dim(training.data[[1]])[2]/10; # same for all (balanced panel)
  mynames = names(testing.one);
  for(myname in mynames)
    {
    testing.sub  = testing.one[[myname]];
      colnames(testing.sub) = paste0("T_",colnames(testing.sub));
    training.sub = training.data[[myname]];
   # print(myname);
    for(comparison.methods in option$comparison.methods)
      {
      # print(comparison.methods);
      tmp = strsplit(comparison.methods,".",fixed=TRUE)[[1]];
      
      comparison.type = tmp[1]; 
      comparison.function = tmp[2];
      
      if(comparison.methods == "sim.cosine")
        {
        compare = lsa::cosine( as.numeric(testing.sub), training.sub);
        # max ...
        which.dir = "max";
        }
      if(comparison.methods == "dist.euclidean")
        {
        compare = as.matrix( stats::dist( 
                          t( cbind(testing.sub, training.sub) ), 
                          method="euclidean" ) )[1,];
        compare   = compare[-c(1)];
        which.dir = "min";
        # min(compare[-c(1)]);
        }
      
      
      compare.matrix = matrix(compare, ncol=m, byrow=TRUE);
        rownames(compare.matrix) = c(1:9,0);
        colnames(compare.matrix) = letters[1:m];
      compare.df = as.data.frame(compare.matrix);
      
      outcomes = mnist.doComparisons(compare.df, 
                      option$decision.stats, which.dir);
     # print(paste0("outcomes: [",length(outcomes),"] --> ", paste0(outcomes,collapse=",")));
      
      best = whichMaxFreq(outcomes);    # again downward biased
                                        # what to do about ties?
      
     # best = paste0(best,collapse=","); # allow ties?
      best = sample(best,1);  # pick one randomly
      # whichMax(compare);
      # names(compare)[whichMax(compare)];
      
      # append testing.sub to training.sub
      # subtract 1 so it is the same form as cosine
      # X.eigen.d = stats::dist( t(X.eigen), method="euclidean");
      row = c(num, best, myLabel, myname, comparison.methods, outcomes);
      results = rbind(results, row);        
      }
    
    }
  # let's do numeric on columns
  results = as.data.frame(results);
    colnames(results) = c("true", "best", "test.id", "data.id",
        "comparison.method", paste0("stats.",option$decision.stats) );
    rownames(results) = NULL;
  results = convertDataTypes(results, cols=c(1:2, 6:ncol(results)), "numeric");
  
  results;
  }

# place in functions.dataframe.R

convertDataTypes = function(df, cols, conversion="numeric")
  {
  ndf = df;
  for(col in cols)
    {
    ndf[,col] = switch(conversion,
                      "numeric" = as.numeric(ndf[,col]),
                       as.numeric(ndf[,col]) # default case of switch
                      );
    }
  ndf;
  }

mnist.getDigit = function(vec, which.dir)
  {
  m = min(vec);
  if(which.dir == "max") { m = max(vec); }
  # this will only return the "first one", biased to 1
  num = which(vec == m)[1];  # names(vec)[m.idx];
  if(num == 10) { num = 0; }
  num;
  }

mnist.doComparisons = function(compare.df, decision.stats, which.dir="min")
  {      
  # maths = list(); # let's store previous ones in case we use them again
  decisions = c();
  for(decision.stat in decision.stats)
    {
    keys = strsplit(decision.stat,"+",fixed=TRUE)[[1]];
    nkeys = length(keys);
    res.joint = 0; # we have multiple keys for a single decision
    for(i in 1:nkeys)
      {
      key = keys[i];
      sub = strsplit(key,"^",fixed=TRUE)[[1]];
      # if maths[[key]], just grab it
      if(key == "mean")
        {
        res = rowMeans(compare.df);
        # maths[[key]] = res;
        res.joint = res + res.joint;
        }
      if(key == "median")
        {
        res = rowMedians(compare.df);
        # maths[[key]] = res;
        res.joint = res + res.joint;
        }
      if(key == "sd")  # -1 * sd ... more compact ?
        {
        res = rowSDs(compare.df);  # we want minimal spread
        # will this always be a "1" or an "8" ?
        # maths[[key]] = res;
        # res.joint = (-1 * res) + res.joint;
        if(which.dir == "max") { res = -1 * res; } # min/max logic is backwards.
        res.joint = res + res.joint;
        }
      
      if(length(sub) > 1)
        {
        if(sub[1] == "top")
          {
          howmany = as.numeric(sub[2]);
          res = mnist.computeTop(compare.df,howmany,which.dir);
          if(which.dir == "min") { res = -1 * res; } # min/max logic is backwards.
          res.joint = res + res.joint;
          }
        }
      
      }
    num = mnist.getDigit(res.joint, which.dir);
    decisions = c(decisions, num);
    }
  as.numeric(decisions);
  }


mnist.computeTop = function(df, howmany=10, which="min", return="sum")
  {
  direction = TRUE;
  if(which == "min") { direction = FALSE; }
  m = dim(df)[2];
  df = as.matrix(df);  # abs( if we have negative values?
  df.sort = sort( unique(as.numeric(df)),
              decreasing=direction)[1:howmany];
  
  
  ndf = df;
  if(which == "min")
    {
    ndf[ ndf[,1:m] > df.sort[howmany] ] = 0;
    } else {
          ndf[ ndf[,1:m] < df.sort[howmany] ] = 0;
          }
  
  if(return == "sum") { return (rowSums(ndf)); }
  }


mnist.prepareOneTest = function(my.files,d=1,r=1, 
                  path.to.mnist, folder.mnist, option)
  {
  num = d; if(num == 10) { num = 0; }
  img.file = my.files[[d]][r]; 
  myLabel = paste0(num,"-",letters[r]);
  
  my.matrix = list();
  one.data = mnist.prepareOneElement(img.file, 
                        path.to.mnist, folder.mnist, option);
  my.matrix = mnist.mergeResultData(one.data, my.matrix, myLabel);
  
  my.matrix;
  }

mnist.prepareOneElement = function(img.file, 
                  path.to.mnist, folder.mnist, option)
  {
  path.to.image.file = gsub(folder.mnist, "", path.to.mnist, fixed=TRUE); 

  result = list();
  for(matrix.size in option$matrix.size)
    {
    img.matrix = mnist.grabMatrixFromImage(img.file, path.to.image.file);
      # im.printImageMatrix(img.matrix, raw=TRUE);
    if(matrix.size == "raw") 
      { 
      mdim = dim(img.matrix); 
      } else {
              mdim = as.numeric(strsplit(matrix.size,"x",fixed=TRUE)[[1]]);
              # overwrites original for this option
              img.matrix  = mnist.centerMatrix(img.matrix, c(24,24));
              mdim = dim(img.matrix);
              }
    for(data.form in option$data.form)
      {
      if(data.form == "b") { img.matrix[img.matrix > 0] = 1; }
      for(matrix.division in option$matrix.division)
        {
        sdim = mdim/matrix.division; # 
        s.gtg = sum( is.wholenumber(sdim) );
        if(s.gtg == 2) # can we divide?
          {
          vec.eigen = vec.eigen.half = vec.eigen.fourth = vec.eigen.third = vec.eigen.sixth = vec.rowsums = vec.colsums = vec.gridcount = c();
          i = 1;
          # top-left, left to right ... 
          r = 1; c = 1; # original matrix indexes
          while(c < mdim[2])
            {
            block = img.matrix[r:(r+sdim[1]-1),c:(c+sdim[2]-1)];
            
            block.eigen = Re(eigen( block )$values); 
              vec.eigen = c(vec.eigen, block.eigen);
            block.eigen.half = block.eigen[1:floor( sdim[1]/2 )];
              vec.eigen.half = c(vec.eigen.half, block.eigen.half);
            block.eigen.fourth = block.eigen[1:floor( sdim[1]/4 )];
              vec.eigen.fourth = c(vec.eigen.fourth, block.eigen.fourth);
            block.eigen.third = block.eigen[1:floor( sdim[1]/3 )];
              vec.eigen.third = c(vec.eigen.third, block.eigen.third);
            block.eigen.sixth = block.eigen[1:floor( sdim[1]/3 )];
              vec.eigen.sixth = c(vec.eigen.sixth, block.eigen.sixth);
            block.rowsums = rowSums(block);
              vec.rowsums = c(vec.rowsums, block.rowsums);
            block.colsums = colSums(block);
              vec.colsums = c(vec.colsums, block.colsums);
            block.gridcount = sum(block);
              vec.gridcount = c(vec.gridcount, block.gridcount); 

            r = r + sdim[1];  if(r > mdim[1]) { r = 1; c = c + sdim[2]; }
            }
          
          
          data.key = paste(matrix.size, data.form, matrix.division, sep=".");
          
          my.list = list();
          ## this code is not efficient
          if(is.element("eigen", option$matrix.features)) {      my.list$eigen      = vec.eigen; }
          if(is.element("eigen.half", option$matrix.features)) { my.list$eigen.half = vec.eigen.half; }
          if(is.element("eigen.fourth", option$matrix.features)) { my.list$eigen.fourth = vec.eigen.fourth; }
          if(is.element("eigen.third", option$matrix.features)) { my.list$eigen.third = vec.eigen.third; }
          if(is.element("eigen.sixth", option$matrix.features)) { my.list$eigen.sixth = vec.eigen.sixth; }
          if(is.element("rowsums", option$matrix.features)) {    my.list$rowsums    = vec.rowsums; }
          if(is.element("colsums", option$matrix.features)) {    my.list$colsums    = vec.colsums; }
          if(is.element("gridcount", option$matrix.features)) {  my.list$gridcount  = vec.gridcount; }
         
          result[[data.key]] = my.list;
          # result[[data.key]] = list("eigen" = vec.eigen, 
          #                           "eigen.half" = vec.eigen.half,
          #                           "rowsums" = vec.rowsums, 
          #                           "colsums" = vec.colsums,
          #                           "gridcount" = vec.gridcount,
          #                           ); 
          }
        }
      
      }
    }
  result;
  }


mnist.mergeResultData = function(one.data, my.matrix, my.labels)
  {
  mynames = names(one.data);
  for(myname in mynames)
    {
    one.sub = one.data[[myname]];
    subnames = names(one.sub);
    for(subname in subnames)
      {
      matrix.name = paste0(myname,"_",subname);
      my.data = one.sub[[subname]];
      # my.n = length(my.data);
      #print(dim(my.matrix[[matrix.name]]));
      #if(!element.exists(my.matrix[[matrix.name]]))
      if(is.null(my.matrix[[matrix.name]]))
        {
        my.matrix[[matrix.name]] = matrix(my.data, ncol=1);
        } else {
                my.matrix[[matrix.name]] = cbind(my.matrix[[matrix.name]], my.data);
                }
      colnames(my.matrix[[matrix.name]]) = my.labels;
      }
    }
  my.matrix;
  }

mnist.prepareTrainingData = function(my.files, 
                  path.to.mnist, folder.mnist, option)
  {
  timer.start = as.numeric(Sys.time());
  cache.md5 = paste0( unlist(option), collapse="-");
  cache.me = paste0("training-", md5(cache.md5), ".rds");
  print(paste0("function mnist.prepareTrainingData with cache.file: ", cache.me ));
  
  cache.file = paste0(path.to.mnist, cache.me);
  if(!file.exists(cache.file))
    {
    n = length(my.files);
    m = length(my.files[[1]]); # we assume balanced panel
    total = n * m;
    
    my.matrix = list();  # list of matrices
    myLabels = c();
    i = 1;
    for(d in 1:n) # true digit
        {
        num = d; if(num == 10) { num = 0; }
        for(r in 1:m) # replicate
          {
          myLabel = paste0(num,"-",letters[r]);
          print( paste0( i,"/",total," = ", 
                  round( (100 * i/total), 0),"  ====>  ",myLabel ) );
          myLabels = c(myLabels, myLabel);
          img.file = my.files[[d]][r];
          one.data = mnist.prepareOneElement(img.file, 
                        path.to.mnist, folder.mnist, option);
          
          my.matrix = mnist.mergeResultData(one.data, my.matrix, myLabels);
          i = 1 + i;
          }
        }
    #
    saveRDS(my.matrix, cache.file);
    } else { my.matrix = readRDS(cache.file); }
  timer.end = as.numeric(Sys.time());
  elapsed = round((timer.end - timer.start), 1);
  print(paste0("time elapsed: ",elapsed, " secs"));
  my.matrix;
  }




mnist.grabFiles = function(set, path.to.mnist, folder.mnist)
  {
  cache.file = paste0(path.to.mnist, set, "-files.rds");
  
  if(!file.exists(cache.file))
    {
    # with caching, we only have to do this once.
    my.path = paste0( path.to.mnist, set, "/");
    
    my.list = list();
    for(i in 1:10)
      {
      digit = as.character(i);
      if(i == 10) { digit = "0"; }
      
      my.sub = paste0(my.path, digit,"/");
      my.sub.files = paste0(folder.mnist, set, "/", digit,"/", 
                                    list.files(my.sub) );
      my.list[[i]] = my.sub.files;
      }
  saveRDS(my.list, cache.file);
    } else { my.list = readRDS(cache.file); }
  my.list;
  }
























