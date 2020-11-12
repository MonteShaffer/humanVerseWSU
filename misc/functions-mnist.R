
mnist.grabFiles = function(set, path.to.mnist, folder.mnist)
  {
  cache.file = cache.file = paste0(path.to.mnist, set, "-files.rds");
  
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
























