
library(readr);

prepareBibleDataFrame = function(path.to.obible, versions, langs)
  {
  # let's store each data.frame separately, and return the full.df as well
  
  cache.file = paste0(path.to.obible, "obible.rds");
  
  full.df = NULL;
  bnames = c("Gen", "Exo", "Lev", "Num", "Deu", "Jos", "Jug", "Rut", "1Sa", "2Sa", "1Ki", "2Ki", "1Ch", "2Ch", "Ezr", "Neh", "Est", "Job", "Psm", "Pro", "Ecc", "Son", "Isa", "Jer", "Lam", "Eze", "Dan", "Hos", "Joe", "Amo", "Oba", "Jon", "Mic", "Nah", "Hab", "Zep", "Hag", "Zec", "Mal", "Mat", "Mak", "Luk", "Jhn", "Act", "Rom", "1Co", "2Co", "Gal", "Eph", "Phl", "Col", "1Ts", "2Ts", "1Ti", "2Ti", "Tit", "Phm", "Heb", "Jas", "1Pe", "2Pe", "1Jn", "2Jn", "3Jn", "Jud", "Rev");
  number.str = c("1","2","3","4","5","6");
  book.idx = numeric(0);
  
  has.book.names = FALSE;
  book.names = c();
  
  i = 1;
  nv = length(versions);
  
  
  for(i in 1:nv)
    {
    version = versions[i];
    lang = langs[i];
    print(version);
    v.file = paste0(path.to.obible, version, "/", version, ".txt");
    v.out  = paste0(path.to.obible, version, ".rds");
   
    
    
    if(!file.exists(v.out))
    {
    
# https://stackoverflow.com/questions/12626637/read-a-text-file-in-r-line-by-line#35761217    
    ####file.conn = file(v.file, open="r", encoding="UTF-8"); # read one line at a time, less memory
    ####lines = readLines(file.conn); print(lines);
    
    # t.file = gsub("hb5.txt","test.txt",v.file);
    # tfile.conn = file(t.file, open="r", encoding="UTF-8"); # read one line at a time, less memory
    # lines = readLines(tfile.conn); print(lines);
    # t.file = "http://md5.mshaffer.com/WSU_STATS419/stackoverflow/chinese/test.txt";
    # tfile.conn = file(t.file, open="r", encoding="UTF-8"); # read one line at a time, less memory
    # lines = readLines(tfile.conn); print(lines);
    v.df = NULL; 
    
    lines = readr::read_lines(v.file, n_max = -1);
    nls = length(lines);
    
    
    nl = 1; # line.number
    nb = 0; # book.number
    last.book.name = "";
    
    for(nl in 1:nls)
      {
      line = lines[nl];
      if(nl > 1)  #skip the first line
        {
        tmp = strsplit(line, " ")[[1]];
        nt = length(tmp);
        
        last.tmp2 = "";
        for(j in 1:nt)
          {
          tmp2 = strsplit(tmp[j], ":")[[1]]; # looking for first :
          nt2 = length(tmp2);
          if(nt2 >= 2)
            {
            # nl = 28374;
            if(version == "kjv")
              {
              bkn = tmp2[1]; bpre = "";
              bfirst = charAt(bkn,1);  ## functions-encryption.R
              if(is.element(bfirst,number.str)) 
                { 
                bpre = bfirst;
                bkn = substr(bkn,2,nchar(bkn));
                }
               
              tmp2.n = as.numeric(sub("\\D*(\\d+).*", "\\1", bkn));
              
                book.name = paste0(bpre, gsub(tmp2.n[nt2-1],"",bkn)); 
                chap.n = tmp2.n[1];
                para.n = tmp2[2];
                line.text = paste0(tmp[(j+1):nt], collapse=" ");
              
              } else {
                      book.name = last.tmp2;
                      chap.n = tmp2[1];
                      para.n = tmp2[2];
                      line.text = paste0(tmp[(j+1):nt], collapse=" ");
                      }
            
            break;
            }
          last.tmp2 = tmp2;
          }

#####
        if(!has.book.names)
          {
          book.names = c(book.names, book.name);
          }
        
        if(book.name != last.book.name)
          {
          nb = 1 + nb;
          book.idx[nb] = nl;
print(paste0("bn: ",nb,"     ", "book.name: ",book.name, " --> last.book.name: ",last.book.name));

          }
        
        row = c(version, lang, nb, book.name, chap.n, para.n, line.text);
        # row = c(version, lang, nb, bnames[nb], chap.n, para.n, line.text);

        v.df = rbind(v.df, row); 
          # version .. lang .. book.name .. chap.n ... para.n (verse) ... para.text (verse)
        last.book.name = book.name;
        }
      nl = 1 + nl;
      }
    book.names = unique(book.names);
    has.book.names = TRUE;
    
    v.df = as.data.frame(v.df);
      colnames(v.df) = c("version","lang",
                        "book.n", "book.name", 
                        "chap.n", "para.n", "para.text");
    v.df$book.n = as.numeric(v.df$book.n);
    v.df$chap.n = as.numeric(v.df$chap.n);
    v.df$para.n = as.numeric(v.df$para.n);
    
    
    
    
    saveRDS(v.df, v.out);
    } else { v.df = readRDS(v.out); }
  
    # end of version
    
    full.df = rbind(full.df, v.df);
    
    }
    ## end of function
    full.df = as.data.frame(full.df);
      colnames(full.df) = c("version","lang",
                        "book.n", "book.name", 
                        "chap.n", "para.n", "para.text");
    full.df$book.n = as.numeric(full.df$book.n);
    full.df$chap.n = as.numeric(full.df$chap.n);
    full.df$para.n = as.numeric(full.df$para.n);
  
  
  }






# 
# prepareBibleDataFrame = function(path.to.obible, versions, langs)
#   {
#   # let's store each data.frame separately, and return the full.df as well
#   
#   cache.file = paste0(path.to.obible, "obible.rds");
#   
#   full.df = NULL;
#   
#   has.book.names = FALSE;
#   book.names = c();
#   
#   i = 1;
#   nv = length(versions);
#   
#   
#   for(i in 1:nv)
#     {
#     version = versions[i];
#     lang = langs[i];
#     
#     v.file = paste0(path.to.obible, version, "/", version, ".txt");
#     v.out  = paste0(path.to.obible, version, ".rds");
#     v.df = NULL; 
# # https://stackoverflow.com/questions/12626637/read-a-text-file-in-r-line-by-line#35761217    
#     file.conn = file(v.file, open="r", encoding="UTF-8"); # read one line at a time, less memory
#     nl = 1; # line.number
#     nb = 0; # book.number
#     last.book.name = "";
#     while(TRUE)
#       {
#       line = readLines(file.conn, n=1); print(line);
#       if( length(line) == 0)
#         {
#         break;
#         }
#       ## process the line
#       if(nl > 1)  #skip the first line
#         {
#         # print(line); break;
#         # process line
#         # line = "Gen 1:1 At the first God made the heaven and the earth."
#         # line2 = "Ge1:3 And God said, Let there be light: and there was light.";
#         
#         tmp = strsplit(line, " ")[[1]];
#         nt = length(tmp);
#         last.tmp2 = "";
#         for(j in 1:nt)
#           {
#           tmp2 = strsplit(tmp[j], ":")[[1]]; # looking for first :
#           nt2 = length(tmp2);
#           if(nt2 >= 2)
#             {
#             chap.n = tmp2[nt2-1];
#             para.n = tmp2[nt2];
#             book.name = last.tmp2;
#             if(nt2 == 3) { book.name = tmp2[1]; }
#             line.text = paste0(tmp[(j+1):nt], collapse=" ");
#             break;
#             }
#           last.tmp2 = tmp2;
#           }
#         
#         if(!has.book.names)
#           {
#           book.names = c(book.names, book.name);
#           }
#         
#         if(book.name != last.book.name)
#           {
#           nb = 1 + nb;
#           }
#         
#         row = c(version, lang, nb, book.name, chap.n, para.n, line.text);
#         v.df = rbind(v.df, row); 
#           # version .. lang .. book.name .. chap.n ... para.n (verse) ... para.text (verse)
#         last.book.name = book.name;
#         }
#       nl = 1 + nl;
#       }
#     close(file.conn);
#     
#     book.names = unique(book.names);
#     has.book.names = TRUE;
#     # end of version
#     full.df = rbind(full.df, v.df);
#     
#     v.df = as.data.frame(v.df);
#       colnames(v.df) = c("version","lang",
#                         "book.n", "book.name", 
#                         "chap.n", "para.n", "para.text");
#     v.df$book.n = as.numeric(v.df$book.n);
#     v.df$chap.n = as.numeric(v.df$chap.n);
#     v.df$para.n = as.numeric(v.df$para.n);
#     
#     
#     
#     
#     }
#   
#   }