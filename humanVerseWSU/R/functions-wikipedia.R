# path.wiki.page = wiki.info$path.wiki.page;
# wiki.html = wiki.info$wiki.html;
library(rvest);
library(magrittr);


wiki.parseWikiPageToString = function(url = "https://en.wikipedia.org/wiki/Columbia_Falls,_Montana")
  {
  str = strsplit(url, "//", fixed=TRUE)[[1]][2];
    find = c("/",".",",");
    replace = c("-","^","+");
    n.find = length(find);
  for(i in 1:n.find)
    {
    str = gsub(find[i],replace[i],str,fixed=TRUE);
    }
  str;
  }

wiki.parseWikiStringBackToPage = function(str = "en^wikipedia^org-wiki-Columbia_Falls+_Montana")
  {
  find = c("/",".",",");
  replace = c("-","^","+");
  n.find = length(find);
  url = str;
  for(i in 1:n.find)
    {
    url = gsub(replace[i],find[i],url,fixed=TRUE);
    }
  url = paste0("https://",url);
  url;
  }




wiki.downloadWikiPage = function(wiki.url="https://en.wikipedia.org/wiki/Columbia_Falls,_Montana", path.wiki, save.encoding="UTF-8")
  {
  #wiki.url = "https://en.wikipedia.org/wiki/Whitefish%2C_Montana";
  #wiki.url = "https://en.wikipedia.org/wiki/Manhattan";


  str = wiki.parseWikiPageToString(wiki.url); # this is also reversible ... maybe pass encoding?

  path.wiki.page = paste0(path.wiki,str,"/");
  createDirRecursive(path.wiki.page);

  wiki.html = paste0(path.wiki.page,"page.html");

  do.nothing = grabHTML(wiki.html, wiki.url, encoding = save.encoding);
  # wildcardSearch("*Record high*",do.nothing);

  list("path.wiki.page" = path.wiki.page, "wiki.html" = wiki.html);
  }






strip_tags = function(html.str)
  {
  return(gsub("<.*?>", "", html.str))
  }



wiki.cleanupNumericVectors = function(vec)
  {
  vec = gsub("(","",vec, fixed=TRUE);
  vec = gsub(")","",vec, fixed=TRUE);
  vec = gsub(",","",vec, fixed=TRUE);
  vec = stringr::str_trim(vec);
  as.numeric(vec);
  }


wiki.cleanupClimateCell = function(str)
  {
  str = gsub("(","",str, fixed=TRUE);
  str = gsub(")","",str, fixed=TRUE);
  str = gsub("<br>",",",str, fixed=TRUE);
  str = stringr::str_trim(str);
  str;
  }


wiki.separateDataFrameUnits = function(df)
  {
  my.cols = names(df);
  #my.rows = strip_tags( row.names(df) );  # get's rid of UNICODE things...

  n.cols = ncol(df);
  n.rows = nrow(df);

  ndf = as.data.frame( matrix(NA,nrow=0,ncol=14), row.names=NULL );



  #ndf$key = my.rows;

  # we will add rows at end if have double units
  new.rows = NULL;
  for(r in 1:n.rows)
    {
    my.row.name = strip_tags ( row.names( df[r,] ) );
    my.row = as.character( df[r,] );
    my.test = strsplit(my.row[1],",")[[1]];

    if(length(my.test) == 1)
      {
      new.row = c(1, my.row.name, my.row);
      new.rows = rbind(new.rows, new.row);
      } else  {
              for(j in 1:length(my.test))
                {
                new.row = c(j, my.row.name);
                for(c in 1:n.cols)
                  {
                  new.test = strsplit(my.row[c],",")[[1]];
                  new.row = c(new.row, new.test[j]);
                  }
                new.rows = rbind(new.rows, new.row);
                }
              }
    }

  ndf = rbind(ndf, new.rows);
  my.cols = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
"Oct", "Nov", "Dec"); # in case other languages ...
  colnames(ndf) = c("units","key",my.cols);

  ndf$units = as.numeric(ndf$units);

  ndf$Jan = as.numeric(ndf$Jan);
  ndf$Feb = as.numeric(ndf$Feb);
  ndf$Mar = as.numeric(ndf$Mar);
  ndf$Apr = as.numeric(ndf$Apr);
  ndf$May = as.numeric(ndf$May);
  ndf$Jun = as.numeric(ndf$Jun);
  ndf$Jul = as.numeric(ndf$Jul);
  ndf$Aug = as.numeric(ndf$Aug);
  ndf$Sep = as.numeric(ndf$Sep);
  ndf$Oct = as.numeric(ndf$Oct);
  ndf$Nov = as.numeric(ndf$Nov);
  ndf$Dec = as.numeric(ndf$Dec);

  rownames(ndf) = NULL;

  ndf;
  }


wiki.parseClimateTable = function(str.climate,str.climate.sea="")
  {
  # old school ...  ## str.climate.sea ... not working (Manhattan)

  rows = strsplit(str.climate,"</tr>")[[1]];
  n.rows = length(rows);

  my.info = xml2::xml_text( xml2::read_html(rows[1]) ) %>%
                convertStringBetweenEncodings() %>%
                stringr::str_trim();

  cut.me = DescTools::StrLeft(my.info, DescTools::StrPos(my.info, pattern="Climate") - 1);
  if(cut.me != "")
    {
    my.info = gsub(cut.me, "", my.info, fixed=TRUE);
    }

  my.cols = strip_tags ( stringr::str_trim(
                (strsplit( stringr::str_trim(rows[2]) ,"</th>")[[1]])[-c(1,14)]) );

  my.rows = NULL;
  my.keys = c();
  for(j in 3:n.rows)
    {
    row = rows[j];

    # row.key = (strsplit( stringr::str_trim(rows[j]) ,"</th>")[[1]][1]) %>%
    #             convertStringBetweenEncodings() %>%
    #             strip_tags() %>%
    #             stringr::str_trim();

    row.key = convertStringBetweenEncodings(
                  stringr::str_trim(
                      strip_tags(
                          strsplit(
                              stringr::str_trim(rows[j]) ,"</th>")[[1]][1] ) ) );



    raw.cols = (strsplit( stringr::str_trim(rows[j]) ,"</td>")[[1]])[-c(13,14)];

    # tmp = strsplit(raw.cols,"<br>");
    tmp = strsplit(raw.cols,";\">");
    if(length(tmp) >= 12)
      {
      my.keys = c(my.keys, row.key);
      vals = numeric(12);
      for(i in 1:12)
        {
        looking = 2; if(i==1) { looking = 4; }
        #val.info = wiki.cleanupClimateCell(tmp[[i]][looking]);
        #my.val = as.numeric( wiki.cleanupClimateCell(tmp[[i]][2]) ); # celsius
        vals[i] = wiki.cleanupClimateCell(tmp[[i]][looking]);
        }
      my.rows = rbind(my.rows, vals);
      }
    }

  colnames(my.rows) = my.cols;
  rownames(my.rows) = my.keys;
  #df = as.data.frame( transposeMatrix( my.rows ) );
  # df = as.data.frame( transposeMatrix( my.rows ) );
  df = as.data.frame(  my.rows  );

  df = wiki.separateDataFrameUnits(df);


  list("title" = my.info, "df" = df );
  }


wiki.findMyTable = function(tables, search)
  {
  my.table = NULL;
  if(length(tables) > 0)
    {
    for(i in 1:length(tables))
      {
      table.i = tables[i] %>% html_text();
      if(!is.na(wildcardSearch( search , table.i)))
        {
        my.table = tables[i]; # original format
        }
      }
    }
  my.table;
  }

wiki.parseAverageClimate = function(path.wiki.page, wiki.html, encoding="UTF-8")
  {
  #path.wiki.page = wiki.info$path.wiki.page;
  #wiki.html = wiki.info$wiki.html;

  mycache = paste0(path.wiki.page,"climate.rds");
  if(file.exists(mycache))
    {
    #data.climate = readRDS( file(mycache, encoding=encoding) );
    return ( readRDS( file(mycache, encoding=encoding) ) );
    }
  rvest.html = xml2::read_html(wiki.html, verbose=TRUE, encoding="UTF-8");
  tables = rvest.html %>%
              html_nodes(".wikitable");

  # my.table = wiki.findMyTable(tables,"*Climate data*");
  my.table = wiki.findMyTable(tables,"*Record high*");

  my.table.sea = wiki.findMyTable(tables,"*Average sea temperature*");  # Doesn't work


  if(!is.null(my.table))
    {

    my.climate = paste0(path.wiki.page,"climate.html");
    xml2::write_html(my.table, file(my.climate, encoding=encoding) );
    str.climate = base::readChar( file(my.climate, encoding=encoding), nchars=9724129);

    str.climate.sea = "";
    if(!is.null(my.table.sea))
        {
        my.climate.sea = paste0(path.wiki.page,"climate-sea.html");
        xml2::write_html(my.table, file(my.climate.sea, encoding=encoding) );
        str.climate.sea = base::readChar( file(my.climate.sea, encoding=encoding), nchars=9724129);
        }


    data.climate = wiki.parseClimateTable(str.climate,str.climate.sea);  # this is a list

    if( exists("local.data.path") )
		  {
      saveRDS( data.climate , file(mycache, encoding=encoding) );
      }
    return (data.climate);
    }
  return (NA);
  }




wiki.historicalPopulation = function(path.wiki.page, wiki.html, encoding="UTF-8")
  {
  mycache = paste0(path.wiki.page,"population.txt");
  if(file.exists(mycache))
    {
    return ( utils::read.csv(mycache, header=TRUE, quote="", sep="|") );
    }
  rvest.html = xml2::read_html(wiki.html, verbose=TRUE, encoding="UTF-8");
  tables = rvest.html %>%
              html_nodes(".toccolours");

  my.table = wiki.findMyTable(tables,"*Historical population*");

  if(!is.null(my.table))
    {
    table.df = (my.table %>%  html_table(fill = TRUE))[[1]];

    row.year = as.numeric(table.df[,1]);
    row.pop = as.numeric( gsub(",","",table.df[,2],fixed=TRUE) );

    table.ndf = na.omit( as.data.frame( cbind(row.year, row.pop) ) );
      colnames(table.ndf) = c("year", "population");

    if( exists("local.data.path") )
		  {
      utils::write.table( table.ndf , file=mycache, quote=FALSE, col.names=TRUE, row.names=FALSE, sep="|");
      }
    return (table.ndf);
    }
  return (NA);
  }


wiki.parseStateCapitalsOfAmerica  = function(path.wiki.page, wiki.html, encoding="UTF-8")
  {
  mycache = paste0(path.wiki.page,"state-capitals.txt");
  if(file.exists(mycache))
    {
    return ( utils::read.csv(mycache, header=TRUE, quote="", sep="|") );
    }

  rvest.html = xml2::read_html(wiki.html, verbose=TRUE, encoding="UTF-8");
  tables = rvest.html %>%
              html_nodes(".wikitable");

  my.table = wiki.findMyTable(tables,"*2716.7*");  # Alaska area should be unique

  if(!is.null(my.table))
    {
    table.df = ((my.table %>%  html_table(fill = TRUE))[[1]])[1:50,];

    # I need the links ...
    url.stem = "https://en.wikipedia.org";

    hrefs = my.table %>%
      html_nodes("th a") %>%
      html_attr("href");

    hrefs = hrefs[-c(1:3,54)]
    length(hrefs);

    # names(table.df);
    # dput(names(table.df));  c("State", "Capital", "Capital Since", "Area (mi2)", "Population (2019 est.)", "MSA/µSA Population\r\n(2019 est.)", "CSA Population\r\n(2019 est.)", "Rank in State\r\n(city proper)")
    # new-lines \r\n are not good ... I could manually rebuild or replace ...
    new.names = gsub("[[:space:]]", " ", names(table.df));
    colnames(table.df) = new.names;

    table.df$url = paste0(url.stem,hrefs);  # should be in order ...
    #names(table.df);
    head(table.df[,c(1:2,9)]);

    if( exists("local.data.path") )
		  {
      utils::write.table( table.df , file=mycache, quote=FALSE, col.names=TRUE, row.names=FALSE, sep="|");
      }
    return (table.df);
    }
  return (NA);
  }












wiki.findCoordinates = function(path.wiki.page, wiki.html, encoding="UTF-8")
  {
  mycache = paste0(path.wiki.page,"coordinates.txt");
  if(file.exists(mycache))
    {
    return ( as.numeric( unlist ( utils::read.csv(mycache, header=TRUE, quote="", sep="|") ) ) );
    }

  rvest.html = xml2::read_html(wiki.html, verbose=TRUE, encoding="UTF-8");

  coords = rvest.html %>%
              html_node(".geo-dec") %>%
              html_text();

  coords = convertStringBetweenEncodings(coords);

  coords = gsub("<U+00B0>","",coords,fixed=TRUE);
    tmp = strsplit(coords," ",fixed=TRUE)[[1]];
  latitude = wiki.parseDEC(tmp[1]);
  longitude = wiki.parseDEC(tmp[2]);


  if( exists("local.data.path") )
		{
    utils::write.table( cbind(latitude,longitude) , file=mycache, quote=FALSE, col.names=TRUE, row.names=FALSE, sep="|");
    }
  as.numeric( unlist( cbind(latitude,longitude) ) );
  }



wiki.parseDEC = function(str)
  {
  str = toupper(str);
  keys = c("N","S","E","W");
  for(key in keys)
    {
    str = gsub(key,paste0(" ",key),str,fixed=TRUE);
    }
  tm = strsplit(str," ",fixed=TRUE)[[1]]; # grabs correct key
  dec = as.numeric(tm[1]);
  dir = tm[2];
  if(dir == "S" || dir == "W") { dec = -1 * dec; }
  dec;
  }






convertStringBetweenEncodings = function(str = "48°24'42<U+2033>N 114°20'24<U+2033>W", from="UTF-8", to="ASCII", method="Unicode")
 {
 iconv(str, from, to, method);
 }





