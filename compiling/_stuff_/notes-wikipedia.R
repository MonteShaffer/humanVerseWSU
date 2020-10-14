





    # row.key = (strsplit( stringr::str_trim(rows[j]) ,"</th>")[[1]][1]) %>%
    #             convertStringBetweenEncodings() %>%
    #             strip_tags() %>%
    #             stringr::str_trim();



#val.info = wiki.cleanupClimateCell(tmp[[i]][looking]);
        #my.val = as.numeric( wiki.cleanupClimateCell(tmp[[i]][2]) ); # celsius

  #df = as.data.frame( transposeMatrix( my.rows ) );
  # df = as.data.frame( transposeMatrix( my.rows ) );

  #path.wiki.page = wiki.info$path.wiki.page;
  #wiki.html = wiki.info$wiki.html;


    # names(table.df);
    # dput(names(table.df));  c("State", "Capital", "Capital Since", "Area (mi2)", "Population (2019 est.)", "MSA/µSA Population\r\n(2019 est.)", "CSA Population\r\n(2019 est.)", "Rank in State\r\n(city proper)")
    # new-lines \r\n are not good ... I could manually rebuild or replace ...




convertStringBetweenEncodings = function(str = "48°24'42<U+2033>N 114°20'24<U+2033>W", from="UTF-8", to="ASCII", method="Unicode")
 {
 iconv(str, from, to, method);
 }




parseWikiStringBackToPage = function(str = "en^wikipedia^org-wiki-Columbia_Falls+_Montana")
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

parseWikiPageToString = function(url = "https://en.wikipedia.org/wiki/Columbia_Falls,_Montana")
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


wiki.findCoordinates = function(path.wiki.page,wiki.html)
  {
  mycache = paste0(path.wiki.page,"coordinates.txt");
  if(file.exists(cache))
    {
    return (utils::read.csv(mycache, header=TRUE, quote="", sep="|") );
    }

  rvest.html = xml2::read_html(wiki.html, encoding = "UTF-8");

  library(magrittr);
  coords = rvest.html %>%
              html_node(".geo-dec") %>%
              html_text();

  coords = gsub("Â°","",coords,fixed=TRUE);
    tmp = strsplit(coords," ",fixed=TRUE)[[1]];
  lat = parseDEC(tmp[1]);

  coords;

  }



parseDEC = function(str)
  {
  str = toupper(str);
  str = gsub("N"," N",str,fixed=TRUE);
  str = gsub("S"," S",str,fixed=TRUE);
  str = gsub("E"," E",str,fixed=TRUE);
  str = gsub("W"," W",str,fixed=TRUE);

  tm = strsplit(str," ",fixed=TRUE)[[1]];

  dec = as.numeric(tm[1]);
  dir = tm[2];
  if(dir == "S" || dir == "W")
    {
    dec = -1 * dec;
    }
  dec;
  }



parseDMSfromWiki = function(str)
  {
  # this string is the WIKIPEDIA FORMAT
  # "48°24′42″N 114°20′24″W"
  # 48°24′42″N
  # 114°20′24″W"
  str = stringr::str_trim(str);
    direction = stringr::str_sub(str, -1, -1);
  str = gsub(direction, "", str, fixed=TRUE);

  tm = base::strsplit(str,"°")[[1]];
    degrees = as.numeric( tm[1] );
  tmm = base::strsplit(tm[2],"′")[[1]];
    minutes = as.numeric( tmm[1] );
  seconds = as.numeric( gsub("″", "", tmm[2]) );

  list("degrees" = degrees, "minutes" = minutes, "seconds" = seconds, "direction" = direction);


  }

# cleanupDMStoDecLatitudeLongitude("48°22′13″N 114°11′20″W")
# cleanupDMStoDecLatitudeLongitude("48°24′42″N 114°20′24″W")
cleanupDMStoDecLatitudeLongitude = function(str = "48°24′42″N 114°20′24″W")
  {
  str = toupper(str);
  # this string is the WIKIPEDIA FORMAT
  str = gsub("[[:space:]]", "", str);
  str = gsub("N", "N ", str, fixed=TRUE);
  str = gsub("S", "S ", str, fixed=TRUE);

  tmp = base::strsplit(str," ")[[1]];

  lat =  parseDMS(tmp[1]);
    latitude = convertDMStoDEC(lat$degrees, lat$minutes, lat$seconds, lat$direction);
  long = parseDMS(tmp[2]);
    longitude = convertDMStoDEC(long$degrees, long$minutes, long$seconds, long$direction);

  c(latitude,longitude);
  }


# iconvlist();
# ANSI_X3.4-1986
# ISO-8859-1
# UTF-8
# UTF8
# iconv "sub" ... <U+xxxx>
# x <- "fa\xE7ile"; Encoding(x) <- "latin1"
# iconv(x, "latin1", "ASCII", "?")     # "fa?ile"
# iconv(xx, "UTF-8", "ASCII", "Unicode") # "fa<U+00E7>ile"
# xx <- iconv(x, "latin1", "UTF-8")
# charToRaw(xx);
# try(log("a"))
# try(log("a"),TRUE)
# print(try(log("a"), TRUE))
# convertStringBetweenEncodings("48°24′42″N 114°20′24″W", "UTF-8", "ISO-8859-1", "Unicode");
# convertStringBetweenEncodings("48°24′42″N 114°20′24″W", "UTF-8", "ASCII", "Unicode");
# url = "https://en.wikipedia.org/wiki/Ober%C3%A1";
# Oberá
# convertStringBetweenEncodings("Oberá"); # this breaks the world
# library(httr); r <- GET("http://httpbin.org/get")
convertStringBetweenEncodings = function(str = "48°24′42″N 114°20′24″W", from="UTF-8", to="ASCII", method="Unicode")
 {
 iconv(str, from, to, method);
 }



# iconv
# writeLines(iconv(readLines("tmp.html"), from = "ANSI_X3.4-1986", to = "UTF8"), "tmp2.html")
# writeLines(iconv(readLines("tmp.html"), from = "ANSI_X3.4-1986", to = "UTF8"), file("tmp2.html", encoding="UTF-8"))
# file(encoding="")
# url(encoding=""

downloadWikiPage = function(url="https://en.wikipedia.org/wiki/Columbia_Falls,_Montana", save.encoding="UTF-8")
  {
  str = parseWikiPageToString(url); # this is also reversible ... maybe pass encoding?



  path.wiki.page = paste0(path.wiki,str,"/");
  createDirRecursive(path.wiki.page);


  wiki.html = paste0(path.wiki.page,"page.html");

  do.nothing = grabHTML(wiki.html, url, encoding = save.encoding);
        ### will store it ...
  do.nothing = grabHTML(wiki.html, url);
        ### second time, will just say "it's cached" ...

  wiki.findCoordinates(path.wiki.page,wiki.html);  # rvest needs file .. not raw
  # utils::download.file(url, wiki.html, method="curl");


  }

# source("C:/_git_/MonteShaffer/humanVerseWSU/humanVerseWSU/R/functions-file.R");

# cloudflare

# wget works ... on debian console ...
