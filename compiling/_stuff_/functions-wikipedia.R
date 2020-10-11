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
  lat = wiki.parseDEC(tmp[1]);
  coords;
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




wiki.parseDMSfromWiki = function(str)
  {
  # this string is the WIKIPEDIA FORMAT
  # "48°24′42″N 114°20′24″W"
  # #48°24′42″N
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
wiki.cleanupDMStoDecLatitudeLongitude = function(str = "48°24′42″N 114°20′24″W")
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

























wiki.parseDMSfromWiki = function(str)
  {
  str = stringr::str_trim(str);
    direction = stringr::str_sub(str, -1, -1);
  str = gsub(direction, "", str, fixed=TRUE);

  # tm = base::strsplit(str,"°")[[1]];
  #   degrees = as.numeric( tm[1] );
  # tmm = base::strsplit(tm[2],"′")[[1]];
  #   minutes = as.numeric( tmm[1] );
  # seconds = as.numeric( gsub("″", "", tmm[2]) );

  list("degrees" = degrees, "minutes" = minutes, "seconds" = seconds, "direction" = direction);


  }

# cleanupDMStoDecLatitudeLongitude("48°22′13″N 114°11′20″W")
# cleanupDMStoDecLatitudeLongitude("48°24′42″N 114°20′24″W")
wiki.cleanupDMStoDecLatitudeLongitude = function(str = "48°24′42″N 114°20′24″W")
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





  #
  # convertStringBetweenEncodings(
  #     "Mean maximum <U+00C2><U+00B0>F (<U+00C2><U+00B0>C)",
  #     from="UTF-8", to="ISO-8859-1", method="");
  #
  # x = "Mean maximum <U+00C2><U+00B0>F (<U+00C2><U+00B0>C)";
  #
  # strip_tags(x);
  #
  # stringi::stri_unescape_unicode(x)
  # # https://stackoverflow.com/questions/17761858/converting-a-u-escaped-unicode-string-to-ascii
  #




# iconv
# writeLines(iconv(readLines("tmp.html"), from = "ANSI_X3.4-1986", to = "UTF8"), "tmp2.html")
# writeLines(iconv(readLines("tmp.html"), from = "ANSI_X3.4-1986", to = "UTF8"), file("tmp2.html", encoding="UTF-8"))
# file(encoding="")
# url(encoding=""


