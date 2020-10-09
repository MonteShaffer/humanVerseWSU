
parseDMS = function(str)
  {
  # this string is the WIKIPEDIA FORMAT
  # 48°24′42″N
  # 114°20′24″W
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



#' cleanupPersonalityDataFrame
#'
#' @param personality.raw The raw dataframe which can auto-populate
#'
#' @return An updated dataframe, cleaned and sorted
#' @export
cleanupPersonalityDataFrame = function(personality.raw = readRDS( system.file("extdata", "personality-raw.rds", package="humanVerseWSU") ) )
  {
  # personality.clean = cleanupPersonalityDataFrame();
  df = removeColumnsFromDataFrame(personality.raw, "V00");
  dim(df);  # 838


  ywd.cols = c("year","week","day");
  ywd = convertDateStringToFormat( df$date_test,
                                    c("%Y","%W","%j"),
                                    ywd.cols,
                                    "%m/%d/%Y %H:%M"
                                  );

  ndf = replaceDateStringWithDateColumns(df,"date_test",ywd);
  ndf = sortDataFrameByNumericColumns(ndf, ywd.cols, "DESC");
  ndf = removeDuplicatesFromDataFrame(ndf, "md5_email");

  dim(ndf); # 678
  ndf;
  }

