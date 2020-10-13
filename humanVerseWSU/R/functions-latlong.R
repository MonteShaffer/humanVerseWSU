



#' parseDMSfromFormat
#'
#'
#' @family LatLong
#'
#'
#' @param str a string form of lat/long
#' @param format
#'
#' @return a list of $degrees , $minutes , $seconds, $direction
#' @export
#'
#' @examples
#' parseDMSfromFormat();
#' parseDMSfromFormat("3 8 29.7335529232441", "MeAsUr");
#' parseDMSfromFormat("-3 8 29.7335529232441");
#' parseDMSfromFormat("-3 8 29.7335529232441 S");
parseDMSfromFormat = function(str="3 8 29.7335529232441", format="measurements")
  {
  format.4 = substr(toupper(format),1,4);
  str = trimMe(str);

  if(format.4 == "MEAS")
    {
    tmp = explodeMe(" ", str);
      direction = NULL;
    if(!is.na(tmp[4])) { direction = tmp[4]; }
    tmp = as.numeric(tmp[1:3]);

    return (  list( "degrees" = tmp[1], "minutes" = tmp[2],
                    "seconds" = tmp[3], "direction" = direction) );
    }

  }




#' convertDECtoDMS
#'
#'
#' @family LatLong
#'
#'
#' @param decimal lat/long in decimal form
#' @param which is it "latitude" or "longitude" ... matters for c("N","S","E","W")
#' @param return one of :: c("list","measurements","UTF8")
#' @param include.direction if TRUE, string returns will have direction appended to end
#'
#' @return either a list or a string based on `return` option; default is "list"
#' @export
#'
#' @examples
#' convertDECtoDMS(3.1415926535897932384626, "latitude");
#' convertDECtoDMS(-3.1415926535897932384626, "la");
#'
#'
#' convertDECtoDMS(3.1415926535897932384626, "latitude", "measurements");
#' measurements::conv_unit("3 8 29.7335529232441", "deg_min_sec", "dec_deg");
#' measurements::conv_unit(3.1415926535897932384626, "dec_deg", "deg_min_sec");
#'
#' convertDECtoDMS(-3.1415926535897932384626, "longitude");
#' convertDECtoDMS(-3.1415926535897932384626, "longitude", "UTF8");
#' convertDECtoDMS(3.1415926535897932384626, "lo");
#'
#' # convertDECtoDMS(-31.0125,"longitude","UTF8"); # removed, as it breaks R to source it ...
#'
#' convertDECtoDMS( convertDMStoDEC(30,60,45,"N"), "latitude", return = "MEAS", include.direction=FALSE);
#' convertDECtoDMS( convertDMStoDEC(30,60,45,"N"), "latitude", return = "MEAS", include.direction=TRUE);
#' convertDECtoDMS( convertDMStoDEC(-30,60,45,"N"), "latitude", return = "MEAS", include.direction=FALSE);
#' convertDECtoDMS( convertDMStoDEC(-30,60,45,"N"), "latitude", return = "MEAS", include.direction=TRUE);
convertDECtoDMS = function(decimal = 105.38, which="latitude", return="list", include.direction=TRUE, decimal.seconds=TRUE)
  {
  # x = c(sin(pi), -sin(pi))
  which.2 = substr(tolower(which),1,2);

  # conv_unit("105 22 48", "deg_min_sec", "dec_deg");
  # conv_unit("105.38", "dec_deg", "deg_min_sec");
  direction = if(which.2=="lo") "E" else "N"; # defaults to latitude
  isNegative = (decimal < 0);
  my.sign = "";
  if(isNegative)
    {
    my.sign = "-";
    direction = if(which.2=="lo") "W" else "S";
    }
  degrees.dec = abs(decimal);
  degrees = floor(degrees.dec);
    remainder = degrees.dec - degrees;
  minutes.dec = 60*remainder;
  minutes = floor(minutes.dec);
    remainder = minutes.dec - minutes;
  seconds.dec = 60*remainder;
  seconds = floor(seconds.dec);

  my.seconds = seconds;
  if(decimal.seconds) { my.seconds = seconds.dec; }


  return.4 = substr(toupper(return),1,4);
  if(return.4 == "LIST")   # "list"
    {
    return (  list("degrees" = degrees, "minutes" = minutes, "seconds" = my.seconds, "direction" = direction) );
    }
  if(return.4 == "MEAS" || return == "CONV")  # measurements::conv_units
    {
    # measurements::conv_units
    # measurements # conv_units
    stem = paste0(degrees," ",minutes," ", my.seconds);
      if(include.direction) { stem = paste0(stem, " ", direction); } else { stem = paste0(my.sign, stem); }
    return ( stem );
    }

  }


#' convertDMStoDEC
#'
#'
#' @family LatLong
#'
#'
#' @param degrees numeric (integer likely)
#' @param minutes  numeric (integer likely)
#' @param seconds  numeric (integer ??)
#' @param direction one of : c("N","S","E","W");
#'
#' @return updated form as numeric (decimal)
#' @export
#'
#' @examples
#' convertDMStoDEC(30,60,45,"S");
#' convertDMStoDEC(-30,60,45,"S");  # negative is only working correctly on degrees
#'
#' convertDMStoDEC(30,60,45,"N");
#' convertDMStoDEC(-30,60,45,"N");
#'
#' convertDMStoDEC("105",direction="E");
#' convertDMStoDEC("105",direction="W");
#'
#'
#' convertDMStoDEC("105 22 48",direction="E",format="measurements");
#' convertDMStoDEC("105 22 48",direction="W",format="MeAsUr");
#'
#' convertDMStoDEC("105 22 48 E", format="MeAsUr");
#' convertDMStoDEC("-105 22 48 E", format="MeAsUr");
#' convertDMStoDEC("105 22 48 W", format="MeAsUr");
#' convertDMStoDEC("-105 22 48 W", format="MeAsUr");
#'
#'
#' convertDMStoDEC(30,60,45,"N");
#'
#' convertDMStoDEC( convertDECtoDMS(3.1415926535897932384626, "lat", "meas"), format="meas");
convertDMStoDEC = function(degrees, minutes=0, seconds=0, direction="N", format=NULL)
  {
  degrees.raw = degrees;

  if(is.character(degrees))
    {
    if(is.null(format))
      {
      degrees = as.numeric(degrees);
      } else { degrees = parseDMSfromFormat(degrees,format=format); }
    }

  if(is.list(degrees))
    {
    deg = degrees; # copy
    degrees = deg$degrees;
    minutes = deg$minutes;
    seconds = deg$seconds;
    if(!is.null(deg$direction)) { direction = deg$direction; }
    }

  mydeg = degrees; # original sign
  degrees = abs(degrees);

  direction = toupper(direction);

  reverse = 1;
  if(mydeg < 0) { reverse = -1 * reverse; }
  if (direction == "S" || direction == "W")
    {
    reverse = -1 * reverse;
    }

  # https://stackoverflow.com/questions/1140189/
  dd = degrees + minutes/60 + seconds/(60*60);

    dd = dd * reverse;

  dd;
  }





#' buildBoundingBoxFromRadiusAndGivenLatitudeLongitude
#'
#' A store locator or other "element" locator can utilize a
#'  bounding box to generate an initial subset; from which
#'  final circle radius calculations can be performed.
#'
#'
#' @family LatLong
#'
#'
#' @param my.radius numeric
#' @param my.latitude numeric
#' @param my.longitude numeric
#' @param my.units a valid "length" measure from measurements:conv_unit
#'
#' @return a numeric vector of length 4: c(latitude.lower, latitude.upper, longitude.lower, longitude.upper);
#' @export
#'
#' @examples
#' buildBoundingBoxFromRadiusAndGivenLatitudeLongitude(10, 46.76551, -117.1919, "mi");
#' buildBoundingBoxFromRadiusAndGivenLatitudeLongitude(10, 46.76551, -117.1919, "km");
buildBoundingBoxFromRadiusAndGivenLatitudeLongitude = function(my.radius, my.latitude, my.longitude, my.units="mi")
  {
  # default values are in miles
  option = c("angstrom", "nm", "um", "mm", "cm", "dm", "m", "km", "inch", "ft",
              "yd", "fathom", "mi", "naut_mi", "au", "light_yr", "parsec", "point");
  if(!is.element(my.units,option)) { my.units = "mi"; } # miles
  factor.lat  = 68.703; if(my.units != "mi") { factor.lat  = measurements::conv_unit(68.703, "mi", my.units); }
  factor.long = 69.172; if(my.units != "mi") { factor.long  = measurements::conv_unit(69.172, "mi", my.units); }

  delta.latitude = my.radius / factor.lat ;
  delta.longitude = my.radius / (factor.long * cos(deg2rad(my.longitude)));

  latitude.lower = my.latitude - delta.latitude;
  latitude.upper = my.latitude + delta.latitude;

  longitude.lower = my.longitude - delta.longitude;
  longitude.upper = my.longitude + delta.longitude;

  c(latitude.lower, latitude.upper, longitude.lower, longitude.upper);
  }
