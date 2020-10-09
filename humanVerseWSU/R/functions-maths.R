
#' deg2rad
#'
#' @param degrees numeric (decimal form) of degrees
#'
#' @return numeric (decimal form) of radians
#' @export
#'
#' @examples
#' deg2rad(180);
#' deg2rad(45);
#' deg2rad(90);
#' deg2rad(60);
#' deg2rad(30);
deg2rad = function(degrees)
  {
  radians = degrees * (pi/180);
  radians;
  }

#' deg2rad
#'
#' @param radians numeric (decimal form) of radians
#'
#' @return numeric (decimal form) of degrees
#' @export
#'
#' @examples
#' rad2deg( deg2rad(180) );
#' rad2deg( deg2rad(45) );
#' rad2deg( deg2rad(90) );
#' rad2deg( deg2rad(60) );
#' rad2deg( deg2rad(30) );
#'
#' rad2deg( pi );
#' rad2deg( pi/2 );
#' rad2deg( pi/4 );
#' rad2deg( pi/3 );
#' rad2deg( pi/6 );
rad2deg = function(radians)
  {
  degrees = radians * (180/pi);
  degrees;
  }


#' buildBoundingBoxFromRadiusAndGivenLatitudeLongitude
#'
#' A store locator or other "element" locator can utilize a
#'  bounding box to generate an initial subset; from which
#'  final circle radius calculations can be performed.
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

