#' is.whole.number
#'
#' `is.integer` doesn't operate as you would expect, this does
#'
#' @param x number (and vector) to evaluate
#' @param tol tolerance of "zero"
#'
#' @return TRUE OR FALSE
#' @export
#'
#' @examples
#'
#' is.whole.number(1);
#' is.whole.number(1.1);
#'
#' is.whole.number(0);
#' is.whole.number(0.1);
#'
#' is.whole.number(-1);
#' is.whole.number(-1.1);
#'
#' is.whole.number(rnorm(5));
#' is.whole.number(rpois(5,1));
#'
is.whole.number = function(x, tol = sqrt(.Machine$double.eps))
  {
  # See ?is.integer
  abs(x - round(x)) < tol;
  }


#' is.positive
#'
#' @param x number (and vector) to evaluate
#' @param tol tolerance of "zero"
#'
#' @return TRUE OR FALSE
#' @export
#'
#' @examples
#'
#' is.positive(1);
#' is.positive(0);
#' is.positive(-1);
#' is.positive(c(-1*1:5,1:5));
#'
is.positive = function(x, tol = sqrt(.Machine$double.eps))
  {
  x > tol;
  }

#' is.negative
#'
#' @param x number (and vector) to evaluate
#' @param tol tolerance of "zero"
#'
#' @return TRUE OR FALSE
#' @export
#'
#' @examples
#'
#' is.negative(1);
#' is.negative(0);
#' is.negative(-1);
#' is.negative(c(-1*1:5,1:5));
#'
is.negative = function(x, tol = sqrt(.Machine$double.eps))
  {
  x < ( -1 * tol );
  }

#' get.sign
#'
#' @param x number (not vector) to evaluate
#' @param other default is NA, zero ... 0 ... may be applicable at times
#' @param tol tolerance of "zero"
#' @param return by default "number" otherwise "character"
#'
#' @return 1 (+) if positive, -1 (-) if negative, "other" otherwise
#' @export
#'
#' @examples
#'
#' get.sign(-1);
#' get.sign(-1, return="chr");
#' get.sign(1);
#' get.sign(1, return="character");
#' get.sign(0);
#' get.sign(0, return="char");
#' get.sign(0, 0);
#' get.sign(0, ".", return="char");
#'
#' # get.sign(-1*(1:10)); # doesn't work
#'
#' get.sign(pi);
#' get.sign(-pi);
#' get.sign( sin(pi) );
#' get.sign( -sin(pi) );
#' get.sign( sin(pi), 0 );
#' get.sign( -sin(pi), 0 );
#' get.sign( sin(pi), tol = .Machine$double.eps^2 );
#' get.sign( -sin(pi), tol = .Machine$double.eps^2  );
#'
get.sign = function(x, other=NA, return="number", tol = sqrt(.Machine$double.eps))
  {
  myval = other;
  if(is.negative(x,tol)) { myval="-"; }
  if(is.positive(x,tol)) { myval="+"; }
  # print(myval);
  if(return == "number")
    {
    if(is.na(myval))
      {
      if(is.numeric(other)) { return(other); } else { return(NA); }
      }
    if(myval == "-") { return(-1); }
    if(myval == "+") { return(1); }
    if(is.numeric(other)) { return(other); }
    return(NA);
    }
  as.character(myval);
  }

