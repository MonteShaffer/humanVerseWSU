

#' isClose
#'
#' Due to issues regarding float-point precision, this function
#' assesses if two numerics are equal "almost".
#'
#' See:  \url{https://stackoverflow.com/questions/63787649/}
#'
#' The structure of this function allows it to be used in traditional subsetting
#'  notation.
#'
#'
#' @family Maths
#'
#' @param a number(s) ... numeric vector of length 1+
#' @param b number(s) ... numeric vector of length 1+
#' @param tol what is the tolerance level of "equalish" ...
#'  defaults to what is used in the function \code{\link{all.equal}}
#'
#' @return vector of logical (bool) with the maximum length of (a,b)
#' @export
#'
#' @examples
#' options(digits=22);
#' n1 = 0.14999999999999999;
#' n2 = 0.15;
#' n3 = 0.15000000000000002;
#' N = c(n1,n2,n3);
#'
#' isClose(n1,n2);
#' isClose(n2,n3);
#' isClose(n1,n3);
#'
#' myTol = 0.00000000000000002;
#' isClose(n1,n2, myTol);
#' isClose(n2,n3, myTol);
#' isClose(n1,n3, myTol);
#'
#' a = sample(N, 5, replace=TRUE);
#' isClose( a, n2, myTol);
#' b = sample(N, 5, replace=TRUE);
#' isClose( n1, b, myTol);
#'
#' a = sample(N, 5, replace=TRUE);
#' b = sample(N, 5, replace=TRUE);
#' isClose( a, b, myTol);
#'
#' a = sample(N, 9, replace=TRUE);
#' b = sample(N, 5, replace=TRUE);
#' isClose( a, b, myTol);  # prints warning, returns matrix
#'
#' sin(pi) == 0;  # Ben's example
#' isClose( sin(pi), 0 );
isClose = function(a,b, tol=sqrt(.Machine$double.eps) )
  {
  # we assume no issues with stats::na.omit
  a.n = length(a);
  b.n = length(b);

  if(a.n == b.n && b.n == 1)  # one of each
    {
    return ( isTRUE( all.equal (a,b, tol) ) );
    }
  if(a.n == b.n) # parallel vector comparison (pairwise)
    {
    r = logical(a.n);
    for(i in 1:a.n)
      {
      r[i] = isTRUE( all.equal (a[i],b[i], tol) );
      }
    return (r);
    }

  # could be unequal vectors, not one
  if(a.n != 1 && b.n !=1)
    {
    warning("Not of Equal length ... Computing Matrix");  # cross-product [combinatorics a.n*b.n and store in matrix] of comparisons

    m = matrix(NA, nrow=a.n, ncol=b.n);
    for(i in 1:a.n)
      {
      for(j in 1:b.n)
        {
        m[i,j] = isTRUE( all.equal (a[i],b[j], tol) );
        }
      }
      colnames(m) = c(paste0("b.",1:b.n));
      rownames(m) = c(paste0("a.",1:a.n));
    return (m);
    }

  one = if(a.n > b.n) { b } else { a }
  vec = if(a.n > b.n) { a } else { b }

  r = logical(length(vec));
  for(i in 1:length(vec))
    {
    r[i] = isTRUE( all.equal (one,vec[i], tol) );
    }
  r;
  }




#' zeroIsh
#'
#'
#' @family Maths
#'
#'
#' @param x a vector of numerics (matrix also works)
#' @param digits how precise to label a value zero
#'
#' @return updated vector with zeroes
#' @export
#'
#' @examples
#' options(scipen = 999);
#' options(digits = 22);
#'
#' x = c(sin(pi), -sin(pi));
#' zeroIsh(x);
#' zeroIsh(x, 8);
zeroIsh = function(x, digits=getOption("digits"))
  {
  # zapsmall has log10 feature

  # positive values
  x.pos = 1 / 10^digits
  x[((x > 0) & (x < x.pos))] = 0L;
  # negative values
  x.neg = -1 * x.pos;
  x[((x < 0) & (x > x.neg))] = 0L;

  # round(x, digits=digits);
  x;
  }




#' deg2rad
#'
#' @family Maths
#'
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



#' rad2deg
#'
#'
#' @family Maths
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


#' fahrenheit2celsius
#'
#' @param fahrenheit Temperature in fahrenheit
#'
#' @return Temperature in celsius
#' @export
#'
#' @examples
#'
#' fahrenheit2celsius(104);
#'
fahrenheit2celsius = function(fahrenheit)
  {
  5/9 * ( fahrenheit - 32);
  }

#' celsius2fahrenheit
#'
#' @param celsius Temperature in celsius
#'
#' @return Temperature in fahrenheit
#' @export
#'
#' @examples
#'
#' celsius2fahrenheit(40);
#'
celsius2fahrenheit = function(celsius)
  {
  32 + celsius * 9/5;
  }


#' differentSigns
#'
#'
#' Tolerance around zero is not included herein ...
#'
#' @family Maths
#'
#' @param a single numeric value
#' @param b single numeric value
#'
#' @return FALSE if they have the same sign, TRUE otherwise
#' @export
#'
#' @examples
#'
#' differentSigns(1,-1);
#' differentSigns(-1,-1);
#' differentSigns(-1,1);
#' differentSigns(0,0);
#' differentSigns(1-2i,-3-4i);
#' differentSigns(1-2i,-3-4i, part="Im");
#'
differentSigns = function(a,b, tol = sqrt(.Machine$double.eps), part="Re")
  {
  # functions-number.R
  a.part = Re(a); if(part=="Im") { a.part = Im(a); }
  a.sign = get.sign(a.part,0);  # if both are zero, NA == NA
  b.part = Re(b); if(part=="Im") { b.part = Im(b); }
  b.sign = get.sign(b.part,0);  # therefore same sign
  if(a.sign == b.sign) { return(FALSE); }
  return(TRUE);
  # if(Re(a) > 0 && Re(b) < 0) { return(TRUE); }
  # if(Re(a) < 0 && Re(b) > 0) { return(TRUE); }
  # return(FALSE);
  }


