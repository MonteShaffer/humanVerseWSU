
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
#' @family System
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

