
# these functions address some system-issues with R ...

# isClose ... isTRUE (all.equal) to deal with tolerance issues of comparing two numbers
# https://stackoverflow.com/questions/63787649/

# isClose is like isEqual, but with a tolerance in case we have problems...
# .Machine

# a is length 1 or b is length 1 or both ...
# if both are not length 1, they have to be the same length ...
# we will return vector of logical (bool) that is the longest length ...
# this will allow it to be natively used in subset notation ... if of vector structure

# this was a bug that came up in the simulations research project with jillian
# took about 4 hours to figure out that this was the problem
# isClose will fix this issue ...




#' isClose
#'
#' Due to issues regarding float-point precision, this function
#' assesses if two numerics are equal "almost".
#'
#' See:  https://stackoverflow.com/questions/63787649/
#'
#' The structure of \code(isClose) allows it to be used in traditional subsetting
#'  notation.
#'
#' @param a number(s) ... numeric vector of length 1+
#' @param b number(s) ... numeric vector of length 1+
#' @param tol what is the tolerance level of "equalish" ...
#'  defaults to what is used in the function \code(all.equal)
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
#' a = sample(N, 5, replace=T);
#' isClose( a, n2, myTol);
#' b = sample(N, 5, replace=T);
#' isClose( n1, b, myTol);
#'
#' a = sample(N, 5, replace=T);
#' b = sample(N, 5, replace=T);
#' isClose( a, b, myTol);
#'
#' a = sample(N, 9, replace=T);
#' b = sample(N, 5, replace=T);
#' isClose( a, b, myTol);  # prints warning, returns matrix
#'
#'
isClose = function(a,b, tol=sqrt(.Machine$double.eps) )
  {
  # we assume no issues with na.omit
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
    warning("Not of Equal length ... Computing Matrix");  # could do cross-product [combinatorics a.n*b.n and store in matrix] of comparisons

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

