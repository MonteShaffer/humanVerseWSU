
# https://stackoverflow.com/questions/64839024/
# Rcpp::cppFunction("long long RShift(long long a, int b) { return a >> b;}");
# Rcpp::cppFunction("long long LShift(long long a, int b) { return a << b;}");
####### Rcpp::cppFunction("long long ShiftR(long long a, int b) { return a >> b;}");
####### Rcpp::cppFunction("long long ShiftL(long long a, int b) { return a << b;}");


# https://stackoverflow.com/questions/37121897/creating-r-package-with-rcpp-errors
#
#
#
bitShiftR = function(x, bits, unsigned=FALSE)
  {
  # as it gets to big, goes to "0" for positive, "-1" for negative: the signed bit
  if(!is.negative(x) | unsigned) { return( bitwShiftR(x,bits) ); }

  # if(!signed) { return( bitwShiftR(x,bits) ); }        # >>
  # if(is.positive(x)) { return( bitwShiftR(x,bits) ); }
  -bitwShiftR(-x,bits) - 1; #  - 1;                  # >>>
  }
# # bitShiftRs = function(x, bits) # signed  # >>
# #   {
# #   bitShiftR(x, bits, TRUE);
# #   }
#
#

bitOr = function(a, b)
  {
  if(!is.negative(a) && ( b <= -1 * 2^31) )
    {
    return (a + b);
    }
  if(!is.negative(b) && ( a <= -1 * 2^31) )
    {
    return (a + b);
    }
  bitwOr(a,b);
  }

bitShiftL = function(x, bits, unsigned=FALSE)
  {
  # currently single bit, built-in functions could do vector
  # if at end, we stay at end ? what about postive?
  # if(x <= -2^31) { return( -2^31 ); }
  if(!is.negative(x) | unsigned)
    {
    tmp = suppressWarnings( bitwShiftL(x,bits) );                # <<<
    if(is.na(tmp)) { tmp = -2^31; }  # 0x80 << 24
    return( tmp );
    }
  # bitShiftL( 0x80, (w %% 32))
  # if(!signed) { return( bitwShiftL(x,bits) ); }        # <<
  # if(is.positive(x)) { return( bitwShiftL(x,bits) ); }
  ## warning is OVERFLOW ... should return ... -2147483648
  ## -1 * 2^31
  tmp = suppressWarnings( -bitwShiftL(-x,bits) ); # - 1;                  # <<<
  if(is.na(tmp))
    {
    tmp = 2^31;
    if(is.negative(x)) { tmp = -1 * tmp; }
    } # overflowing ... shifted too far
  tmp;
  }
# # bitShiftLs = function(x, bits)  # signed  # <<
# #   {
# #   bitShiftL(x, bits, TRUE);
# #   }
#

