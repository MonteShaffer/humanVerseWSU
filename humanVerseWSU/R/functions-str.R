library(stringr);

#' trimMe
#'
#' @param str character string to be "trimmed"
#'
#' @return updated trimmed string
#' @export
#'
#' @examples
#' trimMe("    four   scores    and  seven      years     ");
#' trimMe("\r\n    four   scores    and  seven      years   \t\t  ");
trimMe = function(str)
  {
  stringr::str_trim(str);
  }


#' explodeMe
#'
#' Similar to javascript.split and php.explode
#'
#' @param delimiter character(s) to delimit the split
#' @param str a character string to be split
#'
#' @return a character vector
#' @export
#'
#' @examples
#' str = removeWhiteSpace("    four   scores    and  seven      years     ", "[s]");
#' strvec = explodeMe("[s]", str);
#' strvec[3];
explodeMe = function(delimiter=" ",str="hello friend")
  {
  strsplit(str,delimiter, fixed=TRUE)[[1]];
  }



#' implodeMe
#'
#' Similar to javascript.join and php.implode
#'
#' @param delimiter character(s) to unsplit with
#' @param strvec a character string to be unsplit
#'
#' @return a character string
#' @export
#'
#' @examples
#' implodeMe();
#'
#'
#' str = removeWhiteSpace("    four   scores    and  seven      years     ", "[s]");
#' strvec = explodeMe("[s]", str);
#' implodeMe(",", strvec);
#'
implodeMe = function(delimiter=" ",strvec=c("hello","friend") )
  {
  paste0(strvec, collapse=delimiter);
  }



#' removeWhiteSpace
#'
#' @param str character string to be adjusted
#' @param replace what will we replace the white space with
#' @param n number of spaces to find and replace
#' @param pre.trim if TRUE, trims the string before removing white space within
#' @param post.trim if TRUE, trims the string after removing white space within
#'
#' @return updated adjusted string
#' @export
#'
#' @examples
#' removeWhiteSpace("    four   scores    and  seven      years     ");
#' removeWhiteSpace("\r\n    four   scores    and  seven      years   \t\t  ");
#'
#' removeWhiteSpace("    four   scores    and  seven      years     ", "");
#' removeWhiteSpace("    four   scores    and  seven      years     ", "[s]");
#'
#' removeWhiteSpace("The quick brown fox jumps over the lazy dog", ""); # default is 2
#' removeWhiteSpace("The quick brown fox jumps over the lazy dog", "", n=1);
removeWhiteSpace = function( str, replace=" ", n=2,
                              pre.trim=TRUE, post.trim=TRUE )
  {
  # ?regex
  # $string = preg_replace('/\s+/', '', $string);
  if(pre.trim) { str = trimMe(str); }
    regex.s = paste0("[[:space:]]{",n,",}");
  str = gsub( regex.s, replace, str );
  # str = gsub("[[:space:]]", remain, str); # ... call it twice ?
  if(post.trim) { str = trimMe(str); }
  str;
  }




#' printPaste0
#'
#' merges print(paste0 into one function
#'
#' @param ...  one or more R objects, to be converted to character vectors.
#' @param collapse an optional character string to separate the results
#' @param recycle0 TRUE / FALSE
#'
#' @return prints the result
#' @export
printPaste0 = function(... , collapse = NULL, recycle0 = FALSE)
  {
  # paste0(..., collapse) is equivalent to paste(..., sep = "", collapse), slightly more efficiently.
  print(paste0(... , collapse = collapse, recycle0 = recycle0) );
  }



#' printPaste
#'
#' merges print(paste into one function
#'
#' @param ...  one or more R objects, to be converted to character vectors.
#' @param sep a character string to separate the terms
#' @param collapse an optional character string to separate the results
#' @param recycle0 TRUE / FALSE
#'
#' @return prints the result
#' @export
printPaste = function(... , sep = " ", collapse = NULL, recycle0 = FALSE)
  {
  print(paste(... , sep = sep, collapse = collapse, recycle0 = recycle0) );
  }



#' printMatrix
#'
#' @param mat numeric matrix
#' @param digits numeric, digits to round
#' @param zeroIsh if TRUE, zeroIsh is also called
#' @param z.digits numeric, digits to perform zeroIsh
#'
#' @return matrix, as character string
#' @export
#'
#' @examples
#' x = cbind( stats::rnorm(10,0,1), stats::rnorm(10,3,1), stats::rnorm(10,9,1) );
#' printMatrix( stats::cov(x), 3);
#' printMatrix( stats::cor(x), 3);
#'
#' x = matrix( c(sin(pi), cos(pi), -sin(pi), -cos(pi)), nrow=2);
#' zeroIsh(x);
#' printMatrix( x, 3 );
#' printMatrix( x, 22, zeroIsh = FALSE);
#' printMatrix( x, 22, zeroIsh = TRUE);
#' printMatrix( x, 22, zeroIsh = TRUE, z.digits=16);
printMatrix = function(mat, digits=3, zeroIsh=TRUE, z.digits=6) # align decimals ? ... center ... latex
  {
  n.rows = nrow(mat);
  my.row.names = rownames(mat);
  my.col.names = colnames(mat);

  mat.round = round(mat, digits = digits);
  if(z.digits > 22) { z.digits = 22; } # is this the maximum on all systems?
  if(zeroIsh) { mat.round = zeroIsh(mat.round, z.digits); }

  mat = as.character( mat.round );
  mat = matrix(mat, nrow=n.rows, byrow=FALSE);
    rownames(mat) = my.row.names;
    colnames(mat) = my.col.names;
  print(mat);
  }


#' strPadLeft
#'
#' When caching pages of content, useful for organization.
#'  (e.g., page1.html becomes page_001.html)
#'
#' @param str The 'string' (can be a number)
#' @param final.str.len How long the final str is to be
#' @param padding Fill with, default is "0" (zero)
#'
#' @return string
#' @export
#'
#' @aliases numberPadLeft
#'
#' @examples
#' strPadLeft(33,1);
#' strPadLeft(33,2);
#' strPadLeft(33,3);
#' strPadLeft(33,4);
strPadLeft = function(str, final.str.len, padding="0")
  {
  stringr::str_pad(str, final.str.len, "left", padding);
  }

#' strPadRight
#'
#'
#'
#' @param str The 'string' (can be a number)
#' @param final.str.len How long the final str is to be
#' @param padding Fill with, default is "0" (zero)
#'
#' @return string
#' @export
#'
#' @aliases numberPadRight
#'
#' @examples
#' strPadRight("33.01",5);
#' strPadRight("33.01",6);
#' strPadRight("33.01",7);
#' strPadRight("33.01",8);
strPadRight = function(str, final.str.len, padding="0")
  {
  stringr::str_pad(str, final.str.len, "right", padding);
  }


#' strlen
#'
#' @param str the character string
#'
#' @return the numeric length of said string
#' @export
#'
#' @examples
#' strlen("3.1415926535897932384626");
#' strlen( pi );
strlen = function(str)
  {
  # history :: # https://en.cppreference.com/w/c/string/byte/strlen
  # http://www.cplusplus.com/reference/cstring/
  # https://en.wikipedia.org/wiki/C99
  # https://www.programiz.com/c-programming/library-function/string.h/strlen
  nchar( as.character(str), type="chars");
  }



#' roundMeToString
#'
#' @param val a numeric value
#' @param digits number of digits to round
#' @param leadingZero TRUE / FALSE ... include the leading zero, if necessary
#' @param decimal.sep The decimal separator, default is "."
#'
#' @return string of numeric based on parameters
#' @export
#'
#' @examples
#' roundMeToString(pi, 5);
#' roundMeToString(3.1415926535897932384626, 9);
#'
#' roundMeToString(0.9, 3);
#' roundMeToString(0.9, 3, FALSE);
#'
#' roundMeToString(1.9, 3);
#' roundMeToString(1.9, 3, FALSE);
#'
#' roundMeToString(-0.9, 3);
#' roundMeToString(-0.9, 3, FALSE);
roundMeToString = function(val, digits=3, leadingZero=TRUE, decimal.sep=".")
  {
  isNegative = (val < 0);
  isLessThanOne = (val > 0 && val < 1);

  str = as.character( round(val, digits=digits) );
  tmp = explodeMe(decimal.sep, str);

  whole = tmp[1];
  decimal = tmp[2];

  delta = digits - strlen(decimal);
  if(delta > 0)
    {
    decimal = strPadRight(decimal, digits, "0");
    }

  if(whole == "0" && !leadingZero) { whole = "";}

  paste0(whole, decimal.sep, decimal);
  }

# not just round length, but total length, so they align on decimal centers
vectorRoundMeToString = function(strvec, digits=3, leadingZero = TRUE, alignDecimal=TRUE)
  {

  }

























































#' getKeysFromStringWithSeparator
#'
#' @param str What is the string?
#' @param sep What separates elements in string?
#' @param lower.case Convert input to lowercase?
#'
#' @return a character vector
#' @export
#'
#' @examples
#' str = "Actor,Producer,Writer";
#' keys = getKeysFromStringWithSeparator(str);
#' keys;
getKeysFromStringWithSeparator = function(str, sep=",", lower.case=TRUE)
      {
      # I believe there are two functions strsplit and str_split.  I should pick on.
      if(lower.case) { str = tolower(str);}
      vals = stringr::str_split(str,sep);
      f.vals = c();
      for(val in vals)
        {
        val = stringr::str_trim(val);
        f.vals = c(f.vals,val);
        }
      f.vals;
      }



# normalizePath("C:/Users/Alexander Nevsky/Dropbox/WSU-419/Fall 2020/__student_access__/sample_latex_files/Multivariate-2009/datasets/MBA_CAR_ATTRIB.txt");
# windowsNormalize ...



# for shared notebooks
stringifyLibrary = function(str)
  {
  str = gsub(" ","",str, fixed=TRUE);
  str = gsub("/","",str, fixed=TRUE);
  str = gsub(":","",str, fixed=TRUE);
  str = gsub("-","",str, fixed=TRUE);
  str = gsub(".","",str, fixed=TRUE);
  str;
  }

