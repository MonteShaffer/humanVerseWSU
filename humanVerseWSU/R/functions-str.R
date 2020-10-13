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
#' @param delimiter character(s) to delimit the split
#' @param str a character string to be split
#'
#' @return a character vector
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



printMatrix = function(mat, digits=3) # align decimals ? ... center ... latex
  {
  n.rows = nrow(mat);
  my.row.names = rownames(mat);
  my.col.names = colnames(mat);

  mat = as.character( round(mat, digits = digits) );
  mat = matrix(mat, nrow=n.rows, byrow=TRUE);
    rownames(mat) = my.row.names;
    colnames(mat) = my.col.names;
  print(mat);
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
