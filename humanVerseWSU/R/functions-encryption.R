

#' md5
#'
#' @param strvec a character vector of strings
#' @param serialize a parameter passed to the function digest()
#' @param times how many times `n` you want to perform the md5(str)
#'
#' @return a character vector of equal length, with md5 hashes of each string in original vector
#' @export
#'
#' @examples
#' md5("password");
#'   # the encryption is one-way, yet a dictionary can "hack"
#'   # See <https://crackstation.net/> for an example Rainbow Table
#'   # The best modern tool I have found is SJCL <https://github.com/bitwiseshiftleft/sjcl>
#' md5("password", times=9); # this will make the Rainbow Table work
#'   # if you know the original value and the times=n offset, you could build an internal Rainbow Table
#'
#' md5("The quick brown fox jumps over the lazy dog");
#' md5( unlist( base::strsplit("The quick brown fox jumps over the lazy dog", " ", fixed=TRUE) ) );
#'
#' md5("monte.shaffer@gmail.com", 9);
#' TODO: let's migrate to openssl ... better package ... maybe slower ... openssl has vector function, so will be faster ...
#' ## DEPRECATED
# md5 = function(strvec, times=1, serialize=FALSE)
#   {
#   # digest is not vector driven ... # `apply` could work ...
#   n = length(strvec);
#   nvec = c();
#   for(i in 1:n)
#     {
#     myval = digest::digest(strvec[i], algo="md5", serialize=serialize);
#     if(times > 1)
#       {
#       for(j in 2:times)
#         {
#         myval = digest::digest(myval, algo="md5", serialize=serialize);
#         }
#       }
#     nvec[i] = myval;
#     }
#   nvec;
#   }


#' md5
#'
#' @param strvec a character vector of strings
#' @param serialize a parameter passed to the function digest()
#' @param times how many times `n` you want to perform the md5(str)
#'
#' @return a character vector of equal length, with md5 hashes of each string in original vector
#' @export
#'
#' @examples
#' md5("password");
#'   # the encryption is one-way, yet a dictionary can "hack"
#'   # See <https://crackstation.net/> for an example Rainbow Table
#'   # The best modern tool I have found is SJCL <https://github.com/bitwiseshiftleft/sjcl>
#' md5("password", times=9); # this will make the Rainbow Table work
#'   # if you know the original value and the times=n offset, you could build an internal Rainbow Table
#'
#' md5("The quick brown fox jumps over the lazy dog");
#' md5( unlist( base::strsplit("The quick brown fox jumps over the lazy dog", " ", fixed=TRUE) ) );
#'
#' md5("monte.shaffer@gmail.com", 9);
md5 = function(strvec, times=1)
  {
  # https://cran.r-project.org/web/packages/openssl/openssl.pdf
  # md5(serialize)?
  # digest is not vector driven ... # `apply` could work ...
  nstrvec = openssl::md5(strvec);
  if(times > 1)
      {
      for(j in 2:times)
        {
        nstrvec = openssl::md5(nstrvec);
        }
      }
  nstrvec;
  }





