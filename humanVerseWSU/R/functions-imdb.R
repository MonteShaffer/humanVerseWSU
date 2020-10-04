

#' loadInflationData
#'
#' This data was pulled from \url{https://www.imdb.com} in September 2020.
#'
#' It contains several dataframes.
#' See \url{http://md5.mshaffer.com/WSU_STATS419/imdb.html"} for details.
#'
#' @family IMDB
#'
#' @export

loadDataIMDB = function()
  {
  imdb = list();
  imdb$top250.seed.movies = readRDS(
    system.file("extdata", "imdb/top250.seed.movies.rds", package="humanVerseWSU") );
  imdb$top250.seed.actors = readRDS(
    system.file("extdata", "imdb/top250.seed.actors.rds", package="humanVerseWSU") );
  imdb$top250.actors.info = readRDS(
    system.file("extdata", "imdb/top250.actors.info.rds", package="humanVerseWSU") );
  imdb$top250.actors.movies = readRDS(
    system.file("extdata", "imdb/top250.actors.movies.rds", package="humanVerseWSU") );


  .GlobalEnv$imdb.data = imdb;
  }



getNameOfMovie = function(ttid, imdb=imdb.data)
  {

  }

getNameOfPerson = function(nmid, imdb=imdb.data)
  {

  }


getUniqueNamesForPerson = function(nmid, imdb=imdb.data)
  {

  }

getUniqueCharactersForPerson = function(nmid, imdb=imdb.data)
  {

  }


