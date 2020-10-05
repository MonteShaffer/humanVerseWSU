

#' loadDataIMDB
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

IMDB.getMovieInfo = function(ttid, imdb=imdb.data)
  {

  }


IMDB.searchMovieName = function(str, imdb=imdb.data)
  {


  }

IMDB.getNameOfMovie = function(ttid, imdb=imdb.data)
  {

  }

IMDB.getPersonInfo = function(nmid, imdb=imdb.data$top250.actors.info)
  {
  row = imdb[imdb$nmid == nmid, ];
  row;
  }

IMDB.getNameOfPerson = function(nmid)
  {
  row = IMDB.getPersonInfo(nmid);
  row$name;
  }

IMDB.getMoviesForPerson = function(nmid, return.full=FALSE, imdb=imdb.data)
  {
  # movies info, just a few cols ...
  if(return.full){ rows; } else { rows[, 1:4]; }
  }


# IMDB.searchPersonName("Den*Wash*");
# IMDB.searchPersonName("Robin*");
# IMDB.searchPersonName("robin*");
# IMDB.searchPersonName("robin*", ignore.case=FALSE);
# IMDB.searchPersonName("*obin*");
# IMDB.searchPersonName("*st*");
# IMDB.searchPersonName("Sean*");
# IMDB.searchPersonName("Sean*", return.full=1:5);
# table(IMDB.searchPersonName("Fran*O*", imdb=imdb.data$top250.actors.movies, ignore.case=FALSE)$name);
# IMDB.searchPersonName("Frank Oz");

IMDB.searchPersonName = function(str, ignore.case=TRUE, perl=FALSE, return.full=NULL,  imdb=imdb.data$top250.actors.info)
  {
  grx = utils::glob2rx(str);  # https://stackoverflow.com/questions/5823503/
  grx.grep = grep(grx,imdb$name, ignore.case=ignore.case, perl=perl);
  rows = imdb[grx.grep, ];
  if(is.null(return.full)) { rows; } else { rows[, return.full]; }
  }

# IMDB.genericSearch("*Kentucky*", "bio");
# IMDB.genericSearch("*Kentucky*", "bio", return.full=1:5);
# IMDB.genericSearch("*Kentucky*", "born.where", return.full=1:5);
# IMDB.genericSearch("*Monte*", "name", return.full=1:5);
# IMDB.genericSearch("*Monte*", "bio", return.full=1:5);
# IMDB.genericSearch("*Montana*", "bio", return.full=1:5);
# IMDB.genericSearch("*Montana*", "born.where", return.full=1:5);
IMDB.genericSearch = function(str, col.name, ignore.case=TRUE, perl=FALSE, return.full=NULL,  imdb=imdb.data$top250.actors.info)
  {
  grx = utils::glob2rx(str);  # https://stackoverflow.com/questions/5823503/
  grx.grep = grep(grx,imdb[col.name][[1]], ignore.case=ignore.case, perl=perl);
  rows = imdb[grx.grep, ];
  if(is.null(return.full)) { rows; } else { rows[, return.full]; }

  }

# IMDB.getUniqueNamesForPerson("nm0000136"); # Johnny Depp
# IMDB.getUniqueNamesForPerson(IMDB.searchPersonName("Frank Oz")$nmid[1]);

# IMDB.getUniqueNamesForPerson("nm0172172");

IMDB.getUniqueNamesForPerson = function(nmid, imdb=imdb.data$top250.actors.movies)
  {
  rows = imdb[imdb$nmid==nmid, ];
  na.omit( unique(rows$name) );
  }

# IMDB.getUniqueCharactersForPerson("nm0000136"); # Johnny Depp
# IMDB.getUniqueCharactersForPerson(IMDB.searchPersonName("Frank Oz")$nmid[1]);

IMDB.getUniqueCharactersForPerson = function(nmid, imdb=imdb.data$top250.actors.movies )
  {
  rows = imdb[imdb$nmid==nmid, ];
  na.omit( unique(rows$character) );
  }


