

#' loadDataIMDB
#'
#' This data was pulled from \url{https://www.imdb.com} in September 2020.
#'
#' It contains several dataframes.
#' See \url{http://md5.mshaffer.com/WSU_STATS419/01_imdb-monte.html"} for details.
#'
#' @family IMDB
#'
#' @export

loadDataIMDB = function()
  {
  imdb = list();
    actors = list();
  actors$all = readRDS(
    system.file("extdata", "imdb/all.actors.rds", package="humanVerseWSU") );
  actors$popular50 = readRDS(
    system.file("extdata", "imdb/actors.byyear.popular50.rds", package="humanVerseWSU") );
  actors$gem50 = readRDS(
    system.file("extdata", "imdb/actors.byyear.gem50.rds", package="humanVerseWSU") );
  actors$headliners = readRDS(
    system.file("extdata", "imdb/actors.headliners.rds", package="humanVerseWSU") );
  actors$top250 = readRDS(
    system.file("extdata", "imdb/actors.top250.rds", package="humanVerseWSU") );
  actors$nm5000 = readRDS(
    system.file("extdata", "imdb/actors.nm5000.rds", package="humanVerseWSU") );
  actors$tt5000 = readRDS(
    system.file("extdata", "imdb/actors.tt5000.rds", package="humanVerseWSU") );

    movies = list();
  movies$all = readRDS(
    system.file("extdata", "imdb/all.movies.rds", package="humanVerseWSU") );
  movies$popular50 = readRDS(
    system.file("extdata", "imdb/movies.byyear.popular50.rds", package="humanVerseWSU") );
  movies$gem50 = readRDS(
    system.file("extdata", "imdb/movies.byyear.gem50.rds", package="humanVerseWSU") );
  movies$top250 = readRDS(
    system.file("extdata", "imdb/movies.top250.rds", package="humanVerseWSU") );
  movies$tt5000 = readRDS(
    system.file("extdata", "imdb/movies.tt5000.rds", package="humanVerseWSU") );


  headliners = list();
  # headliner is a top actor, director, writer, or company
  # to be in this list, the said person/institution must have been
  # a headliner on at least 15 movies.
    headliners$companies = readRDS(
      system.file("extdata", "imdb/headliners.companies.rds", package="humanVerseWSU") );
    headliners$directors = readRDS(
      system.file("extdata", "imdb/headliners.directors.rds", package="humanVerseWSU") );
    headliners$writers = readRDS(
      system.file("extdata", "imdb/headliners.writers.rds", package="humanVerseWSU") );
    headliners$actors = actors$headliners;

  glue = list();
    # this was original seed to determine `headliners` above
    # from this seed, those additional pages were collected in
    # all.actors and all.movies
    glue$movies.creatives = readRDS(
    system.file("extdata", "imdb/glue.movies.headliners.rds", package="humanVerseWSU") );

    # imdb$all.movies.creatives = readRDS(
    # system.file("extdata", "imdb/all.movies.creatives.rds", package="humanVerseWSU") );
    # imdb$all.movies.companies = readRDS(
    # system.file("extdata", "imdb/all.movies.companies.rds", package="humanVerseWSU") );
    # imdb$all.movies.extra = readRDS(
    # system.file("extdata", "imdb/all.movies.extra.rds", package="humanVerseWSU") );
    # imdb$all.movies.actors.characters = readRDS(
    # system.file("extdata", "imdb/all.movies.actors.characters.rds", package="humanVerseWSU") );

    imdb$all.actors.rank = readRDS(
    system.file("extdata", "imdb/all.actors.rank.rds", package="humanVerseWSU") );
    imdb$all.actors.movies = readRDS(
    system.file("extdata", "imdb/all.actors.movies.rds", package="humanVerseWSU") );
    imdb$all.actors.info = readRDS(
    system.file("extdata", "imdb/all.actors.info.rds", package="humanVerseWSU") );



    imdb$moviecount.byyear = readRDS(
    system.file("extdata", "imdb/moviecount.byyear.rds", package="humanVerseWSU") );


    imdb$actors = actors;
    imdb$glue = glue;
    imdb$headliners = headliners;
    imdb$movies = movies;

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

# IMDB.getPersonInfo("nm0000005");
# IMDB.getPersonInfo("nm0000005", 1:4);
IMDB.getPersonInfo = function(nmid, return.cols=NULL, imdb=imdb.data$all.actors.info)
  {
  info = imdb[imdb$nmid == nmid, ];
  if(is.null(return.cols)) { info; } else { info[, return.cols]; }
  }

# IMDB.getNameOfPerson("nm0000005");
IMDB.getNameOfPerson = function(nmid)
  {
  row = IMDB.getPersonInfo(nmid);
  row$name;
  }

# IMDB.getMoviesForPerson("nm0000005", 1:5);
IMDB.getMoviesForPerson = function(nmid, return.cols=NULL, imdb=imdb.data)
  {
  # $all.actors.movies
  # $$all.actors.rank
  # inner join with rank / character names ??
  # movies info, just a few cols ...
  info = imdb$all.actors.rank[imdb$all.actors.rank$nmid == nmid, ];

  # character name here as well?
  # https://www.datasciencemadesimple.com/join-in-r-merge-in-r/
  info.more = merge(info, imdb$all.actors.movies, by="ttid");

  if(is.null(return.cols)) { info.more; } else { info.more[, return.cols]; }
  }


# IMDB.searchPersonName("Den*Wash*", return.cols=1:4);
# IMDB.searchPersonName("Robin*");
# IMDB.searchPersonName("robin*");
# IMDB.searchPersonName("robin*", ignore.case=FALSE);
# IMDB.searchPersonName("*obin*");
# IMDB.searchPersonName("*st*");
# IMDB.searchPersonName("Sean*");
# IMDB.searchPersonName("Sean*", return.cols=1:5);
# table(IMDB.searchPersonName("Fran*O*", imdb=imdb.data$top250.actors.movies, ignore.case=FALSE)$name);
# IMDB.searchPersonName("Frank Oz");

IMDB.searchPersonName = function(str, ignore.case=TRUE, perl=FALSE, return.cols=NULL,  imdb=imdb.data$all.actors.info)
  {
  grx = utils::glob2rx(str);  # https://stackoverflow.com/questions/5823503/
  grx.grep = grep(grx,imdb$name, ignore.case=ignore.case, perl=perl);
  rows = imdb[grx.grep, ];
  if(is.null(return.cols)) { rows; } else { rows[, return.cols]; }
  }

# IMDB.genericSearch("*Kentucky*", "bio");
# IMDB.genericSearch("*Kentucky*", "bio", return.cols=1:5);
# IMDB.genericSearch("*Kentucky*", "born.where", return.cols=1:5);
# IMDB.genericSearch("*Monte*", "name", return.cols=1:5);
# IMDB.genericSearch("*Monte*", "bio", return.cols=1:5);
# IMDB.genericSearch("*Montana*", "bio", return.cols=1:5);
# IMDB.genericSearch("*Montana*", "born.where", return.cols=c(1:2,4,5) );
# IMDB.genericSearch("*Seattle*", "born.where", return.cols=c(1:2,4,5) );
IMDB.genericSearch = function(str, col.name, ignore.case=TRUE, perl=FALSE, return.cols=NULL,  imdb=imdb.data$all.actors.info)
  {
  grx = utils::glob2rx(str);  # https://stackoverflow.com/questions/5823503/
  grx.grep = grep(grx,imdb[col.name][[1]], ignore.case=ignore.case, perl=perl);
  rows = imdb[grx.grep, ];
  if(is.null(return.cols)) { rows; } else { rows[, return.cols]; }

  }


# IMDB.getUniqueCharactersForPerson("nm0000136"); # Johnny Depp
# IMDB.getUniqueCharactersForPerson(IMDB.searchPersonName("Frank Oz")$nmid[1]);

IMDB.getUniqueCharactersForPerson = function(nmid, imdb=imdb.data$top250.actors.movies )
  {
  rows = imdb[imdb$nmid==nmid, ];
  na.omit( unique(rows$character) );
  }


