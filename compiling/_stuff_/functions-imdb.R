

#' loadDataIMDB
#'
#' This reads in 23 compressed files that are about 56MB.
#' It will use a bit of RAM to load these data.
#'
#' Short notation for `imdb.data$actors$all`.
#'
#' `$actors$all`         # 71341
#' `$actors$popular50`   # 35245
#' `$actors$gem50`       # 37997
#' `$actors$headliners`  #  2407
#' `$actors$top250`      #  3092
#' `$actors$nm5000`      #  4968
#' `$actors$tt5000`      #  5452
#'
#'
#' `$movies$all`         # 74520
#' `$movies$popular50`   #  5704    3
#' `$movies$gem50`       #  5704    3
#' `$movies$top250`      #   250
#' `$movies$tt5000`      #  4942
#'
#'
#' `$headliners$companies`   #     770
#' `$headliners$directors`   #     606
#' `$headliners$writers`     #     670
#' `$headliners$actors`      #    2407
#' `$glue$movies.creatives`  #  361077      3   # this table was used to build headliners with 15+ rule
#'
#'
#' `$all.movies.creatives`           #  407481      4
#' `$all.movies.companies`           #  120376      4
#' `$all.movies.actors.characters`   #  901324      4
#'
#' <https://www.imdb.com/filmosearch/?sort=num_votes&explore=title_type&role=nm0000071>
#'
#' `$all.actors.rank`     #  1842305       3   # rank is order of search result
#' `$all.actors.movies`   #   282062      11   # this contains the data from search
#' `$all.actors.info`     #    71332      10   # this contains basic actor info from <https://www.imdb.com/name/nm0000071/>
#'
#'
#'
#' This data was pulled from \url{https://www.imdb.com} in September 2020.
#'
#' It contains several dataframes.
#' See \url{http://md5.mshaffer.com/WSU_STATS419/01_imdb-monte.html} for details.
#'
#' Roxygen or RStudio does not allow wildcard `*` in example? Bold?
#' See \url{\url{http://md5.mshaffer.com/WSU_STATS419/01_imdb-monte.html}} for examples.
#'
#' Once loaded, a global object `imdb.data` contains the lists of data.
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

    imdb$all.movies.creatives = readRDS(
    system.file("extdata", "imdb/all.movies.creatives.rds", package="humanVerseWSU") );
    imdb$all.movies.companies = readRDS(
    system.file("extdata", "imdb/all.movies.companies.rds", package="humanVerseWSU") );
    # imdb$all.movies.extra = readRDS(
    # system.file("extdata", "imdb/all.movies.extra.rds", package="humanVerseWSU") );
    imdb$all.movies.actors.characters = readRDS(
    system.file("extdata", "imdb/all.movies.actors.characters.rds", package="humanVerseWSU") );

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



#' IMDB.getMovieInfoFromActorSearch
#'
#' @param ttid Film identifier `ttid`
#' @param return.cols list of columns you want to display, by default all
#' @param imdb If you want to apply this function to a different dataframe, it's possible
#'
#' @return dataframe of results
#' @export
IMDB.getMovieInfoFromActorSearch = function(ttid, return.cols=NULL, imdb=imdb.data$all.actors.movies)
  {
  info = stats::na.omit( imdb[imdb$ttid == ttid, ] ) ;
  if(is.null(return.cols)) { info; } else { info[, return.cols]; }
  }


#' IMDB.searchMovieTitle
#'
#' @param str Search string with basic wildcard `*` operator
#' @param return.cols list of columns you want to display, by default all
#'
#' @param ignore.case Defaults to TRUE, matches "mont" and "Mont"
#' @param perl Defaults to FALSE, convert string query to PERL regex?
#' @param imdb If you want to apply this function to a different dataframe, it's possible
#'
#'
#' @return dataframe of results
#' @export
IMDB.searchMovieTitle = function(str, return.cols=NULL, ignore.case=TRUE, perl=FALSE, imdb=imdb.data$all.actors.movies)
  {
  grx = utils::glob2rx(str);  # https://stackoverflow.com/questions/5823503/
  grx.grep = grep(grx,imdb$title, ignore.case=ignore.case, perl=perl);
  rows = imdb[grx.grep, ];
  if(is.null(return.cols)) { rows; } else { rows[, return.cols]; }
  }


#' IMDB.getPersonInfo
#'
#' @param nmid  Person identifier `nmid`
#' @param return.cols list of columns you want to display, by default all
#' @param imdb If you want to apply this function to a different dataframe, it's possible
#'
#' # dim(imdb.data$all.actors.info);  # 71332    10
#'
#' @return dataframe of results
#' @export
IMDB.getPersonInfo = function(nmid, return.cols=NULL, imdb=imdb.data$all.actors.info)
  {
  info = imdb[imdb$nmid == nmid, ];
  if(is.null(return.cols)) { info; } else { info[, return.cols]; }
  }



#' IMDB.getMoviesForPerson
#'
#' @param nmid  Person identifier `nmid`
#' @param return.cols list of columns you want to display, by default all
#' @param imdb If you want to apply this function to a different dataframe, it's possible
#'
#' dim(imdb.data$all.actors.info);     # 71332      10
#' dim(imdb.data$all.actors.movies);   # 282062     11
#' # glue table ::  nmid|ttid|rank ... rank from ActorSearchMovies
#' dim(imdb$all.actors.rank);          # 1842305     3
#'
#' @return dataframe of results
#' @export
IMDB.getMoviesForPerson = function(nmid, return.cols=NULL, imdb=imdb.data)
  {
  # $all.actors.movies
  # $$all.actors.rank
  # inner join with rank / character names ??
  # movies info, just a few cols ...
  info = imdb$all.actors.rank[imdb$all.actors.rank$nmid == nmid, ];

  # character name here as well?  From a different dataset ...
  # https://www.datasciencemadesimple.com/join-in-r-merge-in-r/
  info.more = merge(info, imdb$all.actors.movies, by="ttid");

  #characters = imdb$all.movies.actors.characters[
   #                 imdb$all.movies.actors.characters$nmid == nmid,];

  #info.even.more = merge(info.more, imdb$all.movies.actors.characters, by="nmid");

  info.more = sortDataFrameByNumericColumns(info.more,"rank", "ASC");

  if(is.null(return.cols)) { info.more; } else { info.more[, return.cols]; }
  }

#' IMDB.searchPersonName
#'
#' @param str Search string with basic wildcard `*` operator
#' @param return.cols list of columns you want to display, by default all
#'
#' @param ignore.case Defaults to TRUE, matches "mont" and "Mont"
#' @param perl Defaults to FALSE, convert string query to PERL regex?
#' @param imdb If you want to apply this function to a different dataframe, it's possible
#'
#'
#' @return dataframe of results
#' @export
IMDB.searchPersonName = function(str, return.cols=NULL, ignore.case=TRUE, perl=FALSE, imdb=imdb.data$all.actors.info)
  {
  grx = utils::glob2rx(str);  # https://stackoverflow.com/questions/5823503/
  grx.grep = grep(grx,imdb$name, ignore.case=ignore.case, perl=perl);
  rows = imdb[grx.grep, ];
  if(is.null(return.cols)) { rows; } else { rows[, return.cols]; }
  }


#' IMDB.genericSearch
#'
#' @param str Search string with basic wildcard `*` operator
#' @param col.name The column to perform search on
#' @param return.cols list of columns you want to display, by default all
#'
#' @param ignore.case Defaults to TRUE, matches "mont" and "Mont"
#' @param perl Defaults to FALSE, convert string query to PERL regex?
#' @param imdb If you want to apply this function to a different dataframe, it's possible

#'
#' @return dataframe of results
#' @export
IMDB.genericSearch = function(str, col.name, return.cols=NULL, ignore.case=TRUE, perl=FALSE, imdb=imdb.data$all.actors.info)
  {
  grx = utils::glob2rx(str);  # https://stackoverflow.com/questions/5823503/
  grx.grep = grep(grx,imdb[col.name][[1]], ignore.case=ignore.case, perl=perl);
  rows = imdb[grx.grep, ];
  if(is.null(return.cols)) { rows; } else { rows[, return.cols]; }
  }



#' IMDB.getUniqueCharactersForPerson
#'
#' This is currently a function of `$all.movies.actors.characters`
#'  and *NOT* `$all.actors.info`
#'
#' @param nmid  Person identifier `nmid`
#' @param imdb If you want to apply this function to a different dataframe, it's possible
#'
#'
#' @return dataframe of results
#' @export
IMDB.getUniqueCharactersForPerson = function(nmid, imdb=imdb.data$all.movies.actors.characters )
  {
  rows = imdb[imdb$nmid==nmid, ];
  characters = as.data.frame( table( stats::na.omit( rows$character ) ) )[,c(2,1)];
      colnames(characters) = c("count","character");
  sortDataFrameByNumericColumns(characters,"count");
  }


