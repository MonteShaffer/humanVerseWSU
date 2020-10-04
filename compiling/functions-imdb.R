




# https://www.imdb.com/search/title/?year=1973&title_type=feature&

getUrlTemplatesIMDB = function()
  {
  result = list();
    result$top250 = "https://www.imdb.com/chart/top";
    result$actorMovieList = "https://www.imdb.com/filmosearch/?explore=title_type&role={nmid}&ref_=filmo_ref_typ&sort=num_votes,desc&mode=detail&page={page}&title_type=movie";
    result$filmInfo = "https://www.imdb.com/title/{ttid}/";
    result$actorInfo = "https://www.imdb.com/name/{nmid}/";
    result$moviesInYear = "https://www.imdb.com/search/title/?title_type=feature&year={date-start},{date-stop}&start={start}&sort={sort}"
  result;
  }

# popular, gems, "duds" ... sort other direction on gems ...


## local.data.path = "R:/WSU_STATS419_FALL2020/";

grabDataForTop250 = function()
  {
  if(!exists("local.data.path")) # is this available from the global scope?
		{
    stop("This requires a local.data.path ... exiting");
    }

  	mypath = paste0(local.data.path,"modules");
		mypath = paste0(mypath,"/imdb");
    # let's update every month ...
		#mycache = format(Sys.Date(), "%Y-%m");
		mycache = "2020-09";  # hard-coded for now
    mypath = paste0(mypath,"/",mycache);
			  createDirRecursive(mypath);

  	mypath.dataframes = paste0(mypath,"/dataframes");
      createDirRecursive(mypath.dataframes);

    mypath.250 = paste0(mypath,"/top250");
      createDirRecursive(mypath.250);

  	mypath.ttid = paste0(mypath,"/movies");
    createDirRecursive(mypath.ttid);

  		mypath.nmid = paste0(mypath,"/actors");
      createDirRecursive(mypath.nmid);

  	mypath.search = paste0(mypath,"/search");
    createDirRecursive(mypath.search);


#######################################
  imdb.urls = getUrlTemplatesIMDB();
    myhtml.250 = paste0(mypath.250,"/top250.html");

  # seed will be top 250 movies
  rawHTML = grabHTML(myhtml.250,imdb.urls$top250,FALSE);
  imdb.250 = parseTop250(myhtml.250);
  	# final dataframe for this object
    movies.sourced.by.top250 = as.data.frame(imdb.250);

  	file.top250 = paste0(mypath.dataframes,"/movies.sourced.by.top250.txt");
  	utils::write.table(movies.sourced.by.top250, file=file.top250, quote=FALSE, col.names=TRUE, row.names=FALSE, sep="|");



  ni = length(imdb.250);







  # get top-15 actors per movie
  actors = NULL;
  for(i in 1:ni)
    {
    ttid = imdb.250[i];
      mypath.ttid2 = paste0(mypath.ttid,"/",ttid);
      createDir(mypath.ttid2);
    myhtml.ttid = paste0(mypath.ttid2,"/","filmInfo.html");
    myurl.ttid = gsub("{ttid}",ttid, imdb.urls$filmInfo, fixed=T);
    print(i); flush.console();
    do.nothing = grabHTML(myhtml.ttid,myurl.ttid,FALSE);  # slow ...

    new.actors = grabbingActorsFromFilm(myhtml.ttid);
    actors = c(actors, new.actors);
    }





############################################################
	actors = na.omit( unique(actors) );
  	
  	
df.actors.top250 = as.data.frame(actors);  	
  	file.actors.top250 = paste0(mypath.dataframes,"/actors.top250.txt");
  	utils::write.table(df.actors.top250, file=file.actors.top250, quote=FALSE, col.names=TRUE, row.names=FALSE, sep="|");

  	
  	
  	
  na = length(actors);   # actors = unique(actors);
  na;

  # get all actor info pages
  #actors = sample(actors);
	  # final dataframe for this object
    actors.info = NULL;
		# I don't have nmid column ....
  for(i in 1:na)
    {
    #nmid = "nm0000243";  # nm0000203
    #nmid = "nm0328640"; # test cases

    nmid = actors[i];
    if(isNMIDisGood(nmid))
      {

      mypath.nmid2 = paste0(mypath.nmid,"/",nmid);
      createDir(mypath.nmid2);


      myhtml.nmid = paste0(mypath.nmid2,"/","actorInfo.html");
      myurl.nmid = gsub("{nmid}", nmid, imdb.urls$actorInfo, fixed=TRUE);
      print(paste(i," of ",na)); flush.console();
      do.nothing = grabHTML(myhtml.nmid,myurl.nmid, FALSE);  # slow ...

      print( paste( paste0("      ", "nmid='",nmid,"';"), paste0("raw='",myhtml.nmid,"';"),"      ",myurl.nmid, sep="      "));
      myactor = parseActorInfo(nmid,myhtml.nmid);  # parse the info
      
      
      actors.info = rbind(actors.info, myactor);
      }
    }

  	
  	
  	
  	
  ## actors.info$source.top250 = TRUE;
  ## actors.info$source.top50 = FALSE;   # top 50 per year, since time .... unique()


    #file.actor.top250 = paste0(mypath.dataframes,"/actors.sourced.by.top250.txt");
    file.actor.top250 = paste0(mypath.dataframes,"/actors.info.by.top250.txt");
    utils::write.table(actors.info, file=file.actor.top250, quote=FALSE, col.names=TRUE, row.names=FALSE, sep="|");

    # we now have 
    
    
    # from loadDataIMDB();
    my.unique.movies = unique( imdb.data$top250.actors.movies$ttid );
    
    mumc = length(my.unique.movies);
    ## monte adds 10/3/2020 ...
    more.actors = NULL;
    
    
    for(i in 1:mumc)
      {
      ttid = my.unique.movies[i];
      percent = sprintf("%.2f",100*i/mumc);
      print(paste0(percent,"%         :: ",i," of ",mumc," ---> ",ttid, " :: ", length(more.actors))); flush.console();
    
      mypath.ttid2 = paste0(mypath.ttid,"/",ttid);
      createDir(mypath.ttid2);
      myhtml.ttid = paste0(mypath.ttid2,"/","filmInfo.html");
      myurl.ttid = gsub("{ttid}",ttid, imdb.urls$filmInfo, fixed=T);
      print(i); flush.console();
      do.nothing = grabHTML(myhtml.ttid,myurl.ttid,FALSE);  # slow ...
  
      new.actors = na.omit( grabbingActorsFromFilm(myhtml.ttid) );
      
      
      more.actors = c(more.actors, new.actors);
      more.actors = unique(more.actors);
      
      }
    
    
    ####### 
    more.actors = unique(more.actors);
    
    length(more.actors);
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  ## save again, once we get the top50 indicator ...



  #grabTopMovies(1898,2020);

  	top.list = grabTopMovies(1898,2020);  # I grab count, but rely on "popular" record list #
	    # this is issue from 1898 to 1908 ... "of" doesn't exist ..


    top.list.movies = sortDataFrameByNumericColumns(top.list$movies,"year","ASC");
   file.movies.top50 = paste0(mypath.dataframes,"/movies.sourced.by.top50.txt");
    utils::write.table(top.list.movies, file=file.movies.top50, quote=FALSE, col.names=TRUE, row.names=FALSE, sep="|");




  top.list.mycounts = as.data.frame(top.list$counts);
  top.list.mycounts = na.omit(top.list.mycounts);

  # we are dropping 1908 and earlier ... not reading for some reason, not end of the world not to have that little data
  # remaining counts are equal, so
  ### top.list.mycounts = removeColumnsFromDataFrame(top.list.mycounts,"count.gem");
  top.list.mycounts = sortDataFrameByNumericColumns(top.list.mycounts,"year","ASC");

  # colnames(top.list.mycounts) = c("year","count");

  file.moviecount.byyear = paste0(mypath.dataframes,"/moviecount.byyear.txt");
  utils::write.table(top.list.mycounts, file=file.moviecount.byyear, quote=FALSE, col.names=TRUE, row.names=FALSE, sep="|");


  # let's do the actor lookup stuff again?  I would have to grab the movies ???

  # movies.sourced.by.year.top50
  # movies.sourced.by.top250










  actors.movies.gluetable = NULL;



	# could shuffle(actors) and run multiple instances ...
	actors = sample(actors);
  #
  first.movies.df = TRUE;
  file.mymovies.df.top250 = paste0(mypath.dataframes,"/actors.movies.top250.txt");
  file.mymovies.info.top250 = paste0(mypath.dataframes,"/movies.info.top250.txt");

	# grab all movies for these 3000 or so actors  ... 3573 -> 3092
  movies = NULL;  # running vector of total movies ... 10,000
	for(i in 1:na)
    {
    nmid = actors[i];
    if(isNMIDisGood(nmid))
      {
      if(!is.na(nmid))
      {
      print("####################################");
      print(paste(i," of ",na, " => ",nmid)); flush.console();
      print("####################################");
      mymovies = doActorSearch( nmid, mypath.search );
      mymovies$nmid = nmid;
        mymovies.actors = removeAllColumnsBut(mymovies,c("rank","ttid","nmid"));
        mymovies.actors = moveColumnsInDataFrame(mymovies.actors,"nmid","before","rank");

        mymovies.info = removeColumnsFromDataFrame(mymovies,c("rank","nmid"));
        mymovies.info = moveColumnsInDataFrame(mymovies.info,"ttid","before","title");
        mymovies.info = moveColumnsInDataFrame(mymovies.info,c("title","genre","paragraph"),"after","millions");


      if(first.movies.df)
        {
        utils::write.table(mymovies.actors, file=file.mymovies.df.top250, append=FALSE, quote=FALSE, col.names=TRUE, row.names=FALSE, sep="|");
         utils::write.table(mymovies.info, file=file.mymovies.info.top250, append=FALSE, quote=FALSE, col.names=TRUE, row.names=FALSE, sep="|");
        first.movies.df = FALSE;
        } else {
         utils::write.table(mymovies.actors, file=file.mymovies.df.top250, append=TRUE, quote=FALSE, col.names=FALSE, row.names=FALSE, sep="|");
       utils::write.table(mymovies.info, file=file.mymovies.info.top250, append=TRUE, quote=FALSE, col.names=FALSE, row.names=FALSE, sep="|");

           }

        myttids = as.vector(unlist(mymovies$ttid));
      for(j in 1: length(myttids))
        {
        actors.movies.gluetable = rbind(actors.movies.gluetable, c(nmid,myttids[j]) );
        }
      movies = c(movies,mymovies$ttid);
      }
      }
	  }
	# length of movies:  120,909
	# length of unique movies: 64,273
	movies = unique(movies);
  length(movies);
	## store glue table








	colnames(actors.movies.gluetable) = c("nmid","ttid");
	gluetable.df = as.data.frame(actors.movies.gluetable);

  file.gluetable.top250 = paste0(mypath.dataframes,"/actors.movies.gluetable.top250.txt");
  utils::write.table( gluetable.df , file=file.gluetable.top250, quote=FALSE, col.names=TRUE, row.names=FALSE, sep="|");

###################  monte you are here !

	###  from top.50, grab all actors ... sounds like a function ... this one function is like 8 functions
#	actors.info$source.top50 = FALSE;


	# get actors per movie in top50 list

	imdb.50 = unlist(top.list.movies$ttid);
	nt50 = length(imdb.50);

# twice as big now 21150 ... with crappy movies ... unpopular & duds

	imdb.50 = sample(imdb.50);



  actors.top50 = NULL;














nt50 = length(imdb.50);
mydata = imdb.50;

### let's grab info from 64, 273 movies about creatives ...
nt50 = length(movies);
mydata = movies;

	file.temp.movies = paste0(mypath.dataframes,"/temp.movies.txt");
  utils::write.table( movies , file=file.temp.movies, quote=FALSE, col.names=TRUE, row.names=FALSE, sep="|");



mydata = sample(mydata);






	movie.nmids = c();
	  movies.nmids.file = paste0(mypath.dataframes,"/movies.creatives.gluetable.stars.txt");
	  writeLine("ttid|nmid|role", file=movies.nmids.file, append=FALSE);
	movie.coids = c();
	  movie.coids.file = paste0(mypath.dataframes,"/movies.companies.gluetable.stars.txt");
	  writeLine("ttid|coid|name", file=movie.coids.file, append=FALSE);

	movie.actors.characters.file = paste0(mypath.dataframes,"/movies.actors.characters.txt");
	  writeLine("ttid|nmid|name|character", file=movie.actors.characters.file, append=FALSE);





  for(i in 28750    :nt50)
    {
    ttid = mydata[i];
      mypath.ttid2 = paste0(mypath.ttid,"/",ttid);
      createDir(mypath.ttid2);
    myhtml.ttid = paste0(mypath.ttid2,"/","filmInfo.html");
    myurl.ttid = gsub("{ttid}",ttid, imdb.urls$filmInfo, fixed=T);
    percent = sprintf("%.2f",100*i/nt50);
    print(paste0(percent,"%         :: ",i," of ",nt50," ---> ",ttid)); flush.console();
    do.nothing = grabHTML(myhtml.ttid,myurl.ttid,FALSE);  # slow ...

    info = grabbingProductionFromFilm(myhtml.ttid);

      my.nmids = na.omit(unlist(info$persons$nmid));     n.nmids = length(my.nmids);
        my.nmids.roles = na.omit(unlist(info$persons$role));
      my.coids = na.omit(unlist(info$extra$co));         n.coids = length(my.coids);
        my.coids.names = na.omit(unlist(info$extra$cona));

      for(jjj in 1:n.nmids)
        {
        if(n.nmids == 0) { writeLine( paste0(ttid,"|","NO-INFO","|","NO-INFO"), file=movies.nmids.file, append=TRUE);
                          } else {
                                   my.nmid = my.nmids[jjj];
                                    if(my.nmid !="a>")
                                      {
                                      my.nmrole = my.nmids.roles[jjj];
                                      if(my.nmrole == "directors") { my.nmrole = "director"; }
                                      if(my.nmrole == "writers") { my.nmrole = "writer"; }
                                      if(my.nmrole == "stars") { my.nmrole = "star"; }
                                      #if(my.nmrole != "star")
                                        #{
                                        # we already have stars ... elsewhere???
                                        writeLine( paste0(ttid,"|",my.nmid,"|",my.nmrole), file=movies.nmids.file, append=TRUE);
                                        #}
                                      }
                          }


        }


      for(kkk in 1:n.coids)
        {
        if(n.coids == 0) { writeLine( paste0(ttid,"|","NO-INFO","|","NO-INFO"), file=movie.coids.file, append=TRUE);
                          } else {
                                  my.coid = my.coids[kkk];
                                  if(my.coid !="a>")
                                    {
                                    writeLine( paste0(ttid,"|",my.coid,"|",my.coids.names[kkk]), file=movie.coids.file, append=TRUE);
                                    }
                                  }
        }


    movie.nmids = c(movie.nmids, my.nmids );
    movie.coids = c(movie.coids, my.coids );

    new.actors.top50 = grabbingActorsFromFilm(myhtml.ttid);
    actors.played.file = gsub("filmInfo.html", "actors-played.txt", myhtml.ttid, fixed=TRUE);
    if(file.exists(actors.played.file))
      {
      actors.played.df = utils::read.csv(actors.played.file, header=TRUE, quote="", sep="|");
      actors.played.n = dim(actors.played.df)[1];

      for(apn in 1:actors.played.n)
        {
        actors.played.row = actors.played.df[apn,];
        if(!is.na(str_trim(actors.played.row[1])))
          {
          writeLine(paste0( ttid,"|",
                          actors.played.row[1],"|",
                          actors.played.row[2],"|",
                          actors.played.row[3]),
                  file=movie.actors.characters.file, append=TRUE);
          }
        }

      }

    if(!is.null(new.actors.top50))
      {
      adding = length(new.actors.top50);
        #print(paste0(" Adding ... ", adding));
        #print(new.actors.top50);
      actors.top50 = c(actors.top50, new.actors.top50);
      }
  }



# tt7572934  ... 28654

# tt7572934  ...







#########################################################################
#########################################################################
	#########################################################################



## let's grab creative info
	nct = length(movie.nmids);  # 56794
	movie.nmids.u = unique(movie.nmids);
	nctu = length(movie.nmids.u);  # 26899
nmidsmovies.file = paste0(mypath.dataframes,"/creatives.movies.gluetable.txt");
	  writeLine("nmid|ttid", file=nmidsmovies.file, append=FALSE);

	dput(movie.nmids.u, file = paste0(mypath.dataframes,"/creatives.movies.nmids.u.txt") );

	#movie.nmids.u = sample(movie.nmids.u);

	movies.topCreative = NULL;



	for(i in 1:nctu)
    {
    nmid = movie.nmids.u[i];
    print("####################################");
    percent = sprintf("%.2f",100*i/nctu);
    print(paste0(percent,"%         :: ",i," of ",nctu," ---> ",nmid)); flush.console();

    #print(paste(i," of ",na50, " => ",nmid)); flush.console();
    print("####################################");
    mymovies = doActorSearch( nmid, mypath.search );
    myttids = as.vector(unlist(mymovies$ttid));
    for(j in 1: length(myttids))
      {
      if(length(myttids) == 0)
        {
        writeLine( paste0(nmid,"|","DANGLING"), file=nmidsmovies.file, append=TRUE);
        } else {
                myttid = myttids[j];
								writeLine( paste0(nmid,"|",myttid), file=nmidsmovies.file, append=TRUE);

                }
      }
    movies.topCreative = c(movies.topCreative,mymovies$ttid);
	  }




	length(movies.topCreative);  # 799471 ... 714115



	movies.topCreative.u = unique(movies.topCreative);
  length(movies.topCreative.u);  # 218528  ... 226376

#########################################################################
	#########################################################################
	#########################################################################





























































	##################  grabbing co-ids and directors/writers to add to the netw


	length(actors.top50);  # 119311

	actors.top50.u = unique(actors.top50);

	na50 = length(actors.top50.u);  # 64041

	temp.actors.top50.df = as.data.frame(actors.top50.u);
	  colnames(temp.actors.top50.df ) = c("actors");

	###############  monte is here ###############
	 file.temp.actors.top50 = paste0(mypath.dataframes,"/temp.actors.top50.unique.txt");
  utils::write.table( temp.actors.top50.df , file=file.temp.actors.top50, quote=FALSE, col.names=TRUE, row.names=FALSE, sep="|");



	actors.top50.u = sample(actors.top50.u);
  #movies = NULL;  # running vector of total movies ... 10,000



	movies.top50 = NULL;
	actors.movies.gluetable.top50 = c();
	for(i in 1:na50)
    {
    nmid = actors.top50.u[i];
    if(isNMIDisGood(nmid))
      {
      print("####################################");
      percent = sprintf("%.2f",100*i/na50);
      print(paste0(percent,"%         :: ",i," of ",na50," ---> ",nmid)); flush.console();

      #print(paste(i," of ",na50, " => ",nmid)); flush.console();
      print("####################################");
      mymovies = doActorSearch( nmid, mypath.search );
      myttids = as.vector(unlist(mymovies$ttid));
      for(j in 1: length(myttids))
        {
        actors.movies.gluetable.top50 = rbind(actors.movies.gluetable.top50, c(nmid,myttids[j]) );
        }
      movies.top50 = c(movies.top50,mymovies$ttid);
      }
	  }

	movies.top50.u = unique(movies.top50);


	# length(actors.movies.gluetable.top50)  # [1] 3027634
	# length(movies.top50);  # [1] 1513575
	# top 50 movies x years
	# nt50 = length(imdb.50); # 10390  movies ...
	# length(actors.top50);  # 119311
	# na50 = length(actors.top50.u);  # 64041  actors
	# total movies from these actors
	# > length(movies.top50.u); [1] 264200


	colnames(actors.movies.gluetable.top50) = c("nmid","ttid");
	gluetable.df2 = as.data.frame(actors.movies.gluetable.top50);

  file.glue.top50 = paste0(mypath.dataframes,"/actors.movies.gluetable.top50.txt");
  utils::write.table( gluetable.df2 , file=file.glue.top50, quote=FALSE, col.names=TRUE, row.names=FALSE, sep="|");



















	# top 250 movies
	# got 3,092 actors  (3,750) ... unique
	# got 64,273 movies ...  avg actor has 20 ...
	# many-to-one relationship ... network analysis ...

	# best-pictures ... academy
	# best-actor ... ... movies ...
	# https://www.imdb.com/list/ls009480135/
	# best-supporting ... actor/actress

	# ideas,
	# seed = movies from 1973  ...  2,298
	# seed = movies that had winner of (best-picture, best-actor, best-actress)
  # build a column "seed" ... indicator variable per seed ...
	# total movies by year ... dataframe ...
	# seed = 2,868 titles. 1999
	# seed top movies of year (50 by year) ... decades ...
	# top-50 for the 70s ... 50x10 = 500
	# top-50 for the 1890-2020 ... 50x10 = 500
	# top250AllTime ... alltime
	# top50ByYear ... 100x50 = 5000 movies ...  popularity ... votes ... 10,000
	# grabTopMoviesForGivenYear = function(n, year)  # total movies
  # sort=user_rating,desc  ... gems ... people took time to vote
	# sort=popular ... box office $$$ ...

  nm = length(movies);

  # store 4 dataframes:  movies.search, actors.info, movies.info, gluetable
  # not going to grab moviesInfo for 64,000 ... movies.info,
	# movies.top50.eachYear ...







	actors.info = NULL;
	actors.movies.gluetable = NULL;
	movies.sourced.by.actor = NULL;
	movies.sourced.by.year.top50 = NULL;

	# buildDataFrames ...
	  # movies.sourced.by.top250
	  # actors.info
	  # actors.movies.gluetable
	  # movies.sourced.by.actor
	  # movies.sourced.by.year.top50
	# movies.top.by.year
	# movies.top250 ... list of tt's ...
	# movies.paragraph ... tts ... paragraph ... long-text ... NLP ...
	# NLP ... Deerwester example in the NOTEBOOK ....
	# concept of glue ... https://www.informit.com/articles/article.aspx?p=27785&seqNum=3

  }

# R:/WSU_STATS419_FALL2020/modules/imdb/2020-09/search/actor/            Nut Merchant /
isNMIDisGood = function(nmid)
  {
  nmid = as.numeric( gsub("nm","",nmid,fixed=TRUE) );
  return (!is.na(nmid));
  }


# nm0268306
# ex = parseActorInfo(nmid,raw); ex;
#  myactor = parseActorInfo(myhtml.nmid);  # parse the info
# denzel = parseActorInfo("R:/WSU_STATS419_FALL2020/modules/imdb/2020-09/actors/nm0000243/actorInfo.html");
# other = parseActorInfo("R:/WSU_STATS419_FALL2020/modules/imdb/2020-09/actors/nm0328640/actorInfo.html");
parseActorInfo = function(nmid,raw)
  {
  # myurl.nmid = "https://www.imdb.com/name/nm0328640/";
  # raw = myhtml.nmid = "R:/WSU_STATS419_FALL2020/modules/imdb/2020-09/actors/nm0328640/actorInfo.html";

  # myurl.nmid = "https://www.imdb.com/name/nm0000243/";
  # raw = myhtml.nmid = "R:/WSU_STATS419_FALL2020/modules/imdb/2020-09/actors/nm0000243/actorInfo.html";



  # nmid = "nm0268306";
  # myurl.nmid = "https://www.imdb.com/name/nm0268306/";
  # raw = myhtml.nmid = "R:/WSU_STATS419_FALL2020/modules/imdb/2020-09/actors/nm0268306/actorInfo.html";

  # nmid = "nm0000243";
  # myurl.nmid = "https://www.imdb.com/name/nm0000243/";
  # raw = myhtml.nmid = "R:/WSU_STATS419_FALL2020/modules/imdb/2020-09/actors/nm0000243/actorInfo.html";


  mycache = gsub(".html",".txt",raw);

  if(!file.exists(mycache))
  #if(TRUE)
  {
  # list, not dataframe ... back to dataframe, it won't save right.

  result = data.frame( matrix(ncol = 10,nrow = 1) );
  colnames(result) = c("nmid","name", "roles", "bio",
          "born.when", "born.where",
          "died.when", "died.where",
          "starmeter.rank", "starmeter.delta");
  i = 1;


  rvest.html = xml2::read_html(raw, encoding = "ISO-8859-1");

  result$nmid[i] = nmid;
  #result = list();
  name = rvest.html %>%
            html_node(".header .itemprop") %>%
            html_text();
  result$name[i] = name;

  # 3 roles or less ...
  roles = rvest.html %>%
            html_nodes("#name-job-categories a") %>%
            html_text() %>%
            str_trim();

  # nroles = length(roles);  # rvest is not working like R with NA's
  # 
  # myroles = c("NA","NA","NA");
  # if(nroles > 0)
  #   {
  #   for(j in 1:nroles)
  #     {
  #     myroles[j] = roles;
  #     }
  #   }

  # result$role.1[i] = myroles[1];
  # result$role.2[i] = myroles[2];
  # result$role.3[i] = myroles[3];
  
  result$roles = paste(roles,collapse=",");

  bio = rvest.html %>%
            html_node(".name-trivia-bio-text") %>%
            html_text() %>%
            str_trim();

      bio = gsub("See full bio","",bio,fixed=TRUE);
      bio = gsub("Â»","",bio,fixed=TRUE);
      bio = gsub("»","",bio,fixed=TRUE);
      bio = gsub("[[:space:]]", " ", bio);
      #bio = gsub("...","",bio,fixed=TRUE);
      bio = str_trim(bio);
  result$bio[i] = bio;


#####################################################
  born.when = born.location = "NA";

  born = rvest.html %>%
            html_node("#name-born-info");

  if(!is.na(born))
    {
    born.when = born %>%
              html_node("time") %>%
              html_attr('datetime');


    born.idx = 3; if(is.na(born.when)) {born.idx = 1;}
    born.where = born %>%
              html_nodes("a");

    born.n = length(born.where);
    if(born.n < born.idx) { } else {
                                    born.location = born.where[born.idx] %>%
                                            html_text() %>%
                                            str_trim();
                                    }

    }


  result$born.when[i] = born.when;
  result$born.where[i] = born.location;

  #####################################################


  died.when = died.location = "NA";

  died = rvest.html %>%
            html_node("#name-death-info");

  if(!is.na(died))
    {
    died.when = died %>%
              html_node("time") %>%
              html_attr('datetime');


    died.idx = 3; if(is.na(died.when)) {died.idx = 1;}
    died.where = died %>%
              html_nodes("a");

    died.n = length(died.where);
    if(died.n < died.idx) { } else {
                                    died.location = died.where[died.idx] %>%
                                            html_text() %>%
                                            str_trim();
                                    }

    }

  result$died.when[i] = died.when;
  result$died.where[i] = died.location;
#####################################################

  # starmeter.rank = rvest.html %>%
  #           html_node("#meterRank") %>%
  #           html_text() %>%
  #           str_trim() %>%
  #           as.numeric();
  
  starmeter.rank = rvest.html %>%
            html_node("#meterRank") %>%
            html_text() %>%
            str_trim();
  
  starmeter.rank = str_trim ( gsub("Top","",starmeter.rank, fixed=TRUE) );
  starmeter.rank = as.numeric( starmeter.rank );
  
  
  
  result$starmeter.rank[i] = starmeter.rank;

  starmeter.delta = rvest.html %>%
            html_node("#meterChangeRow");

  starmeter.delta.direction = starmeter.delta %>%
            html_node("span") %>%
            html_text() %>%
            str_trim();

  starmeter.delta.degree = starmeter.delta %>%
            html_node("#meterChange") %>%
            html_text() %>%
            str_trim();
      starmeter.delta.degree = as.numeric( gsub(",","",starmeter.delta.degree,fixed=TRUE) );

  starmeter.delta.final = starmeter.delta.degree;
    if(starmeter.delta.direction == "Down") { starmeter.delta.final = -1 * starmeter.delta.final; }

  result$starmeter.delta[i] = starmeter.delta.final;


# result.df = as.data.frame( rbind(result,result), row.names=FALSE );
  # save(result, file=mycache);
  # write(result, file=mycache);
  #my.dput = dput(result);
  #mycache;
  #dput(result);

  #dput(result, file=mycache);
 # dump( result, file=mycache);  # Error in FUN(X[[i]], ...) : invalid first argument

  utils::write.table(result, file=mycache, quote=FALSE, col.names=TRUE, row.names=FALSE, sep="|");
  } else {
         result =  utils::read.csv(mycache, header=TRUE, quote="", sep="|");
    }


  result[1,];  # return just the row, this is how we will build the master file

  }


#' from.year = 1920
#' to.year = 2020
#' grabTopMovies(1920,2020);
#' grabTopMovies(1898,2020);
#'
#' ## probably an issue with the parser ...
#' old movies (1898) write:: table then read::csv is breaking
#'  getwd();
#[1] "R:/WSU_STATS419_FALL2020/modules/imdb/2020-09/topN/1899"
#> x = read.csv("popular.txt", header=T, sep="|");
#Warning message:
#In read.table(file = file, header = header, sep = sep, quote = quote,  :
#  incomplete final line found by readTableHeader on 'popular.txt'

grabTopMovies = function(from.year, to.year)
  {
  # will grab top-50 ...


  # sort = "user_rating,desc" ... "gems"  ... could be another function
  #sorts = c("popularity", "user_rating,desc");
  sorts = c("moviemeter,asc", "moviemeter,desc", "user_rating,desc", "user_rating,asc");
  sorts.stem = c("popular","unpopular","gems","duds");  # file system ...


  years = from.year:to.year;

  years = sample(years); # randomization to allow for parallel instances

  result = NULL;
  count = NULL;
  mycount.names = c("year","count.popular","count.unpopular","count.gem","count.dud");

  y = 0; ny = length(years);
  for(year in years)
    {
    y = 1 + y;
    print(paste0(y , " of ", ny, "  -->  ", year));

    stack.movies.popular = NULL;
    stack.count.unpopular = NULL;
    stack.count.gem = NULL;
    stack.count.dud = NULL;

    # multiple sorts ... hard code
    stack.popular = grabTopMoviesForGivenYear(year, sorts[1]);
    stack.unpopular = grabTopMoviesForGivenYear(year, sorts[2]);
    stack.gem = grabTopMoviesForGivenYear(year, sorts[3]);
    stack.dud = grabTopMoviesForGivenYear(year, sorts[4]);

      stack.popular.totalcount = stack.popular$totalcount;
        if(is.na(stack.popular.totalcount)) {stack.popular.totalcount = dim(stack.popular$movies)[1];}
      stack.unpopular.totalcount = stack.unpopular$totalcount;
        if(is.na(stack.unpopular.totalcount)) {stack.unpopular.totalcount = dim(stack.unpopular$movies)[1];}

      stack.gem.totalcount = stack.gem$totalcount;
        if(is.na(stack.gem.totalcount)) {stack.gem.totalcount = dim(stack.gem$movies)[1];}

      stack.dud.totalcount = stack.dud$totalcount;
        if(is.na(stack.dud.totalcount)) {stack.dud.totalcount = dim(stack.dud$movies)[1];}



    row = stack.popular$movies;  # rbind ... dataframe
      row$rank.popular = row$rank;
      row$rank.unpopular = 0;
      row$rank.gem = 0;
      row$rank.dud = 0;

    toadd = stack.unpopular$movies;  # equal col-length dataframe ...
      toadd$rank.popular = 0;
      toadd$rank.unpopular = toadd$rank;
      toadd$rank.gem = 0;
      toadd$rank.dud = 0;

    myrow.unpopular = updateDataFrameWithUniqueNewElementsIndicated(row, "ttid",  toadd, "rank.unpopular", "rank");
    #result = rbind(result, myrow);

    toadd = stack.gem$movies;  # equal col-length dataframe ...
      toadd$rank.popular = 0;
      toadd$rank.unpopular = 0;
      toadd$rank.gem = toadd$rank;
      toadd$rank.dud = 0;

    myrow.gem = updateDataFrameWithUniqueNewElementsIndicated(myrow.unpopular, "ttid",  toadd, "rank.gem", "rank");
    #result = rbind(result, myrow);

    toadd = stack.dud$movies;  # equal col-length dataframe ...
      toadd$rank.popular = 0;
      toadd$rank.unpopular = 0;
      toadd$rank.gem = 0;
      toadd$rank.dud = toadd$rank;

    myrow.dud = updateDataFrameWithUniqueNewElementsIndicated(myrow.gem, "ttid",  toadd, "rank.dud", "rank");

    result = rbind(result,myrow.dud); # this has all unique movies with T/F



    print(stack.popular.totalcount == stack.gem.totalcount);
    # for searches that have less than 50 results

    count = rbind(count, c(year,stack.popular$totalcount,stack.unpopular$totalcount,stack.gem$totalcount,stack.dud$totalcount) );




    }
  colnames(count) = mycount.names;

  list("movies"=result, "counts"=count);
  }

# r1 = grabTopMoviesForGivenYear(1899,"popularity");
# r2 = grabTopMoviesForGivenYear(1899,"user_rating,desc");
grabTopMoviesForGivenYear = function(year, sort="moviemeter,asc")
  {
  # sort = "user_rating,desc" ... "gems" ... could be another function
  sorts = c("moviemeter,asc", "moviemeter,desc", "user_rating,desc", "user_rating,asc");
  sorts.stem = c("popular","unpopular","gems","duds");  # file system ...
  sort.idx = findAllIndexesWithValueInVector(sorts,sort);
  sort.stem = sorts.stem[sort.idx];

  imdb.urls = getUrlTemplatesIMDB();

  # I am not caching a "n" factor, so hardcoded
  n=50;
  # returns movies(n) and totalcount for year ... n <=50
  if(n > 50) { n = 50; } # pagination not implemented ...
  start = 1;
  date.start = paste0(year,"-01-01");
  date.stop = paste0(year,"-12-31");

  url.top = imdb.urls$moviesInYear;
    url.top = gsub("{start}"      ,start,      url.top,fixed=TRUE);
    url.top = gsub("{date-start}" ,date.start, url.top,fixed=TRUE);
    url.top = gsub("{date-stop}"  ,date.stop,  url.top,fixed=TRUE);
    url.top = gsub("{sort}"       ,sort,       url.top,fixed=TRUE);

    # assume we live in local.path world from previous logic
  mypath = paste0(local.data.path,"modules");
		mypath = paste0(mypath,"/imdb");
    # let's update every month ...
		#mycache = format(Sys.Date(), "%Y-%m");
		mycache = "2020-09";  # hard-coded for now
    mypath = paste0(mypath,"/",mycache);
    mypath = paste0(mypath,"/topN/",year);
			  createDirRecursive(mypath);  # copy/paste means maybe a function?

  html.top = paste0(mypath,"/",sort.stem,".html");

  cache.top = gsub(".html",".txt",html.top, fixed=TRUE);
  count.top = gsub(".txt",".count",cache.top, fixed=FALSE);

  #if(!file.exists(cache.top))
  if(TRUE)
    {
    do.nothing = grabHTML(html.top, url.top, FALSE);  # slow ...
      r = parseSearchFromTop(html.top);

    utils::write.table(r$movies, file=cache.top, quote=FALSE, col.names=TRUE, row.names=FALSE, sep="|");
    utils::write.table(r$totalcount, file=count.top, quote=FALSE, col.names=FALSE, row.names=FALSE, sep="|");

    } else {
            movies = utils::read.csv(cache.top, header=TRUE, quote="", sep="|");
            totalcount = utils::read.csv(count.top, header=TRUE, quote="", sep="|");
            r = list("movies" = movies, "totalcount" = as.numeric(totalcount) );
            }

  r;
  }








parseSearchFromTop = function(raw)
  {

# encoding = "ISO-8859-1"
# https://stackoverflow.com/questions/45290452/encoding-error-with-read-html
  rvest.html = xml2::read_html(raw, encoding = "ISO-8859-1");

  # rvest::guess_encoding(raw);

  ########################
  totalcount = 0;
  getcount = rvest.html %>%
    html_node(".desc") %>%
    html_text();

  temp = strsplit(getcount,"of",fixed=T);
  temp2 = strsplit(temp[[1]][2],"titles", fixed=T);
  temp3 = str_trim(temp2[[1]][1]);
  temp4 = gsub(",","", temp3, fixed=TRUE);
    # if an actor has 1,000 pages, this would break other code ...

  totalcount = as.numeric(temp4);

  ########################
  movies = NULL;

  movies = rvest.html %>%
    html_nodes(".mode-advanced");

  pagecount = length(movies);
  result = data.frame( matrix(ncol = 12,nrow = pagecount) );
  colnames(result) = c("rank", "title", "paragraph", "ttid", "year", "rated", "minutes", "genre", "ratings", "metacritic", "votes", "millions");

  for(i in 1:pagecount)
  {
    movie = movies[i];

    rank = movie %>%
      html_node(".lister-item-index") %>%
      html_text() %>%
      as.numeric();
    result$rank[i] = rank;

    title = movie %>%
      html_node(".lister-item-header a") %>%
      html_text();
    result$title[i] = title;

    ttid = movie %>%
      html_node(".lister-item-header a") %>%
      html_attr("href");

    temp = strsplit(ttid,"/",fixed=T);
    ttid = temp[[1]][3];
    result$ttid[i] = ttid;

    year = movie %>%
      html_node(".lister-item-year") %>%
      html_text();
    year = cleanupYearIMDB(year);
    result$year[i] = year;

    rated = movie %>%
      html_node(".certificate") %>%
      html_text();
    result$rated[i] = rated;

    minutes = movie %>%
      html_node(".runtime") %>%
      html_text();
    minutes = cleanupMinutesIMDB(minutes);
    result$minutes[i] = minutes;

    genre = movie %>%
      html_node(".genre") %>%
      html_text();
    genre = str_trim(genre);
    result$genre[i] = genre;

    ratings = movie %>%
      html_node("div .rating-list") %>%
      html_attr("title");
    temp = strsplit(ratings,"/",fixed=T);
    temp = gsub("Users rated this","",temp[[1]][1],fixed=T);
    temp = str_trim(temp);
    ratings = as.numeric(temp);
    result$ratings[i] = ratings;

    metacritic = movie %>%
      html_node(".ratings-metascore span") %>%
      html_text();
    metacritic = as.numeric(str_trim(metacritic));
    result$metacritic[i] = metacritic;

    p = movie %>%
      html_nodes(".lister-item-content p");
    temp = strsplit(as.character(p),'class="',fixed=TRUE);
    temp2 = strsplit(temp[[2]][2],"\r\n",temp,fixed=TRUE);
    para = str_trim(gsub("</p>","",temp2[[1]][2],fixed=TRUE));

    result$paragraph[i] = para;

    info = movie %>%
      html_nodes(".lister-item-content p span") %>%
      html_text();

    # does this need to be fixed in main parser for "actor-search"?
    if(is.na(info[8]))
      {
      votes = as.numeric(gsub(",","",info[6],fixed=T));
      } else  {
              votes = as.numeric(gsub(",","",info[8],fixed=T));
              }
    result$votes[i] = votes;

    millions = cleanupMillionsIMDB(info[11]);
    result$millions[i] = millions;
  }

  list("movies"=result, "totalcount"=totalcount);
}


doActorSearch = function(nmid, mypath.search)
  {
  print(paste0("doActorSearch( ",nmid));
  path = paste0(mypath.search,"/actor/",nmid,"/");
    createDirRecursive(path);
  myurl.nmid = gsub("{nmid}",nmid, imdb.urls$actorMovieList, fixed=TRUE);

  df.out = paste0(path,"dataframe.txt");
  if(!file.exists(df.out))
  {
  page = 1;
  m.page = numberPadLeft(page,3);

  url.current = gsub("{page}",page, myurl.nmid, fixed=TRUE);
  html.current = paste0(path,"page_",m.page,".html");



  do.nothing = grabHTML(html.current, url.current);

  page1 = parseSearchForActors(html.current);
  raw = html.current; # for testing
  totalcount = page1$totalcount;

  movies = page1$movies;

  if(totalcount <= 50)
    {
    #return (movies);
    } else  {
            pages = ceiling(totalcount/50);

            for(i in 1:(pages-1) )
              {
              page = 1 + page;
              m.page = numberPadLeft(page,3);

              url.current = gsub("{page}",page, myurl.nmid, fixed=TRUE);
              html.current = paste0(path,"page_",m.page,".html");

              do.nothing = grabHTML(html.current, url.current);

              pageN = parseSearchForActors(html.current);

              movies = rbind(movies, pageN$movies);
              }
            #return (movies);
            }

    utils::write.table(movies, file=df.out, quote=FALSE, col.names=TRUE, row.names=FALSE, sep="|");

    } else {
            movies = utils::read.csv(df.out, header=TRUE, quote="", sep="|");
            }
  movies;
  }


parseSearchForActors = function(raw)
  {

# encoding = "ISO-8859-1"
# https://stackoverflow.com/questions/45290452/encoding-error-with-read-html
  rvest.html = xml2::read_html(raw, encoding = "ISO-8859-1");

  # rvest::guess_encoding(raw);

  ########################
  movies = NULL;

  movies = rvest.html %>%
    html_nodes(".mode-detail");

  pagecount = length(movies);
      # should do this first, if pagecount < 50, set=total count
      # set totalcount=pagecount and skip next section "monte was here"



  ######################## monte was here
  totalcount = 0;
  getcount = rvest.html %>%
    html_nodes(".desc") %>%
    html_text();


  # nm0328640  ... one page
  temp = strsplit(getcount,"titles",fixed=T);
    temp2 = str_trim(temp[[1]][1]);
  is.of = as.numeric( regexpr("of",temp2,fixed=TRUE) );

  if(is.of == -1)
    {
    # if no 'of'
    totalcount = as.numeric(temp2);
    } else  {
            temp = strsplit(temp2,"of",fixed=T);
            totalcount = as.numeric(temp[[1]][2]);
            }

  if(is.na(totalcount)) { totalcount = 1; }  # nm0315760

  ############################################

  result = data.frame( matrix(ncol = 12,nrow = pagecount) );
  colnames(result) = c("rank", "title", "paragraph", "ttid", "year", "rated", "minutes", "genre", "ratings", "metacritic", "votes", "millions");

  # we had an actor from a movie list, but when doing a search for movies from actor, get nothing.
  if(pagecount == 0)  # nm6925197
    {
    result[1,]=NA;

    return ( list("movies"=result, "totalcount"=0) );
    }


  for(i in 1:pagecount)
  {
    movie = movies[i];

    rank = movie %>%
      html_node(".lister-item-index") %>%
      html_text() %>%
      as.numeric();
    result$rank[i] = rank;

    title = movie %>%
      html_node(".lister-item-header a") %>%
      html_text();
    result$title[i] = title;

    ttid = movie %>%
      html_node(".lister-item-header a") %>%
      html_attr("href");

    temp = strsplit(ttid,"/",fixed=T);
    ttid = temp[[1]][3];
    result$ttid[i] = ttid;

    year = movie %>%
      html_node(".lister-item-year") %>%
      html_text();
    year = cleanupYearIMDB(year);
    result$year[i] = year;

    rated = movie %>%
      html_node(".certificate") %>%
      html_text();
    result$rated[i] = rated;

    minutes = movie %>%
      html_node(".runtime") %>%
      html_text();
    minutes = cleanupMinutesIMDB(minutes);
    result$minutes[i] = minutes;

    genre = movie %>%
      html_node(".genre") %>%
      html_text();
    genre = str_trim(genre);
    result$genre[i] = genre;

    ratings = movie %>%
      html_node("div .rating-list") %>%
      html_attr("title");
    temp = strsplit(ratings,"/",fixed=T);
    temp = gsub("Users rated this","",temp[[1]][1],fixed=T);
    temp = str_trim(temp);
    ratings = as.numeric(temp);
    result$ratings[i] = ratings;

    metacritic = movie %>%
      html_node(".ratings-metascore span") %>%
      html_text();
    metacritic = as.numeric(str_trim(metacritic));
    result$metacritic[i] = metacritic;

    p = movie %>%
      html_nodes(".lister-item-content p");
    temp = strsplit(as.character(p),'class="',fixed=TRUE);
    temp2 = strsplit(temp[[2]][2],"\r\n",temp,fixed=TRUE);
    para = str_trim(gsub("</p>","",temp2[[1]][2],fixed=TRUE));

    result$paragraph[i] = para;

    info = movie %>%
      html_nodes(".lister-item-content p span") %>%
      html_text();

    votes = as.numeric(gsub(",","",info[8],fixed=T));
    result$votes[i] = votes;

    millions = cleanupMillionsIMDB(info[11]);
    result$millions[i] = millions;
  }

  list("movies"=result, "totalcount"=totalcount);
  }



#' Title
#'
#' @param millions
#' @family IMDB-scrape
#'
#' @return
#' @export
#'
#' @examples
cleanupMillionsIMDB = function(millions)
{
  millions = gsub('$','',millions, fixed=T);
  millions = gsub('M','',millions, fixed=T);

  millions = as.numeric(millions);
  millions;
}

cleanupMinutesIMDB = function(minutes)
{
  minutes = gsub('min','',minutes, fixed=T);

  minutes = as.numeric(minutes);
  minutes;
}

cleanupYearIMDB = function(year)
{
  year = gsub('(','',year, fixed=T);
  year = gsub(')','',year, fixed=T);
  year = gsub('I','',year, fixed=T);
  year = as.numeric(year);
  year;
}

grabbingProductionFromFilm = function(raw)
  {
  cache = gsub("filmInfo.html", "production.txt", raw, fixed=TRUE);
  if(file.exists(cache))
    {
    #production.df = utils::read.csv(cache, header=TRUE, sep="|");
    #persons = production.df$actors;
    #return (persons);

    result = readRDS(cache);
    return (result);
    }
  rvest.html = xml2::read_html(raw, encoding = "ISO-8859-1");

  result = list();

  credits = rvest.html %>%
            html_nodes(".credit_summary_item");
  n.c = length(credits);

  persons = as.data.frame(matrix(NA, ncol=2)); colnames(persons) = c("role","nmid");
  idx = 1;
  if(n.c > 0)
    {
    for(j in 1:n.c)
      {
      credit = credits[j];

      myrole = credit %>%
            html_node("h4")  %>%
            html_text();

      myrole = tolower( gsub(":","",myrole,fixed=TRUE) );



      mywhos = credit %>%
            html_nodes("a")  %>%
            as.character();

      n.w = length(mywhos);


      if(n.w > 0)
        {
        for(k in 1:n.w)
          {
          who = mywhos[k];
          temp = strsplit(who,"/",fixed=TRUE);
          nmid = str_trim(temp[[1]][3]);
          if(nmid != "a>")
            {
            persons[idx,] = c(myrole,nmid);
            idx = 1 + idx;
            }
          }
        }

      }
    }

  result$persons = persons;
  ##########   budget ############# while we are here ...
  keys = rvest.html %>%
        html_nodes("#titleDetails .txt-block h4") %>%
        html_text() %>%
        tolower();

  values = rvest.html %>%
        html_nodes("#titleDetails .txt-block");

  n.k = length(keys);



  v = list();
  v$co = c();
  v$cona = c();
  v$href.txt = c();
  if(n.k > 0)
  {
  for(j in 1:n.k)
    {
    key = gsub(":","",keys[j],fixed=TRUE);
    v$keys[j] = key;

    value = values[j];

      hrefs = value %>%
        html_nodes("a");


    href.final = c();
      if(length(hrefs) > 0)
        {
        for(k in 1:length(hrefs))
          {
          href.txt = hrefs[k]  %>% html_text() %>%  str_trim();


          if( !(href.txt =="See more" || href.txt =="IMDbPro" || is.na(href.txt)) )
            {
            #v$href.txt[j][k] = href.txt;
            href.final = c(href.final,href.txt);
            }
          if(key == "production co")
            {
            href = as.character(hrefs[k]);
            temp = strsplit(href,"/",fixed=TRUE);
            temp2 = strsplit(temp[[1]][3],"?",fixed=TRUE);
            myco = str_trim(temp2[[1]][1]);
            if(!is.na(myco))
              {
              v$co = c(v$co, myco);
              v$cona = c(v$cona, href.txt);
              }
            }
          }
        }

      href.final.str = paste(as.character(href.final),collapse=", ",sep="");

      v$href = c(v$href, href.final.str);
      # let's get text after h4, not in an <a tag
      temp = strsplit( as.character(value), "</h4>", fixed=TRUE);
      temp2 = strsplit( temp[[1]][2], "</a>", fixed=TRUE);
      temp3 = strsplit( temp2[[1]][1], "<span", fixed=TRUE);
      my.txt = str_trim(temp3[[1]][1]);
      my.txt = gsub("<.*?>", "", my.txt);  # remove any stray tags...

      v$txt[j] = str_trim(my.txt);

    }
  }


  result$extra = v;

  saveRDS(result, cache, ascii=TRUE);



  result;
  }




grabbingActorsFromFilm = function(raw)
  {
  cache = gsub("filmInfo.html", "actors-played.txt", raw, fixed=TRUE);
  # if(file.exists(cache))
  #   {
  #   actors.df = utils::read.csv(cache, header=TRUE, sep="|");
  #   actors = actors.df$actors;
  #   return (actors);
  #   }

  rvest.html = xml2::read_html(raw, encoding = "ISO-8859-1");
  # hrefs = rvest.html %>%
  #           html_nodes(".cast_list tr .primary_photo");
  rows = rvest.html %>%
               html_nodes(".cast_list tr");

  na = length(rows);
  actors = NULL;
  names = NULL;
  characters = NULL;
  if(na > 0)
    {
    for(i in 1:na)
      {
      ######################
      actor = rows[i] %>%
              html_node(".primary_photo")  %>%
                  as.character();
      #print(actor);
      if(!is.na(actor))     # first row is garbage?
        {                   # Cast overview, first billed only:
        temp = strsplit(actor,"/",fixed=TRUE);
        actors[i] = temp[[1]][3];

        ######################
        cols = rows[i] %>%
                html_nodes("td")  %>%
                    html_text();

        myname = str_trim(cols[2]);
        myname = gsub("[[:space:]]", " ", myname);
          names[i] = myname;

        mycharacter = str_trim(cols[4]);
        mycharacter = gsub("[[:space:]]", " ", mycharacter);
          characters[i] = mycharacter;
        }
      }
    }
  # caching
  if(na > 0)
    {
    actors.df = removeNAsFromDataFrame( data.frame( cbind(actors,names,characters) ) );
      colnames(actors.df) = c("actors","names","characters");
    utils::write.table(actors.df, file=cache, quote=FALSE, col.names=TRUE, row.names=FALSE, sep="|");
    }
  actors;
  }

parseTop250 = function(raw)
  {
  rvest.html = xml2::read_html(raw, encoding = "ISO-8859-1");

  hrefs = rvest.html %>%
    html_nodes(".posterColumn");

  movies = NULL;
  for(i in 1:250)
    {
    movie = hrefs[i] %>%
                html_node("a") %>%
                as.character();
    temp = strsplit(movie,"/",fixed=TRUE);
    movies[i] = temp[[1]][3];
    }
  movies;
  }
