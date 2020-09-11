
# install.packages("stringr", dependencies=T);
#library(stringr);
# install.packages("rvest", dependencies=T);
#library(rvest);

# Research question:  who is a better actor?  Will Smith?  Denzel Washington?

## actor ... person_id
##			movie_id ... details
##			name ... count movies

# https://rvest.tidyverse.org/index.html

## Denzel Washington [nm0000243] vs Will Smith [nm0000226]

## https://www.imdb.com/filmosearch/?explore=title_type&role=nm0000243&ref_=filmo_ref_typ&sort=num_votes,desc&mode=detail&page=1&title_type=movie

# R , javascript, php, (c/c++)

# imdb ...


grabFilmInfoFromFilmsPage = function(page)
{
  # 50 elements
  # # title = id = rank = year = rating = minutes = genre = votes = metascore = desc = millions

  movies = page %>%
    html_nodes(".mode-detail");



  pagecount = length(movies);

  result = data.frame( 			matrix(ncol = 11,nrow = pagecount) );
  # a matrix-type form with lots of NA values ...

  colnames(result) = c("rank", "title", "ttid", "year", "rated", "minutes", "genre", "ratings", "metacritic", "votes", "millions");


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
    year = cleanupYear(year);
    result$year[i] = year;

    rated = movie %>%
      html_node(".certificate") %>%
      html_text();
    result$rated[i] = rated;

    minutes = movie %>%
      html_node(".runtime") %>%
      html_text();
    minutes = cleanupMinutes(minutes);
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

    # para ... +5 EASTER EGG ...

    info = movie %>%
      html_nodes(".lister-item-content p span") %>%
      html_text();

    votes = as.numeric(gsub(",","",info[8],fixed=T));
    result$votes[i] = votes;

    millions = cleanupMillions(info[11]);
    result$millions[i] = millions;
  }

  #str(result);

  result;
}







cleanupMillions = function(millions)
{
  millions = gsub('$','',millions, fixed=T);
  millions = gsub('M','',millions, fixed=T);

  millions = as.numeric(millions);
  millions;
}

cleanupMinutes = function(minutes)
{
  minutes = gsub('min','',minutes, fixed=T);

  minutes = as.numeric(minutes);
  minutes;
}

cleanupYear = function(year)
{
  year = gsub('(','',year, fixed=T);
  year = gsub(')','',year, fixed=T);
  year = gsub('I','',year, fixed=T);
  year = as.numeric(year);
  year;
}

grabNameFromFilmsPage = function(page)
{
  name = page %>%
    html_node(".header") %>%
    html_text();

  name = gsub("Most Rated Feature Films With","",name,fixed=T);
  name = str_trim(name);

  name;
}


grabFilmCountFromFilmsPage = function(page)
{
  totalcount = page %>%
    html_nodes(".desc") %>%
    html_text();

  temp = strsplit(totalcount,"of",fixed=T);
  temp2 = strsplit(temp[[1]][2],"titles", fixed=T);

  totalcount = str_trim(temp2[[1]][1]);
  totalcount = as.numeric(totalcount);

  temp2 = strsplit(temp[[1]][1],"to", fixed=T);

  pagecount = str_trim(temp2[[1]][2]);
  pagecount = as.numeric(pagecount);

  result = list();

  result$totalcount = totalcount;
  result$pagecount = pagecount;

  result;
}


#   nmid = "nm0000226";
# 	will = grabFilmsForPerson(nmid);
# 	plot(will$movies.50[,c(1,6,7:10)]);
#  	boxplot(will$movies.50$millions);

#   nmid = "nm0000243";
# 	denzel = grabFilmsForPerson(nmid);
# 	plot(denzel$movies.50[,c(1,6,7:10)]);
#  	boxplot(denzel$movies.50$millions);


# https://www.imdb.com/title/tt0466839/?ref_=filmo_li_tt ... get box office budget/gross if NA ... on millions. ..

grabFilmsForPerson = function(nmid)
{
  url = paste("https://www.imdb.com/filmosearch/?explore=title_type&role=",nmid,"&ref_=filmo_ref_typ&sort=num_votes,desc&mode=detail&page=1&title_type=movie", sep="");

  page1 = read_html(url);
    # imdb
    #     nmid/$nmid/
    #       - top-by-votes_01.html
    #     ttid
    # imdb/$md5/page_01.html  ... $md5 = md5(url);
  
  result = list();
  ## useful for other data purposes
  result$nmid = nmid;

  ## name of person
  result$name = grabNameFromFilmsPage(page1);
  result$countfilms = grabFilmCountFromFilmsPage(page1);

  result$movies.50 = grabFilmInfoFromFilmsPage(page1);




  ##  parallel format ...
  # ranks = page1 %>%
  # html_nodes(".lister-item-index") %>%
  # html_text() %>%
  # as.numeric();

  # ranks;

  # years = page1 %>%
  # html_nodes(".lister-item-year") %>%
  # html_text();

  # years = gsub('(','',years, fixed=T);
  # years = gsub(')','',years, fixed=T);
  # years = gsub('I','',years, fixed=T);
  # years = as.numeric(years);

  # titles = page1 %>%
  # html_nodes(".lister-item-header a") %>%
  # html_text();

  # titles;


  result;
}
