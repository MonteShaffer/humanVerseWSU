library(pvclust);
library(factoextra);
library(ggplot2);
library(psych);

plot.hclust.sub = function(X.hclust, k=12, mfrow = c(2,2))
  {
  # Let's show cuts as plots
  # https://stackoverflow.com/questions/34948606/hclust-with-cutree-how-to-plot-the-cutree-cluster-in-single-hclust

  colors = colorspace::rainbow_hcl(k);
  dend = stats::as.dendrogram(X.hclust);
  dend = dendextend::color_branches(dend, k = k);

  # grDevices::dev.new(width=10, height=7.5, noRStudioGD = TRUE)

  graphics::par(mfrow = c(1,1));
  graphics::plot(dend);

  dend.labels = base::labels(dend);
  groups = dendextend::cutree(dend, k=k, order_clusters_as_data = FALSE);
    # stats::cutree
    # dendextend::cutree
  dends = list();
  for(i in 1:k)
    {
    print( paste0("Pruning ",i," of ",k) );
    keep.me <- dend.labels[i != groups];
    dends[[i]] = dendextend::prune(dend, keep.me);  # generics  #  dendextend
    }


  graphics::par(mfrow = mfrow);
  for(i in 1:k)
    {
    graphics::plot(dends[[i]], main = paste0("Tree number ", i));
    }

  # restore plot
  graphics::par(mfrow = c(1,1));
  }


perform.hclust = function(X, n.groups = 12, method = "complete",
          dist.method = "euclidean", dist.p = 2,
          showPlots = TRUE, pvclust.parallel = FALSE )
  {
  times = c(); time.names = c();
  n.cols = ncol(X);
  if(n.groups > n.cols) { n.groups = n.cols; }

  colors = rainbow(n.groups, s = 0.6, v = 0.75);

  time.start = Sys.time();
      X = as.matrix(X);
      X.t = transposeMatrix(X);
      X.d = stats::dist(X, method=dist.method, p=dist.p);
  time.end = Sys.time();

  elapse = sprintf("%.3f", as.numeric(time.end) - as.numeric(time.start));
      times = c(times,elapse);
      time.names = c(time.names,"dist");

  time.start = Sys.time();
  X.hclust = stats::hclust( X.d, method=method);
  time.end = Sys.time();

  elapse = sprintf("%.3f", as.numeric(time.end) - as.numeric(time.start));
      times = c(times,elapse);
      time.names = c(time.names,"hclust");

  if(showPlots)
    {
    time.start = Sys.time();
    base::plot(X.hclust);
    time.end = Sys.time();

    elapse = sprintf("%.3f", as.numeric(time.end) - as.numeric(time.start));
        times = c(times,elapse);
        time.names = c(time.names,"hclust-plot");

    time.start = Sys.time();
    plot.hclust.sub(X.hclust, k=n.groups);
    time.end = Sys.time();

    elapse = sprintf("%.3f", as.numeric(time.end) - as.numeric(time.start));
        times = c(times,elapse);
        time.names = c(time.names,"hclust-plot-groups");
    }

  # pvclust
  time.start = Sys.time();
  X.pvclust = pvclust::pvclust ( X.t, method.hclust=method, parallel=pvclust.parallel);
  time.end = Sys.time();

  elapse = sprintf("%.3f", as.numeric(time.end) - as.numeric(time.start));
        times = c(times,elapse);
        time.names = c(time.names,"pvclust");


  if(showPlots)
    {
    time.start = Sys.time();
        graphics::plot(X.pvclust);
        pvclust::pvrect(X.pvclust);
    time.end = Sys.time();
    elapse = sprintf("%.3f", as.numeric(time.end) - as.numeric(time.start));
        times = c(times,elapse);
        time.names = c(time.names,"pvclust-plot");
    }

  timer = as.data.frame( cbind(time.names,times) );
    colnames(timer) = c("names", "times");
  timer$times = as.numeric(timer$times);

  list("dist" = X.d, "hclust" = X.hclust, "pvclust" = X.pvclust, "timer" = timer);
  }



perform.kmeans = function(X, centers = 12, algorithm = "Hartigan-Wong",
                      iter.max = 50, nstart=100, trace=FALSE, showPlots = TRUE,
                      stars.len = 0.5, stars.key.loc = c(4,3), stars.draw.segments = TRUE  )
  {
  if(length(centers) == 1)
    {
    colors = rainbow(centers, s = 0.6, v = 0.75);
    }
  X.kmeans = stats::kmeans(X, centers,
                      iter.max = iter.max, algorithm = algorithm,
                      nstart = nstart, trace = trace);

  if(showPlots)
    {
    palette(colors);

    graphics::stars(X.kmeans$centers, len = stars.len, key.loc = stars.key.loc,
        main = paste0("Algorithm: [",algorithm,"] \n Stars of KMEANS=", centers),
        draw.segments = stars.draw.segments);

    factoextra::fviz_cluster(X.kmeans, data=X);
    }

  membership = as.data.frame( matrix( X.kmeans$cluster, ncol=1)) ;
      rownames(membership) = row.names(X);
      colnames(membership) = c("cluster");


  list("kmeans" = X.kmeans, "membership" = membership, "table" = table(membership));
  }





perform.EFA = function(X, n.factors=8, which="factanal",
                          rotation = "varimax", scores = "regression",
                          fa.fm = "ml")
  {
  info = list();

  info$KMO = performKMOTest(X);
  print( paste0(" KMO test has score: ",info$KMO$KMO," --> ",info$KMO$msg) );

  info$BST = performBartlettSphericityTest(X);
  print( paste0(" Bartlett Test of Sphericity   --> ",info$BST$msg) );

  if(which == "factanal")
    {
    print(" Using stats::factanal ... ");

    X.factanal = stats::factanal(X, n.factors, rotation=rotation, scores=scores);
    print(" Overview ");
    print(X.factanal);


    print(" Uniqueness as (1-$uniquenesses)");
      round(1 - X.factanal$uniquenesses, digits=2);

    #print(" Communalities");
    #  round(X.factanal$communalities, digits=2);
    # https://www.youtube.com/watch?v=C5RJvMaHJNo


    print(" Loadings");
      print(X.factanal$loadings, digits=2, cutoff=0.25, sort=FALSE);

    plot(X.factanal$loadings[,1:2], type="n");
        text(X.factanal$loadings[,1:2],labels=names(X),cex=.7)

    if(scores != "regression")
      {
      X.factanal.regression = factanal(X, n.factors, rotation=rotation, scores="regression");
      myScores = X.factanal.regression$scores;
      } else { myScores = X.factanal$scores; }

    print(" Scores (Regression) ... saved ... ");
    X.factanal$scores = myScores;
    }

  if(which == "fa")
    {
    print(" Using psych::fa ... ");

    X.factanal = psych::fa(X, n.factors, rotate=rotation, scores=scores, fm=fa.fm);
    print(" Overview ");
    print(X.factanal);


    print(" Uniqueness as (1-$uniquenesses)");
      round(1 - X.factanal$uniquenesses, digits=2);

    #print(" Communalities");
    #  round(X.factanal$communalities, digits=2);

    # factor1 = c(1,3,7,8,15);
    # psych::alpha( df[, factor1]);

    print(" Loadings");
      print(X.factanal$loadings, digits=2, cutoff=0.25, sort=FALSE);

    plot(X.factanal$loadings[,1:2], type="n");
        text(X.factanal$loadings[,1:2],labels=names(X),cex=.7)

    if(scores != "regression")
      {
      X.factanal.regression = psych::fa(X, n.factors, rotate=rotation, scores="regression", fm=fa.fm);
      myScores = X.factanal.regression$scores;
      } else { myScores = X.factanal$scores; }

    print(" Scores (Regression) ... saved ... ");

    print(" CFI ");
    X.factanal$CFI = 1-((X.factanal$STATISTIC - X.factanal$dof)/(X.factanal$null.chisq - X.factanal$null.dof));
    print(X.factanal$CFI);
    print(" TLI ");
    print(X.factanal$TLI);

    X.factanal$scores = myScores;
    }





  X.factanal;
  }


howManyFactorsToSelect = function(X, max.factors = 12, rotate = "varimax",
                              eigen.cutoff = 1, alpha = 0.05, showPlots = TRUE,
                              fa.fm="minres", fa.fa="fa", fa.iter=50,
                              fa.error.bars=FALSE, fa.se.bars=FALSE)
  {
  # for VSS, we will let the others run wild ...
  # eigen > 1 may have more than max.factors ...
  print("  Paralell Analysis");
  fa = psych::fa.parallel(Xs, fm = fa.fm, fa = fa.fa, n.iter = fa.iter,
                        error.bars = fa.error.bars, se.bars=fa.se.bars);

  n.cols = ncol(X);
  n.rows = nrow(X);
  if(max.factors > n.cols) { max.factors = n.cols; }

  choices = c();

  print("=============================================");
  print("  VSS Analysis");
  myVSS = psych::vss(X, n = max.factors, rotate=rotate, plot=showPlots);

  myVSS.dataframe = as.data.frame(cbind(1:max.factors,myVSS$vss.stats[,c(1:3)]) );
      colnames(myVSS.dataframe) = c("Factors","df","chisq","pvalue");
  myVSS.dataframe = myVSS.dataframe[myVSS.dataframe$dof > 0, ];

  if(dim(myVSS.dataframe)[1] > 0)
    {
    myVSS.dataframe$isFactorChoiceValid = performSimpleChiSquaredTest(myVSS.dataframe$chisq,
                                            myVSS.dataframe$dof, alpha=alpha);

    myVSS.dataframe;
    n.vss = getIndexOfDataFrameRows(myVSS.dataframe,"isFactorChoiceValid",TRUE);

    if(!anyNA(n.vss)) { choices = c(choices, n.vss); }
    }

  X.corr = stats::cor(X);
  print("************************");


  X.corr.eigen = base::eigen(X.corr)$values;
  eigen.rule = X.corr.eigen[X.corr.eigen >= eigen.cutoff];
  print( paste0("  Eigenvalues >= ",eigen.cutoff, "   ...  [ n = ",length(eigen.rule)," ]") );
  print(eigen.rule);
  print("************************");

  n.eigen = length(eigen.rule);
  if(n.eigen != 0)
    {
    for(i in 1:n.eigen)
      {
      choices = c(choices, i);
      }
    }

  if(showPlots)
    {
    nFactors::plotuScree(X.corr.eigen);
    graphics::abline(h = 1, col="blue");
    }
  nResults = nFactors::nScree(eig = X.corr.eigen,
              aparallel = nFactors::parallel(
                              subject = n.rows,
                              var = n.cols )$eigen$qevpea);

  choices = c(choices, nResults$Components$noc);  # optimal coordinates
  choices = c(choices, nResults$Components$naf);  # acceleration factor
  choices = c(choices, nResults$Components$nparallel); # parallel analysis
  choices = c(choices, nResults$Components$nkaiser); # eigenvalues

  strong = FALSE;  # strongly recommend
  if(nResults$Components$noc == nResults$Components$nparallel) { strong = nResults$Components$noc; }

  if(showPlots)
    {
    nFactors::plotnScree(nResults, main="Component Retention Analysis");
    }

  myTable = as.data.frame( table(choices), row.names=NULL );
    colnames(myTable) = c("Factor", "vote.count");

  votes = whichMaxFreq(choices);

  for(i in 1:length(votes))
    {
    print( paste0("A ",votes[i], "-Factor solution has the most votes!") );
    }
  print("");

  if(!isFALSE(strong))
    {
    print("Due to Optimal Coordinantes and Parallel Analysis Agreement,");
    print(  paste0("A ",strong, "-Factor solution is *strongly* recommended!") );
    }
  print("************************");
  print("  Final Analysis of VSS, Eigen, nFactors");
  print(myTable);
  print("");

  list("fa" = fa, "eigen" = eigen.rule, "table" = myTable, "votes" = votes , "strongly" = strong);
  }


