

#' performSimpleChiSquaredTest
#'
#' @param chi numeric (can be vector)
#' @param df numeric (can be vector of matching length)
#' @param alpha numeric alpha-level, default is 0.05
#'
#' @return list of results
#' @export
#'
#' @examples
#' performSimpleChiSquaredTest(1944.5, 36);
#' performSimpleChiSquaredTest(12, 22);
#' performSimpleChiSquaredTest(22, 12);
#' performSimpleChiSquaredTest(22, 12, alpha=0.01);
performSimpleChiSquaredTest = function(chi, df, alpha=0.05)
  {
  # performSimpleChiSquaredTest(1944.5,36);
  n.chi = length(chi);
  n.df = length(df);
  if(n.chi != n.df)
    {
    warning("Something wrong in performSimpleChiSquaredTest ... chi and df are of different lengths");
    return (NA);
    }

  truth = c();
  pvalues = c();

  for(i in 1:n.chi)
    {
    pvalue = 1 - stats::pchisq(chi[i],df[i]);
    pvalues[i] = pvalue;

    truth[i] = pvalue < alpha;
    }

  list("alpha" = alpha, "pvalue" = pvalue, "truth" = truth);
  }

#' performBartlettSphericityTest
#'
#' @param X Either the numeric dataframe or the correlation matrix of the dataframe
#' @param n.obs If null, I assume X is a dataframe, not a correlation matrix
#' @param alpha Alpha level (0.05) to report findings as $msg
#'
#' @return list with $pvalue and $msg
#' @export
#'
#' @examples
#' library(datasets);
#' data(iris);
#' head(iris);
#' performBartlettSphericityTest(iris[,1:4]);
#' performBartlettSphericityTest(iris[,1:4], alpha=0.01);
#' performBartlettSphericityTest(cor(iris[,1:4]), n.obs = nrow(iris));
performBartlettSphericityTest = function(X, n.obs = NULL, alpha = 0.05)
	{
  # <https://www.ibm.com/support/knowledgecenter/SSLVMB_23.0.0/spss/tutorials/fac_telco_kmo_01.html>
  if(is.null(n.obs))
    {
    # if X is data, it is now in correlation form X
    n.obs = nrow(X);
    X = stats::cor(X);  # square ...
    }
  myTest = psych::cortest.bartlett(X, n = n.obs);

  pvalue = myTest$`p.value`;

  if(pvalue < alpha)
    {
    msg = (paste0("Bartlett's test of sphericity ... pvalue < alpha ... ", pvalue , " < ", alpha, " ... \n CONCLUSION: we believe this data is likely suitable for factor analysis or PCA"));
    } else {
          msg = "Oh snap!";
          msg = paste0(msg,"\n", "To put this in layman's terms, the  variables in our dataset are fairly uncorrelated so a data reduction technique like PCA or factor analysis would have a hard time compressing these variables into linear combinations that are able to capture significant variance present in the data. <https://www.statology.org/bartletts-test-of-sphericity/>");
          }

  list("pvalue" = pvalue, "msg" = msg);
  }




#' performKMOTest
#'
#' @param X Either the numeric dataframe or the correlation matrix of the dataframe
#' @return list with $KMO and $msg
#' @export
#'
#' @examples
#' library(datasets);
#' data(iris);
#' head(iris);
#' performKMOTest(iris[,1:4]);
#'
#' setosa = subsetDataFrame( iris, "Species", "setosa" );
#' performKMOTest( setosa[,1:4] );
performKMOTest = function(X)
	{
  # <https://www.ibm.com/support/knowledgecenter/SSLVMB_23.0.0/spss/tutorials/fac_telco_kmo_01.html>

  myTest = REdaS::KMOS(X);

  my.kmo = myTest$KMO;


if(my.kmo >= 0.90)
  {
  msg = ("marvelous!");
  } else if(my.kmo >= 0.80)
    {
    msg = ("meritorious!");
    }  else if(my.kmo >= 0.70)
        {
        msg = ("middling!");
        } else if(my.kmo >= 0.60)
            {
            msg = ("mediocre!");
            }  else if(my.kmo >= 0.50)
                {
                msg = ("miserable!");
                } else {
                        msg = ("mayhem!");
                        msg = paste0(msg, "\n", "Oh snap!");
                        msg = paste0(msg, "\n", "Kaiser-Meyer-Olkin (KMO) Test is a measure of how suited your data is for Factor Analysis. The test measures sampling adequacy for each variable in the model and for the complete model. The statistic is a measure of the proportion of variance among variables that might be common variance. The lower the proportion, the more suited your data is to Factor Analysis. <https://www.statisticshowto.com/kaiser-meyer-olkin/>");
                        }

  list("KMO" = my.kmo, "msg" = msg);
  }



### Bartlett Test of Sphericity
# Bartlett's test
# https://en.wikipedia.org/wiki/Bartlett's_test
# > ## Bartlett Test of Sphericity
# > p = 22; lnR = log(det(R)); test = -1*((n-1)-(2*p+5)/6)*lnR; crit = qchisq(.95, df=(p^2-p)/2);
# > test > crit;
# > print("If TRUE, Reject Null of Sphericity => continue with dimension reduction");
# >
# http://personality-project.org/r/html/cortest.bartlett.html


# https://stats.idre.ucla.edu/spss/output/factor-analysis/
# a.  Kaiser-Meyer-Olkin Measure of Sampling Adequacy ??? This measure varies between 0 and 1, and values closer to 1 are better.  A value of .6 is a suggested minimum.
#
# b.  Bartlett???s Test of Sphericity ??? This tests the null hypothesis that the correlation matrix is an identity matrix.  An identity matrix is matrix in which all of the diagonal elements are 1 and all off diagonal elements are 0. You want to reject this null hypothesis.
#
# Taken together, these tests provide a minimum standard which should be passed before a factor analysis (or a principal components analysis) should be conducted.
#
### Levene's test
# https://en.wikipedia.org/wiki/Levene%27s_test
# http://personality-project.org/r/html/KMO.html

# https://www.researchgate.net/publication/5142799_Dimension_Reduction_Regression_in_R

# https://www.statmethods.net/advstats/factor.html

### Box's M test
# https://en.wikipedia.org/wiki/Box%27s_M_test
# HW 7, pg 5 (b)
# final, pg 11 ...



