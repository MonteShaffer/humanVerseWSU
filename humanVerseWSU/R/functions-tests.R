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



