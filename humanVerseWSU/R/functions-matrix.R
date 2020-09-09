

#' transposeMatrix
#'
#' @param mat a matrix
#'
#' @return matrix, updated
#' @export
#'
#' @examples
#' m = as.matrix(structure(c(1, 0, 4, 0, 3, 0, 2, 0, 5), .Dim = c(3L, 3L)));
#' transposeMatrix(m);
#' 
transposeMatrix = function(mat)
	{
	t(mat);	
	}
	
#' rotateMatrix
#'
#' @param x matrix
#' @param clockwise direction of rotation, default is clockwise
#'
#' @return matrix, updated
#' @export
#'
#' @examples
#' m = as.matrix(structure(c(1, 0, 4, 0, 3, 0, 2, 0, 5), .Dim = c(3L, 3L)));
#' rotateMatrix(m);
#' rotateMatrix(m,F);
#' 
rotateMatrix = function(x,clockwise=T) 
	{
	if(clockwise)
		{
		t( apply(x, 2, rev) );
		} else 	{
				apply( t(x),2, rev);
				}
	}
	
#' rotateMatrix90
#'
#' @param x matrix
#' @param clockwise direction of rotation, default is clockwise
#'
#' @return matrix, updated
#' @export
#'
#' @examples
#' m = as.matrix(structure(c(1, 0, 4, 0, 3, 0, 2, 0, 5), .Dim = c(3L, 3L)));
#' rotateMatrix90(m);
#' rotateMatrix90(m,F);
#' 
rotateMatrix90 = function(x,clockwise=T) 
	{
  rotateMatrix(x,clockwise);	
	}

#' rotateMatrix180
#'
#' @param x matrix
#' @param clockwise direction of rotation, default is clockwise
#'
#' @return matrix, updated
#' @export
#'
#' @examples
#' m = as.matrix(structure(c(1, 0, 4, 0, 3, 0, 2, 0, 5), .Dim = c(3L, 3L)));
#' rotateMatrix180(m);
#' rotateMatrix180(m,F);  # equivalent
#' 	
rotateMatrix180 = function(x,clockwise=T) 
	{
  rotateMatrix(
    rotateMatrix(x,
			clockwise),
		clockwise);

	}

#' rotateMatrix270
#'
#' @param x matrix
#' @param clockwise direction of rotation, default is clockwise
#'
#' @return matrix, updated
#' @export
#'
#' @examples
#' m = as.matrix(structure(c(1, 0, 4, 0, 3, 0, 2, 0, 5), .Dim = c(3L, 3L)));
#' rotateMatrix270(m);
#' 
#' rotateMatrix270(m,F);
#' rotateMatrix90(m); # equivalent
#' 	
rotateMatrix270 = function(x,clockwise=T) 
	{
  rotateMatrix(
    rotateMatrix(
      rotateMatrix(x,
			clockwise),
		clockwise),
	clockwise);	
	}


#' rotateMatrixAngle
#'
#' @param x matrix
#' @param a angle, must be a multiple of 90 degrees, can be 630
#' @param clockwise direction of rotation, default is clockwise
#'
#' @return matrix, updated
#' @export
#'
#' @examples
#' m = as.matrix(structure(c(1, 0, 4, 0, 3, 0, 2, 0, 5), .Dim = c(3L, 3L)));
#' 
#' rotateMatrixAngle(m,0);
#' rotateMatrixAngle(m,90);
#' rotateMatrixAngle(m,180);
#' rotateMatrixAngle(m,270);
#' rotateMatrixAngle(m,-90); # we reverse the direction of clockwise if negative
#' 
#' rotateMatrixAngle(m,270,F);
#' rotateMatrixAngle(m,-90,F); # we reverse the direction of clockwise if negative
#' 
#' rotateMatrixAngle(m,33);
#' rotateMatrixAngle(m,630);
#' 
rotateMatrixAngle = function(x, a=0, clockwise=T)
	{
	rem = a %% 90;
	if(rem != 0) 
		{ 
		stop("Not a valid angle"); 
		}
	
	if(a < 0)
	  {
	  # let's reverse the direction
	  clockwise = if(clockwise) FALSE else TRUE;
	  a = abs(a);
	  }
	div = a %% 360;
	
	switch( as.character(div),
		"90" 	= rotateMatrix90 (x,clockwise),
		"180" = rotateMatrix180(x,clockwise),
		"270" = rotateMatrix270(x,clockwise),
		x
		);
	
	}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
if(FALSE)
	{
	switch(skew,
          "uniform" = c(0.2,0.4,0.6,0.8,1),
          "skew-right" = c(0.1,0.2,0.3,0.7,1),
          "skew-left" = c(0.3,0.7,0.8,0.9,1),
          "normative" = c(0.1,0.3,0.7,0.9,1),
          c(0.2,0.4,0.6,0.8,1) # default case of switch
        );
		
		
  myMatrix = matrix ( c (
    1, 0, 2,
    0, 3, 0,
    4, 0, 5
  ), nrow=3, byrow=T);
  
  
	## functions

education = function(one)
{
  result = list();
  result$who 		= one;
  result$think 	= c("intensitively", "critically");
  result$goal 	= "intelligences + character";
  result;
}


##  Cntrl+Shift+C
# myMatrix = matrix ( c (
#   1, 0, 2,
#   0, 3, 0,
#   4, 0, 5
# ), nrow=3, byrow=T);

transposeMatrix = function(mat)
{
  t(mat);
}

#rotateMatrix90(mat)  ## clockwise
#rotateMatrix180(mat)
#rotateMatrix270(mat)
# 3x3 matrix ... ## matrix multiplication
# rotateMatrix (mat, a) ... 90, 180, 270, 0


	}
		
	
	