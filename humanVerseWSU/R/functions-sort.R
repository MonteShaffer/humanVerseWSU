
#' callOrderFunctionWithMatrixInput
#'
#' See \url{https://stackoverflow.com/questions/63801018/}
#'
#' @family Sort
#'
#' @param mat matrix
#'
#' @return this is placed in a dataframe to re-order it.
#' @export
#'
callOrderFunctionWithMatrixInput = function(mat)
	{
	do.call(order, split(mat, (seq(mat) - 1) %/% nrow(mat)));
	}

