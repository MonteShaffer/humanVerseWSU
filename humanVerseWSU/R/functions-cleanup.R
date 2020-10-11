


#' cleanupPersonalityDataFrame
#'
#' @param personality.raw The raw dataframe which can auto-populate
#'
#' @return An updated dataframe, cleaned and sorted
#' @export
cleanupPersonalityDataFrame = function(personality.raw = readRDS( system.file("extdata", "personality-raw.rds", package="humanVerseWSU") ) )
  {
  # personality.clean = cleanupPersonalityDataFrame();
  df = removeColumnsFromDataFrame(personality.raw, "V00");
  dim(df);  # 838


  ywd.cols = c("year","week","day");
  ywd = convertDateStringToFormat( df$date_test,
                                    c("%Y","%W","%j"),
                                    ywd.cols,
                                    "%m/%d/%Y %H:%M"
                                  );

  ndf = replaceDateStringWithDateColumns(df,"date_test",ywd);
  ndf = sortDataFrameByNumericColumns(ndf, ywd.cols, "DESC");
  ndf = removeDuplicatesFromDataFrame(ndf, "md5_email");

  dim(ndf); # 678
  ndf;
  }

