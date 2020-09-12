devtools::document();


data(package="humanVerseWSU")

write.table(idf,file="inst/extdata/inflation.txt",sep="|",row.names=F,quote=F);


library(devtools);
install_github("MonteShaffer/humanVerseWSU/humanVerseWSU");
library(humanVerseWSU);


saveRDS(idf,file="inst/extdata/inflation.rds");

idf <- readRDS("inst/extdata/inflation.rds")


idf = readRDS( system.file("extdata", "inflation.rds", package="humanVerseWSU") );



fpath <- system.file("extdata", "inflation.rds", package="my_package")

https://stackoverflow.com/questions/13463103/inst-and-extdata-folders-in-r-packaging

