devtools::document();


data(package="humanVerseWSU")

write.table(idf,file="inst/extdata/inflation.txt",sep="|",row.names=F,quote=F);


saveRDS(idf,file="inst/extdata/inflation.rds");

idf <- readRDS("inst/extdata/inflation.rds")



fpath <- system.file("extdata", "my_raw_data.csv", package="my_package")

https://stackoverflow.com/questions/13463103/inst-and-extdata-folders-in-r-packaging

