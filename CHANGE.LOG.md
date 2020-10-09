
# 0.1.4.1

* updated `functions-file.R`
  - added function `loadRDS` :: it loads datasets living in package
* added personality.json files
* updated `functions-dataframe.R`
  - added function `getIndexOfDataFrameRows` :: it now returns NA if not found ... 
  - added function `subsetDataFrame` :: it now returns NA if not found ... 
  - edited function `getIndexOfDataFrameColumns` :: it now returns NA if not found ... 
  - edited function `findAllIndexesWithValueInVector` :: it now returns NA if not found ...
* added `functions-search.R`
  - added function `wildcardSearch`  
* added `functions-test.R`
  - added function `performBartlettSphericityTest` 
  - added function `performKMOTest`
  - added function `performSimpleChiSquaredTest`
* added `functions-EDA.R`  (not compiled, TODO documentation)
  - added function `howManyFactorsToSelect`
  - added function `perform.hclust`
  - added function `plot.hclust.sub`
* added data `protein.rds` as /inst/extdata  
* added `functions-encryption.R`
  - added function `md5`
* added `functions-maths.R`
  - added function `deg2rad`
  - added function `rad2deg`
  - added function `buildBoundingBoxFromRadiusAndGivenLatitudeLongitude`
* added `functions-cleanup.R`
  - added function `cleanupPersonalityDataFrame`  
* added `functions-mysql.R`  
  - added functions to replicate "store locator" more efficiently
  
```
library(RMariaDB); # install.packages("RMariaDB", dependencies=TRUE);

# copy/paste _SECRET_.txt into console...
mysql.connection = mysql.secretConnectionSQL(str="WSU_SANDBOX_", save="wsu");

	my.tablename = "zipcodes";
	my.zipcode = 99163;  # or possible a string # my.zipcode = "99163";	
sql.template = "SELECT * FROM {tablename} WHERE zipcode = '{zipcode}';";
	keys = c("tablename", "zipcode");
	vals = c(my.tablename, my.zipcode);	
sql = parseTemplateSQL(sql.template, keys, vals);
result = mysql.fetchAllSQL(mysql.connection, sql);
result;

	my.radius = 10; my.units = "mi"; #miles
	my.latitude = result$latitude; my.longitude = result$longitude; 
box = buildBoundingBoxFromRadiusAndGivenLatitudeLongitude(my.radius, my.latitude, my.longitude, my.units);

sql.template = "SELECT * FROM {tablename} WHERE latitude > {latitude.lower} AND latitude < {latitude.upper} AND longitude < {longitude.lower} AND longitude > {longitude.upper} ORDER BY zipcode ASC;";
	keys = c("tablename", "latitude.lower", "latitude.upper", "longitude.lower", "longitude.upper");	
	vals = c(my.tablename, box);	
sql = parseTemplateSQL(sql.template, keys, vals);
neighbors = mysql.fetchAllSQL(mysql.connection, sql);
neighbors;

RMariaDB::dbDisconnect(mysql.connection);
```
  
  
# 0.1.4.0

* moved IMDB dataset to its own repository <https://github.com/MonteShaffer/imdb>

```
library(devtools);
detach(package:imdb); # if older version is attached with library(imdb);
install_github("MonteShaffer/imdb/imdb");
# # Choose (1) if possible, (3) otherwise ...
library(imdb);
```

# 0.1.3.0

* added the IMDB dataset (50MB) to the system



This would be nice <https://github.com/vaab/gitchangelog>
