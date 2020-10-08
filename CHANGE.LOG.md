
# 0.1.4.1

* added personality.json files
* updated `functions-dataframe.R`
  - added function `getIndexOfDataFrameRows` :: it now returns NA if not found ... 
  - added function `subsetDataFrame` :: it now returns NA if not found ... 
  - edited function `getIndexOfDataFrameColumns` :: it now returns NA if not found ... 
* add `functions-search.R`
  - added function `wildcardSearch`  
* updated `functions-dataframe.R`
  - edited function `findAllIndexesWithValueInVector` :: it now returns NA if not found ...
* add `functions-test.R`
  - added function `performBartlettSphericityTest` 
  - added function `performKMOTest`

# 0.1.4.0

* moved IMDB dataset to its own repository <https://github.com/MonteShaffer/imdb>

```
library(devtools);
detach(package:imdb);
install_github("MonteShaffer/imdb/imdb");
# # Choose (1) if possible, (3) otherwise ...
library(imdb);
```

# 0.1.3.0

* added the IMDB dataset (50MB) to the system



This would be nice <https://github.com/vaab/gitchangelog>
