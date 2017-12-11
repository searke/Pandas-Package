# Pandas-Package
This Mathematica notebook is a quick hack to get some of the cool indexing functionality of the pandas package for Python.

It creates an expression called pandasSeries that can be sliced like in R. 
So far I have only implemented series and not the pane data structure. 
The code in here isn't meant to be terribly efficent but instead is a proof of concept. 
Mathematica doesn't have any iterator design pattern and so I've written a bit of a hack to get the pandasSeries expression to work wherever a normal List would. 
A pandasSeries is comprised of a list of data values and possibly a list of labels for those values and a name for the collection. Some simple examples are below:  
```
  (* Generate two series with labels and take the complement of the first under the second *)  
  ps1 = pandasSeries[{1, 2, 2, 6, 4, 4}, {"a", "b", "c", "c", "c", "d"}, "ps1"]; 
  ps2 = pandasSeries[{3, 4, 6, 3, 4, 4}, {"a", "z", "k", "k", "c", "aleph"}, "ps2"]; 
  Complement[ps2, ps1]   
  pandasSeries[{4, 6, 3, 4}, {"z", "k", "k", "aleph"}, "ps2"]  
```

```
  (* Generate a thousand random points from the normal distribution and find the average of all of them greater than 0.5 *) 

 ps= pandasSeries[RandomVariate[NormalDistribution[], 1000], Null, Null]; 

Mean@ps[[ps > 0]]
```

