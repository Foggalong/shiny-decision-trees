# Decision Tree Shiny App
In summer 2020 [Isabella Deutsch](https://isabelladeutsch.com/) and [myself](https://www.maths.ed.ac.uk/~jfogg/) gave a mini lecture on machine learning as part of a [Sutton Trust](https://www.suttontrust.com/) summer school. One of the questions on our problem sheet asked students to explore building a decision tree for spam filtering using this Shiny App. For the duration of the summer school the app is [deployed on ShinyApps.io](https://foggalong.shinyapps.io/sutton-dt/).


## Background
The webapp is written in the [R programming language](https://en.wikipedia.org/wiki/R_(programming_language)) using a toolkit called [Shiny](https://shiny.rstudio.com/) to create the interface.

R itself is a reasonably mature language (26 years old at time of writing) well favoured by statisticians across academia and industry for the depth of the tools it provides \[1\]\[2\]. For example, a function called [`rpart`](https://www.rdocumentation.org/packages/rpart/versions/4.1-15/topics/rpart) does a considerably portion of the heavy lifting here in calculating the decision tree. If you do a math degree at University you will no doubt come across R in your statistics courses.

Shiny by comparison is relatively new; development started about 8 years ago but it's really come into its own over the last couple of years. Creating intuitive, interactive webapps is normally a tricky business and it's an area of development in its own right. Part of what makes Shiny so appealing is that it strips UI back to a basic selection of customisable widgets, all handled in R. This makes it incredibly easy for statisticians, most of whom have no background in UI or web development, to create and deploy nice looking applications for others to explore their work.

## Data Set
The app uses the [Spambase Data Set](https://archive.ics.uci.edu/ml/datasets/Spambase) from the UCI Machine Learning Repository \[1\]. A partitioned version of this dataset is included with this repository, but in summary each row is a different and the columns are as follows:

- 1 to 48 are "word" (i.e. a sequence of on-whitespace characters) frequencies as percentages of the email body text,
- 49 to 54 are character frequencies as percentages of the email body text,
- 55 is the average length of an uninterrupted sequences of capital letters,
- 56 is the length of the longest uninterrupted sequence of capital letters,
- 57 is the total number number of capital letters in the email,
- 58 is a bool (i.e. true/false) variable as to whether the email is truly spam.

There are 4,601 emails of which 1,813 are spam and 2,788 are not. As mentioned we've randomly partitioned this into two files, [one for training](https://github.com/Foggalong/shiny-decision-trees/blob/master/training-data.csv) (70% of the entries) and [one for testing](https://github.com/Foggalong/shiny-decision-trees/blob/master/test-data.csv) (the remaining 30% of entries).


## License
The `app.R` file is released under an MIT License and provided without warranty.


## References
- [\[1\]](https://socialsciences.mcmaster.ca/jfox/Teaching-with-R.pdf) Fox, John & Andersen, Robert (2005), "Using the R Statistical Computing Environment to Teach Social Statistics Courses". Department of Sociology, McMaster University.
- [\[2\]](https://www.nytimes.com/2009/01/07/technology/business-computing/07program.html) Vance, Ashlee (2009). "Data Analysts Captivated by R's Power". New York Times.
- [\[3\]](http://archive.ics.uci.edu/ml): Dua, Dheeru and Graff, Casey (2017), UCI Machine Learning Repository, University of California, Irvine, School of Information and Computer Sciences.
