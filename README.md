You can run this code with RStudio.

This code is based on packages ‘plotly’, ‘shiny’ and ‘ggplot2’. If you haven’t install these packages, you can use the following code to install.

```
install.packages(c("plotly","ggplot2", "shiny","bslib"))
```

Then open the file 'plot/app.R'. Firstly you are supposed to set the path to the document 'DSAA5024_Group03'. You can use the following code to change the path.

```
setwd('.../DSAA5024_Group03/')
```

where ... represents where you store our document.

Then you can run the code with two methods. You can run 

```
shiny::runApp('plot')
```

directly or click the 'Run App' button in the upper-right corner. Then you can see our visualization system.

If there is an error at the bar plot position, you can click the refresh button in the upper-left corner. This will fix the problem.

Then you can choose what you care about on the left and the four plots will change accordingly.
