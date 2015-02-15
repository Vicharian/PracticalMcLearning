# Project : Practical Machine Learning

These are the various stages of analysis

## Exploratory Analysis

The data is voluminious but it is for a small group of users and for a few instances of use.

After the exploratory analysis it is did not appear that it would make sense to impute most of the missing data. The plots were created in one shot and examined in just the finder preview

```{r}
  for (i in 1:ncol(x)) {
    png(paste0("Plot",i,".png"), height=480, width=480)
    print(qplot(data=x,x[,i],xlab=names(x)[i]))
    dev.off()
  }
```
The clean-up and feature reduction entailed

  * Dropping columns with needless factor
  * Dropping features with less than 5% data
  * Dropping features with more than 95% NA
  * Remove features that were more than 0.7 correlated

```{r}
# Clean it up a bit
all=all[,-which(names(all) %in% c('X','cvtd_timestamp'))]   # remove the serial # and useless factor variables
all=all[,colSums(!is.na(all))/nrow(all) > .95]              # remove columns with high NA
all=all[,colSums(all!="")/nrow(all) > .95]                  # remove columns with high blanks
cm=cor(all[,!sapply(all,is.factor)])                        # find correlation between features
all=all[,-findCorrelation(cm,.70,verbose = F)]              # remove correlated columns
```

## Fitting models


## Prediction

## Conclusion

This is [on GitHub](https://github.com/jbt/markdown-editor) so let me know if I've b0rked it somewhere.

## Project

These are the four plots of the deliverable

### Plot 1

![plot 1](figure/plot1.png)


```{r}
  for (i in 1:ncol(x)) {
    png(paste0("Plot",i,".png"), height=480, width=480)
    print(qplot(data=x,x[,i],xlab=names(x)[i]))
    dev.off()
  }
```

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r}
summary(cars)
```

You can also embed plots, for example:

```{r}
plot(cars)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
