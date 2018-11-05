question 2
==========

rna\_counts &lt;- read.csv("~/Desktop/eXpress\_dm\_counts.csv", h = T)

dim(rna\_counts)

mean\_function &lt;- function (x=rna\_counts, y=FALSE) { if (y == TRUE)
{ x &lt;- (log2(x+0.00000001))} return(mean(x))}

mean\_function(rna\_counts\[,2\])  
mean\_function(rna\_counts\[,2\], y = TRUE)

question 3
==========

dim\_data &lt;- ncol(rna\_counts) stored\_means &lt;- rep(0,
dim\_data-1)

for (i in 1:dim\_data) { stored\_means\[i-1\] &lt;-
mean\_function(rna\_counts\[,i\], y=FALSE)
print(colnames(rna\_counts\[i\])) print(stored\_means\[i-1\]) }

question 4
==========

sapply(rna\_counts\[,2:56\], mean\_function)

TimeTest2 &lt;- sapply(rna\_counts\[,2:56\], mean\_function)

TimeTest1 &lt;- for (i in 1:dim\_data) { stored\_means\[i-1\] &lt;-
mean\_function(rna\_counts\[,i\], y=FALSE)
print(colnames(rna\_counts\[i\])) print(stored\_means\[i-1\]) }

library(microbenchmark) microbenchmark(TimeTest2)
microbenchmark(TimeTest1) \#The sapply function (question 4) ran faster
than the for loop (question 3)

question 5
==========

colMeans(rna\_counts\[,2:56\])

question 6
==========

nrow(rna\_counts) stored\_gene\_means &lt;-
rowMeans(rna\_counts\[,2:56\]) print(stored\_gene\_means)

question 7
==========

lg\_male &lt;- subset(rna\_counts, select = (grepl("lg\_male\_hdhorn*",
names(rna\_counts)))) sm\_male &lt;- subset(rna\_counts, select =
(grepl("sm\_male\_hdhorn*", names(rna\_counts))))

gene\_difference &lt;- function (x) { sm\_male &lt;- subset(x, select =
(grepl("sm\_male\_hdhorn*", names(x)))) small\_mean &lt;-
rowMeans(sm\_male) lg\_male &lt;- subset(rna\_counts, select =
(grepl("lg\_male\_hdhorn*", names(x)))) large\_mean &lt;-
rowMeans(lg\_male) gene\_difference &lt;- large\_mean - small\_mean }
difference &lt;- gene\_difference(rna\_counts) print(difference)

question 8
==========

library(ggplot2) theme\_update(plot.title = element\_text(hjust = 0.5))

ggplot(rna\_counts, aes(x=stored\_gene\_means, y=difference)) +
geom\_point() + xlab("Mean Gene Expression") + ylab("Mean Gene
Expression Difference") + ggtitle("Mean gene expression vs mean gene
difference in large and small male Drosophila headhorns")

ggplot(rna\_counts, aes(x=stored\_gene\_means, y=difference)) +
geom\_point() + xlab("Mean Gene Expression") + ylab("Mean Gene
Expression Difference") + scale\_x\_continuous(trans='log2') +
scale\_y\_continuous(trans='log2') + ggtitle("Transformed mean gene
expression vs mean gene difference in large and small male Drosophila
melanogaster headhorns")
