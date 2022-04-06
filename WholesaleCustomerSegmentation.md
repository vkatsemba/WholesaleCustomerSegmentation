Wholesale Customer Segmentation
================
Vadim Katsemba
4/1/2022

# The data set refers to clients of a wholesale distributor. It includes the annual spending in monetary units (m.u.) on diverse product categories

Load libraries

``` r
library(dplyr)
library(ggplot2)
library(cluster)
library(gridExtra)
```

Read the
dataset

``` r
data <- read.csv("Wholesale customers data.csv", stringsAsFactors = FALSE)
```

Explore the dataset for names of variables, structure and get the first
and last rows of the dataset

``` r
names(data)
```

    ## [1] "Channel"          "Region"           "Fresh"           
    ## [4] "Milk"             "Grocery"          "Frozen"          
    ## [7] "Detergents_Paper" "Delicassen"

``` r
str(data)
```

    ## 'data.frame':    440 obs. of  8 variables:
    ##  $ Channel         : int  2 2 2 1 2 2 2 2 1 2 ...
    ##  $ Region          : int  3 3 3 3 3 3 3 3 3 3 ...
    ##  $ Fresh           : int  12669 7057 6353 13265 22615 9413 12126 7579 5963 6006 ...
    ##  $ Milk            : int  9656 9810 8808 1196 5410 8259 3199 4956 3648 11093 ...
    ##  $ Grocery         : int  7561 9568 7684 4221 7198 5126 6975 9426 6192 18881 ...
    ##  $ Frozen          : int  214 1762 2405 6404 3915 666 480 1669 425 1159 ...
    ##  $ Detergents_Paper: int  2674 3293 3516 507 1777 1795 3140 3321 1716 7425 ...
    ##  $ Delicassen      : int  1338 1776 7844 1788 5185 1451 545 2566 750 2098 ...

``` r
head(data)
```

    ##   Channel Region Fresh Milk Grocery Frozen Detergents_Paper Delicassen
    ## 1       2      3 12669 9656    7561    214             2674       1338
    ## 2       2      3  7057 9810    9568   1762             3293       1776
    ## 3       2      3  6353 8808    7684   2405             3516       7844
    ## 4       1      3 13265 1196    4221   6404              507       1788
    ## 5       2      3 22615 5410    7198   3915             1777       5185
    ## 6       2      3  9413 8259    5126    666             1795       1451

``` r
tail(data)
```

    ##     Channel Region Fresh  Milk Grocery Frozen Detergents_Paper Delicassen
    ## 435       1      3 16731  3922    7994    688             2371        838
    ## 436       1      3 29703 12051   16027  13135              182       2204
    ## 437       1      3 39228  1431     764   4510               93       2346
    ## 438       2      3 14531 15488   30243    437            14841       1867
    ## 439       1      3 10290  1981    2232   1038              168       2125
    ## 440       1      3  2787  1698    2510     65              477         52

The variables Channel and Region must be converted to factors as they
have finite values

``` r
data$Channel <- as.factor(data$Channel)
data$Region <- as.factor(data$Region)
```

Convert all the continuous variables to numeric

``` r
data[3:8] <- lapply(data[3:8], as.numeric)
```

Statistical summary of the continuous
    variables

``` r
summary(data[,3:8])
```

    ##      Fresh             Milk          Grocery          Frozen       
    ##  Min.   :     3   Min.   :   55   Min.   :    3   Min.   :   25.0  
    ##  1st Qu.:  3128   1st Qu.: 1533   1st Qu.: 2153   1st Qu.:  742.2  
    ##  Median :  8504   Median : 3627   Median : 4756   Median : 1526.0  
    ##  Mean   : 12000   Mean   : 5796   Mean   : 7951   Mean   : 3071.9  
    ##  3rd Qu.: 16934   3rd Qu.: 7190   3rd Qu.:10656   3rd Qu.: 3554.2  
    ##  Max.   :112151   Max.   :73498   Max.   :92780   Max.   :60869.0  
    ##  Detergents_Paper    Delicassen     
    ##  Min.   :    3.0   Min.   :    3.0  
    ##  1st Qu.:  256.8   1st Qu.:  408.2  
    ##  Median :  816.5   Median :  965.5  
    ##  Mean   : 2881.5   Mean   : 1524.9  
    ##  3rd Qu.: 3922.0   3rd Qu.: 1820.2  
    ##  Max.   :40827.0   Max.   :47943.0

Barplots of the discrete variables

``` r
ggplot(data, aes(x = Channel)) +
  geom_bar(stat = "count", width = 0.5, fill = "steelblue") +
  theme_minimal() +
  labs(title = "Barplot to display Channel Comparison", xlab = "Channel")
```

![](WholesaleCustomerSegmentation_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
ggplot(data, aes(x = Region)) +
  geom_bar(stat = "count", width = 0.5, fill = "steelblue") +
  theme_minimal() +
  labs(title = "Barplot to display Region Comparison", xlab = "Region")
```

![](WholesaleCustomerSegmentation_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

Histograms of the distribution of the continuous
variables

``` r
p1 <- ggplot(data, aes(x= Fresh)) + geom_histogram() + labs(title = "Histogram for Fresh distribution")
p2 <- ggplot(data, aes(x= Milk)) + geom_histogram() + labs(title = "Histogram for Milk distribution")
p3 <- ggplot(data, aes(x= Grocery)) + geom_histogram() + labs(title = "Histogram for Grocery distribution")
p4 <- ggplot(data, aes(x= Frozen)) + geom_histogram() + labs(title = "Histogram for Frozen distribution")
p5 <- ggplot(data, aes(x= Detergents_Paper)) + geom_histogram() + labs(title = "Histogram for Detegerents Paper distribution")
p6 <- ggplot(data, aes(x= Delicassen)) + geom_histogram() + labs(title = "Histogram for Delicassen distribution")
grid.arrange(p1, p2, p3, p4, p5, p6, nrow=2)
```

![](WholesaleCustomerSegmentation_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

Given the skewed distributions, we shall perform a logarithmic
transformation of all of the continuous variables

``` r
data[,3:8] <- log(data[,3:8])
```

Hisograms of the continuous variables by
Channel

``` r
p1 <- ggplot(data, aes(x= Fresh, fill = Channel, color = Channel)) + geom_histogram(bins = 10, position = "identity", alpha = 0.5)
p2 <- ggplot(data, aes(x= Milk, fill = Channel, color = Channel)) + geom_histogram(bins = 10, position = "identity", alpha = 0.5)
p3 <- ggplot(data, aes(x= Grocery, fill = Channel, color = Channel)) + geom_histogram(bins = 10, position = "identity", alpha = 0.5)
p4 <- ggplot(data, aes(x= Frozen, fill = Channel, color = Channel)) + geom_histogram(bins = 10, position = "identity", alpha = 0.5)
p5 <- ggplot(data, aes(x= Detergents_Paper, fill = Channel, color = Channel)) + geom_histogram(bins = 10, position = "identity", alpha = 0.5)
p6 <- ggplot(data, aes(x= Delicassen, fill = Channel, color = Channel)) + geom_histogram(bins = 10, position = "identity", alpha = 0.5)
grid.arrange(p1, p2, p3, p4, p5, p6, nrow=3)
```

![](WholesaleCustomerSegmentation_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Density plots of the continuous
variables

``` r
p1 <- ggplot(data, aes(x = Fresh)) + geom_density(fill = "blue") + labs(title = "Fresh Density")
p2 <- ggplot(data, aes(x = Milk)) + geom_density(fill = "blue") + labs(title = "Milk Density")
p3 <- ggplot(data, aes(x = Grocery)) + geom_density(fill = "blue") + labs(title = "Grocery Density")
p4 <- ggplot(data, aes(x = Frozen)) + geom_density(fill = "blue") + labs(title = "Frozen Density")
p5 <- ggplot(data, aes(x = Detergents_Paper)) + geom_density(fill = "blue") + labs(title = "Detergents Paper Density")
p6 <- ggplot(data, aes(x = Delicassen)) + geom_density(fill = "blue") + labs(title = "Delicassen Density")
grid.arrange(p1, p2, p3, p4, p5, p6, nrow=3)
```

![](WholesaleCustomerSegmentation_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
The distributions may not have all become normalized, but are not as
heavily skewed as before.

Boxplots of the continuous variables by Region and
Channel

``` r
p1 <- ggplot(data, aes(x = Region, y= Fresh, fill = Channel)) + geom_boxplot() + labs(title = "Fresh Boxplot")
p2 <- ggplot(data, aes(x = Region, y= Milk, fill = Channel)) + geom_boxplot() + labs(title = "Milk Boxplot")
p3 <- ggplot(data, aes(x = Region, y= Grocery, fill = Channel)) + geom_boxplot() + labs(title = "Grocery Boxplot")
p4 <- ggplot(data, aes(x = Region, y= Frozen, fill = Channel)) + geom_boxplot() + labs(title = "Frozen Boxplot")
p5 <- ggplot(data, aes(x = Region, y= Detergents_Paper, fill = Channel)) + geom_boxplot() + labs(title = "Detergents Paper Boxplot")
p6 <- ggplot(data, aes(x = Region, y= Delicassen, fill = Channel)) + geom_boxplot() + labs(title = "Delicassen Boxplot")
grid.arrange(p1, p2, p3, p4, p5, p6, nrow=3)
```

![](WholesaleCustomerSegmentation_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

Get the optimal number of clusters using the Gap statistic method

``` r
set.seed(123)
stat_gap <- clusGap(data[, 3:8], FUN = kmeans, nstart = 25, K.max = 10, B = 50)
```

    ## Warning: did not converge in 10 iterations

``` r
plot(stat_gap)
```

![](WholesaleCustomerSegmentation_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

Find the optimal number of clusters using the elbow method

``` r
set.seed(123)
k.max <- 15
wss <- sapply(1:k.max, function(k){kmeans(data[,3:8], k, nstart=50,iter.max = 15 )$tot.withinss})
wss
```

    ##  [1] 4800.919 3314.947 2807.248 2489.194 2283.392 2093.718 1963.929
    ##  [8] 1833.298 1747.028 1656.294 1578.267 1504.479 1437.381 1380.279
    ## [15] 1329.492

``` r
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
```

![](WholesaleCustomerSegmentation_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

Using k = 2 from the Gap statistic method, create a k-means clustering
model

``` r
k2 <- kmeans(data[, 3:8], 2, iter.max = 100, nstart = 50, algorithm = "Lloyd")
k2
```

    ## K-means clustering with 2 clusters of sizes 182, 258
    ## 
    ## Cluster means:
    ##      Fresh     Milk  Grocery   Frozen Detergents_Paper Delicassen
    ## 1 8.260679 8.921031 9.384720 6.750228         8.431986   6.817742
    ## 2 9.061999 7.556718 7.775563 7.690204         5.624831   6.557479
    ## 
    ## Clustering vector:
    ##   [1] 1 1 1 2 1 1 1 1 1 1 1 2 1 1 1 2 1 2 1 1 1 2 2 1 1 1 2 2 1 2 1 2 2 2 2
    ##  [36] 1 2 1 1 2 2 1 1 1 1 1 1 1 1 1 2 1 1 1 2 1 1 1 2 1 1 1 1 1 2 1 1 1 2 2
    ##  [71] 2 1 2 2 1 2 2 1 2 1 2 1 1 2 1 1 1 2 2 2 2 2 1 2 1 1 1 2 2 2 1 1 1 2 2
    ## [106] 2 1 1 1 1 2 1 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 1 1 2 2 2 2 2 2 2 1 1 2 2
    ## [141] 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 1 1 2 1 1 1 2 2 1 1 1 1 1 2 2 1 1 1 1 2
    ## [176] 1 2 2 2 2 1 1 1 2 2 2 2 1 1 1 2 2 2 1 2 2 2 1 2 2 1 1 2 2 2 1 2 1 1 1
    ## [211] 2 1 2 1 1 1 1 2 1 2 2 1 2 2 2 2 1 2 2 2 2 1 2 2 2 1 2 2 2 2 2 2 2 1 1
    ## [246] 1 2 2 2 2 2 1 2 2 1 2 2 2 2 2 2 2 2 1 1 1 1 2 1 2 2 2 2 2 2 2 2 2 2 1
    ## [281] 2 1 2 2 2 2 2 2 2 2 2 2 2 1 2 1 2 1 1 2 1 1 1 1 1 1 1 2 2 1 2 2 1 2 2
    ## [316] 1 2 1 2 1 2 2 2 1 2 2 2 2 2 2 2 1 2 1 2 1 2 2 2 2 1 1 1 1 2 1 1 1 2 1
    ## [351] 2 1 2 1 2 2 2 1 2 2 2 2 2 1 2 1 2 2 2 2 2 2 2 1 2 2 1 2 2 1 2 2 1 2 2
    ## [386] 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 1 2 2 2 1 1 1 1 2 1 2 2 1 1 1 1 1
    ## [421] 1 1 2 1 1 2 1 2 2 2 2 2 2 2 1 2 2 1 2 2
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1] 1401.941 1913.006
    ##  (between_SS / total_SS =  31.0 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"    
    ## [5] "tot.withinss" "betweenss"    "size"         "iter"        
    ## [9] "ifault"

``` r
clusplot(data, k2$cluster, color=TRUE, shade=TRUE, labels=0, lines=0)
```

![](WholesaleCustomerSegmentation_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

Using k = 3 from the elbow method, create a k-means clustering
model

``` r
k3 <- kmeans(data[, 3:8], 3, iter.max = 100, nstart = 50, algorithm = "Lloyd")
k3
```

    ## K-means clustering with 3 clusters of sizes 75, 216, 149
    ## 
    ## Cluster means:
    ##      Fresh     Milk  Grocery   Frozen Detergents_Paper Delicassen
    ## 1 6.827410 8.586721 9.187840 5.831026         8.209773   5.725304
    ## 2 8.991553 7.402535 7.624570 7.569569         5.430247   6.441322
    ## 3 9.310122 8.928249 9.249122 7.652753         8.034640   7.462652
    ## 
    ## Clustering vector:
    ##   [1] 3 3 3 2 3 3 3 3 3 3 3 2 3 3 3 2 1 2 3 3 3 2 3 3 3 1 2 2 3 2 3 2 2 2 2
    ##  [36] 1 3 3 1 2 3 3 1 1 1 3 3 3 3 3 2 1 3 1 2 3 3 1 2 1 1 3 3 3 2 1 1 3 3 2
    ##  [71] 2 3 2 3 3 2 2 3 2 1 2 1 3 2 3 3 3 3 2 3 2 2 3 2 1 1 1 2 2 2 3 3 3 3 2
    ## [106] 2 1 3 1 1 2 3 2 2 2 2 2 2 2 2 2 2 2 3 2 3 2 3 1 2 2 2 2 2 2 2 3 1 2 2
    ## [141] 3 2 2 2 2 3 2 2 2 2 2 2 2 2 2 1 3 2 3 1 3 2 2 3 3 3 3 1 2 2 1 1 1 1 2
    ## [176] 1 3 2 2 2 3 3 1 3 1 2 2 1 3 1 2 2 2 1 2 2 3 3 2 2 3 3 3 1 2 1 2 1 1 3
    ## [211] 2 3 2 3 1 3 1 2 1 2 2 1 2 2 2 2 3 2 1 2 3 1 2 1 2 1 2 2 2 3 2 2 2 3 3
    ## [246] 1 2 2 2 2 2 3 2 3 3 2 2 3 3 3 2 2 2 3 1 3 1 2 3 2 2 2 2 2 2 2 3 2 2 3
    ## [281] 2 3 3 2 3 2 2 2 2 2 2 2 2 3 3 1 2 3 3 1 3 3 1 1 1 1 3 2 2 1 2 2 1 2 3
    ## [316] 3 2 1 2 3 2 2 2 3 2 3 2 2 2 2 2 3 2 1 2 3 2 2 2 2 1 3 1 1 2 1 3 3 2 3
    ## [351] 2 3 2 1 3 1 2 1 3 2 2 2 2 1 2 3 2 2 2 2 3 2 2 3 2 2 3 2 2 1 2 2 3 2 3
    ## [386] 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 3 2 3 3 3 3 3 2 1 3 2 3 3 3 1 3
    ## [421] 1 3 2 3 3 2 3 3 2 2 2 3 2 2 3 3 2 3 2 1
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1]  711.6976 1354.2249  741.3352
    ##  (between_SS / total_SS =  41.5 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"    
    ## [5] "tot.withinss" "betweenss"    "size"         "iter"        
    ## [9] "ifault"

``` r
clusplot(data, k3$cluster, color=TRUE, shade=TRUE, labels=0, lines=0)
```

![](WholesaleCustomerSegmentation_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

Perform PCA and apply it to the dataset

``` r
pcclust <- prcomp(data[, 3:8], scale = FALSE)
summary(pcclust)
```

    ## Importance of components:
    ##                           PC1    PC2    PC3     PC4     PC5     PC6
    ## Standard deviation     2.1995 1.7391 1.1272 1.02557 0.70739 0.50095
    ## Proportion of Variance 0.4424 0.2766 0.1162 0.09618 0.04576 0.02295
    ## Cumulative Proportion  0.4424 0.7189 0.8351 0.93130 0.97705 1.00000

``` r
pcclust$rotation[, 1:2]
```

    ##                         PC1        PC2
    ## Fresh            -0.1737170 0.68513571
    ## Milk              0.3944630 0.16239926
    ## Grocery           0.4543636 0.06937908
    ## Frozen           -0.1721960 0.48769100
    ## Detergents_Paper  0.7455150 0.04191162
    ## Delicassen        0.1494356 0.50970874

Plot the clusters for k = 2

``` r
set.seed(123)

ggplot(data, aes(x = Detergents_Paper , y = Fresh)) + 
  geom_point(stat = "identity", aes(color = as.factor(k2$cluster))) +
  scale_color_discrete(name = " ", 
                       breaks=c("1", "2"),
                       labels=c("Cluster 1", "Cluster 2")) +
  ggtitle("Segments of Wholesale Customers", 
          subtitle = "Using K-means Clustering")
```

![](WholesaleCustomerSegmentation_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

Plot the clusters for k = 3

``` r
set.seed(123)

ggplot(data, aes(x = Fresh, y = Detergents_Paper)) + 
  geom_point(stat = "identity", aes(color = as.factor(k3$cluster))) +
  scale_color_discrete(name = " ", 
                       breaks=c("1", "2", "3"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3")) +
  ggtitle("Segments of Wholesale Customers", 
          subtitle = "Using K-means Clustering")
```

![](WholesaleCustomerSegmentation_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->
