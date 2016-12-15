Measuring Non-linear Associations
=================================

=================================

This study compared three different measures of association (a probabilistic measure of association called *A*, distance correlation (*dcor*) and Maximal Information Coefficient (*MIC*)) at identifying inter-variable relationships in large biological datasets. The comparisons are based on power simulations of two-way associations using a range of sample sizes, sampling distributions and additive noise levels.

The simulations found that no measure of association had greater statistical power in all cases. Using the gene expression data, *dcor* identified all strong associations identified by $A$ in the two-way case. The analysis suggests that $A$ suffers from some of the same power deficiencies that have been previously identified for *MIC*.

Usage
-----

The project is based on [ProjectTemplate](http://projecttemplate.net/index.html). To load the project run the following commands:

```R
library(ProjectTemplate)
load.project()
```

The following snippet is an example of how to estimate the power of two different measures of association (Pearson and Spearman correlation) for 15 different types of non-linear functions over a range of different noise levels and sample sizes:

```R
#get all function types from the nlf package
types <- ls(getNamespace("nlf"), all.names=F)

#define the association measures to use
measures <- c(r2, spear)
measureNames <- c('Pearson', 'Spearman')

#run the simulation
system.time(res <- estimatePower(types,
                                 measures,
                                 measureNames,
                                 nsim=500,
                                 runif,
                                 noise=3,
                                 numNoise=30,
                                 sizes=c(50, 100, 250, 500)))

#2D scatter plots of power vs noise for all associations and function types
ggplot(res, aes(noiseLevel, power, colour=measure)) +
  geom_line(size=1.1) +
  facet_grid(n~Function)+
  theme(legend.position="bottom")

#an interactive surface
plotSurface(res, "Pearson", "sigmoid")

```

The project depends on the [nlf package](https://github.com/fjro/nlf) which defines the non-linear functions.

Despite using parallel processing and C++ the simulations can be compute intensive depending on the parameters used so caution is advised in choosing the parameters for the $estimatePower$ function.

There is a formal [report](reports/mnla.pdf) documenting the work and also a corresponding notebook.
