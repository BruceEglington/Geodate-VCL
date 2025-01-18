# Calculate Wasserstein distance between two mineral age distributions. 

Code to accompany the _Geochronology_ manuscript, [Lipp & Vermeesch (2023)](https://egusphere.copernicus.org/preprints/2022/egusphere-2022-1200/).
 
Scripts provided in `R` and `python` to calculate the 2nd Wasserstein distance, $W_2$, between the Byskealven and Vefsna age distributions from [Morton et al. 2008](https://doi.org/10.1016/j.sedgeo.2008.07.001) (see Figure 1 in manuscript.) It makes use of the [`POT`](https://pythonot.github.io/) and [`transport`](https://cran.r-project.org/web/packages/transport/index.html) packages. The scripts can be run from command line using `python W2.py` or `Rscript W2.R` and should return: `W2 = 490.0071914467088` 

The file `wobus.csv` contains the Ar-Ar ages from [Wobus et al. 2003](https://pubs.geoscienceworld.org/gsa/geology/article-abstract/31/10/861/29139/has-focused-denudation-sustained-active-thrusting), used in the `IsoplotR` example in the manuscript.
