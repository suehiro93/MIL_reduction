# Complementarily labeled learning via MIL-reduction
* This repository gives the implementation of the experiment published in [1]. 

1. Daiki Suehiro, Eiji Takimoto, "Simplified and Unified Analysis of Various Learning Problems by Reduction to Multiple-Instance Learning", Proceedings of the 38th Conference on Uncertainty in Artificial Intelligence (UAI 2022), 2022.8, to appear.

---

# Requirement
* R (version 3.4 or later.)
* R Packages: "data.table", "compiler", and "Rcplex."
* Software   
CPLEX provided by IBM.  

To know how to install CPLEX and Rcplex, the following pages may be helpful for you:
* http://www-stat.wharton.upenn.edu/~josezubi/INSTALL
* https://cran.r-project.org/web/packages/Rcplex/INSTALL
* https://cvxr.rbind.io/cvxr_examples/cvxr_using-other-solvers/

---

# Usage

source('demo_MIL4CLL.R')