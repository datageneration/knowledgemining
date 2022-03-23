* Install mrobust package
net install mrobust, from(http://fmwww.bc.edu/RePEc/bocode/m)
* Load TEDS dataset for exercise 
use "https://github.com/karlho/knowledgemining/blob/main/data/TEDS2016.dta?raw=true"
* Run mrobust using logit model 
mrobust logit votedpp_1 Taiwanese Econ_worse Independence Unification  KMT DPP age edu female
