# Evolution of freshwater crabs in the Lake Malawi/Nyasa catchment

Evolution of freshwater crabs paper

Data and code for Layfield et al. "Ecological speciation in East African freshwater crabs of the genus Arcopotamonautes Bott, 1955 across a lake-river boundary in the Lake Malawi catchment (Potamoidea: Potamonautidae: Potamonatinae)"

***

**Assets**

***

File: params-crabs.txt The ipyrad parameter file (run denovo).

File: crabs.vcf: ipyrad output variant file, 73 specimens including _A.bellarussus_ and outgroup _M.choloensis_

File: crabs_without_Choloensis.vcf: ipyrad output variant file, 72 specimens excluding outgroup _M.choloensis_

File: crabs_analysis1.vcf ipyrad output variant file, 61 specimens including the _A.montivagus_ group I, _A.montivagus_ group II, and _A.orbitospinus_

File: crabs_analysis2.vcf ipyrad output variant file, 33 specimens including the _A.montivagus_ group I and _A.orbitospinus_

File: crabs.snps, ipyrad output SNPs file.

File: raxml_snps_invariant_sites_removed.phylip.treefile containing 73 specimens, generated from SNPs file, invariant sites removed.

File: Analysis_1_crabsplinkPCA.eigenval containing PCA eigenvalues from 61 specimens 

File: Analysis_2_crabsplinkPCA.eigenval containing PCA eigenvalues from 33 specimens 

File: Analysis_2_crabsplink.4.P Plink file containing 33 specimens to compile admixture test in analysis 2

***

**Scripts**

***

ddrad_analysis.txt Script used for population structure

Admixture_and_PCA.R The R code used for the analyses of the population structure

fineRADstructure.txt Script used for FineRADstructure analysis

Morphology.R The R code used for the analyses of the morphology

***

**Notes**

***

Analysis 1 refers to _A.montivagus_ group I vs _A.montivagus_ group II vs _A.orbitospinus_

Analysis 2 refers to _A.montivagus_ group I vs _A.orbitospinus_

