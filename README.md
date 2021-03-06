# Corvides-SSE
RevBayes code used in McCullough et al. 2022, "Wallacean and Melanesian Islands Promote Higher Rates of Diversification within the Global Passerine radiation Corvides" 

See link for publisher's website: 
https://academic.oup.com/sysbio/advance-article-abstract/doi/10.1093/sysbio/syac044/6608712?redirectedFrom=fulltext


This repository holds RevBayes code for the six analyses: 

1. GeoHiSSE islands v. continents 
2. extended GeoHiSSE islands v. continents 
3. GeoHiSSE East v. West Wallace's Line
4. extended GeoHiSSE East v. West Wallace's Line
5. 4-state MuSSE (combined islands v. continents and Wallace's Line) 
6. 4-state MuHiSSE (combined islands v. continents and Wallace's Line) 

Each of these folders has the data and revbayes script that we used in our paper. I also included the combined and resampled log files for each of these analyses, as well as the code needed to plot the figures presented in the paper. 



Methods for this study relating to this repository are below: 

To assess how geographic barriers influence diversification within Corvides, we fit eight different state-dependent speciation and extinction models (Maddison et al. 2007) in Revbayes (Höhna et al. 2016). State-dependent Speciation and Extinction (SSE) models estimate per-lineage speciation and extinction rate parameters specific to particular traits, such as geography. We assessed how diversification within Corvides is best explained by in situ diversification within island/continental systems, crossing Wallace’s line, a combination of both, or an unmeasured trait. 

Generally, we tested two traits: island life and dispersal across Wallace’s Line (Wallace 1863). We categorized islands as those that were either of volcanic origin (i.e., Solomon Islands, Mascarene Islands) or continental drift islands that had separated for the duration of the crown age of Corvides to present-day (i.e., Greater Antilles, Madagascar, New Zealand, and New Caledonia). In terms of evolutionary dynamics of Corvides, these continental fragments are effectively isolated from other landmasses. However, we considered continental plate islands (land-bridge) islands as “continents;” this type of island was connected with the mainland by a land-bridge during times of low sea level during the Pleistocene, thus allowing free movement between landmasses (i.e., New Guinea to Australia, Greater Sundas to mainland southeast Asia). Our second trait was whether a taxon was present east or west of Wallace’s Line (Figure S2); taxa that are present on either side of Wallace’s Line were considered “widespread.” 

First, we implemented GeoHiSSE (Caetano et al. 2018), which is a model extension of geographic state-dependent speciation and extinction (GeoSSE; (Goldberg et al. 2011)) that incorporates a hidden state to add the possibility of diversification changes that due to unmeasured factors other than geography(HiSSE; (Beaulieu and O’Meara 2016)). Incorporation of a hidden state ameliorates problems with type I error that have been widely documented for SSE models, including GeoSSE (Alves et al. 2017) by redefining the null hypothesis to be one that has more complexity in diversification. We defined three main states where extant Corvides taxa are found as either continent (state 0 as defined by figure S2), island (1), or both (i.e., widespread, state 01). For each of the states, we extended them to include a hidden state (denoted by A or B) to allow the data to be uncertain to test if diversification within Corvides is not associated with our measured geographic trait. We added anagenetic speciation (d0A, d01A, d1A, d0B, d01B, and d1B) that describes dispersal from one state to the other, such as a continental taxon dispersing to an island without a speciation event. We incorporated cladogenetic speciation, which includes within region (s0A, s1A, s0B, and s1B) speciation events. We allowed for extinction/extirpation (X0A, X1A, X0B, X1B) within endemic regions. Following Caetano et al. (2018), we did not allow for in situ speciation (s) or extirpation (X) for “widespread” taxa or dispersal (d) directly between endemic regions (i.e., 0 to 01 to 1). In addition, we extended the GeoHiSSE model (referred to “extended GeoHiSSE”) of Caetano et al. (2018) to allow separate parameter estimations of the three types of cladogenetic range evolution that it models: allopatric cladogenesis, widespread sympatry, and subset sympatry (Figure S3). 

To explore how the combined effect of both biogeographic barriers (insularity and Wallace’s Line) affect diversification within Corvides, we also implemented the Multistate Speciation and Extinction model with a hidden state in Revbayes. We developed MuSSE and MuHiSSE models to infer diversification within four states: islands east of Wallace’s Line (0), continents east of Wallace’s Line (1), islands west of Wallace’s Line (2), and continents west of Wallace’s Line (3). We allowed for in situ speciation (s), anagenetic dispersal between regions (d), and extirpation (X). For each analysis, we ran between five and 16 independent chains to speed up computational time; total generations varied from 540 million to one billion (Table S7). After assessing convergence (with ESS values >200), we combined posterior distributions LogCombiner v.1.10.4 (Drummond and Rambaut 2007) with 25% burnin. 








