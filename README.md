Computer Code and Data Set Files Corresponding to:

### "Time Series Methodology for Analyzing Calcium Imaging Data"

Authors:

M. Girardi, marco.girardi.6@phd.unipd.it, University of Padua, Department of Statistical Sciences    
F. Setoudehtanzagi1, f.setoudehtazangi@uq.edu.au , University of Queensland, School of Mathematics & Physics   
F. Spoto federica.spoto@uniroma1.it, Sapienza University of Rome, Department of Statistical Sciences   
C. Tezza christian.tezza@unibo.it, University of Bologna, Department of Statistics   
K. Fokianos fokianos@ucy.ac.cy University of Cyprus, Department of Mathematics & Statistic   


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""    
Data Research Camp (DRC) 2022 - University of Padova   
https://researchcamp2022.stat.unipd.it/    
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""    
   
There are two scripts in the R language:   
DataAnalysis.R  -> data analysis and data processing  
Model.R  -> employ the Hidden Markov Model (HMM) and perfoms the binning of the time series  


The "Kalman_filter.m" is a MATLAB script for obtaning a filtered calcium series given one neuron observed calcium. The filtering relies on a state-space model representation (given in the article) and then performs standard Kalman filtering calculations. It also requires as input the inferred activation states from HMM model. The optimization of the likelihood function is fast and reliable for a neuron. We recommend different starting points for different inputs. 



