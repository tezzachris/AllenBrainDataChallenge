### Data description
* Source: The Allen Brain Observatory (Allen Institute MindScope Program, 2016)

Data description: The neuronal activity in the mouse visual cortex in response to a range of visual stimuli. Here we focus on a single area (primary visual cortex), and on three different depths (200 μm, 275 μm, 375 μm).

The visual stimuli displayed in each session are:
• session A: drifting gratings, natural movie 1, natural movie 3;
• session B: static gratings, natural images, natural movie 1;
• session C2: locally sparse noise, natural movie 1, natural movie 2


# cells.Rdata
The data frame cells.Rdata contains the identifier of each neuron, its depth and the experimental sessions in which its activity has been recorded. The table contains the following variables:
cell_id : identifier of the neuron;
depth : depth of the neuron;
A : Boolean, whether the neuron’s trace is present in session A; B : Boolean, whether the neuron’s trace is present in session B; C : Boolean, whether the neuron’s trace is present in session C.

# sessionA.Rdata, sessionB.Rdata, sessionC.Rdata
The data frames session#.Rdata contain the calcium traces for the targeted neurons during the chosen session. Traces are recorded at a frequency of 30Hz. Columns indicate the cells (cell_id), rows corresponds to time (frames).
sessionA.Rdata has 47 columns and 115481 rows. sessionB.Rdata has 44 columns and 113854 rows. sessionC.Rdata has 39 columns and 124060 rows.

# stimuliA.Rdata, stimuliB.Rdata, stimuliC.Rdata
The vectors contain information on the type of visual stimulus displayed in each frame for the three sessions (strings).

# dfa.mat 
Example of inputs for running the Kalman_filter.m optimization routine.


# References:

DRC Data Overview report (2022). 
Allen Brain Observatory (2017). Technical whitepaper: stimulus set and response analyses. URL: help.brain-map.org/display/observatory/Documentation.  
Allen Institute MindScope Program (2016). Allen Brain Observatory – 2-photon visual coding [dataset]. brain-map.org/explore/circuits.
