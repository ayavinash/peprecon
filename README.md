PepRecon can be usilized to visualize and evaluate the performance of a liquid chromatography coupled to electrospray tandem mass spectrometry (LC/ESI-MS/MS) experiment. The tool provides a visual summary of the Mascot database search result for an experiment. The summary includes a scatter-plot of precursor ions classified on their expectation scores in m/z and elution time dimensions, information on Mascot scores in m/z and elution time dimensions, distribution of charge states of all precursor ions in these dimensions, and statistics such as the good/poor peptide identification ratio and average mass deviation for an experiment. The performance of gas-phase fractionation experiments is increased by obtaining the optimal m/z scan regions to increase proteome coverage. The visual information provided by PepRecon can also be useful for devising optimization strategies for the integrated LC/ESI-MS/MS system.


The patch script is to be pasted inside the mascot server result loading cgi file. 
The ms_peprecon.pl parses the mascot dat file
The ms_compositeplot.r genrates the peprecon composite plot which is served back
to the user.

