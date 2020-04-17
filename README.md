# OW_RCT
 Codebase for OW-RCT paper.
-**1.simulations_main** contains the functions implementing various methods and scripts to run continuous and binary outcomes simulations.
 -**Main_RCT_Continuous.R**: run simulations with continuous outcome.
 -**Main_RCT_Binary.R**: run simulations with binary outcome.
 -**Crude.R**: function implements UNADJ estimator.
 -**IPWC.R**: function implements IPW estimator.
 -**LinearR.R**: function implements LR estimator.
 -**PS_AIPW.R**: function implements AIPW estimator.
 -**OW.R**: function implements OW estimator.
 -**example.R**: simple demo for running simulations.\\
 -**all_jobs.sh**: Bash script to run all simulations.\\
-**simulations_summary** contains the scripts to reproduce the Figures and Tables in the main text.
 -**plot_cont.R**: visualize continuous simulation results, produce Figure 1 in main text.\\
 -**plot_bin.R**: visualize binary simulation results, produce Figure 2,3 in main text.\\
 -**table_produce.R**: summarize all results, produce Table 1 in the main text, Table 1,2,3 in Web Appendix E.\\
-**read_data** contains the script to reproduce results for real data (data source upon requested).
 -**real_data_application.R**:Analyze BestAir study data produce Table 3 in the main text.

 
 Please see Web Appendix D for reproducing the results.
