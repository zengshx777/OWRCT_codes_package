#!/bin/bash
R CMD BATCH --vanilla '--args h.degree=0 pt=0.5 mis.specified=0 rand.seed=1' Main_RCT_Continuous.R cont_1.out &
R CMD BATCH --vanilla '--args h.degree=0.75 pt=0.5 mis.specified=0 rand.seed=2' Main_RCT_Continuous.R cont_2.out &
R CMD BATCH --vanilla '--args h.degree=0 pt=0.7 mis.specified=0 rand.seed=3' Main_RCT_Continuous.R cont_3.out &
R CMD BATCH --vanilla '--args h.degree=0 pt=0.5 mis.specified=1 rand.seed=4' Main_RCT_Continuous.R cont_4.out &
R CMD BATCH --vanilla '--args h.degree=0 pt=0.7 mis.specified=1 rand.seed=5' Main_RCT_Continuous.R cont_5.out &
R CMD BATCH --vanilla '--args h.degree=0.75 pt=0.7 mis.specified=0 rand.seed=6' Main_RCT_Continuous.R cont_6_out &
R CMD BATCH --vanilla '--args h.degree=0.25 pt=0.5 mis.specified=0 rand.seed=7' Main_RCT_Continuous.R cont_7_out &
R CMD BATCH --vanilla '--args h.degree=0.5 pt=0.5 mis.specified=0 rand.seed=8' Main_RCT_Continuous.R cont_8_out &
R CMD BATCH --vanilla '--args h.degree=0 pt=0.6 mis.specified=0 rand.seed=9' Main_RCT_Continuous.R cont_9_out &


R CMD BATCH --vanilla '--args rare.degree=0 h.degree=0 pt=0.5 mis.specified=0 rand.seed=5' Main_RCT_Binary.R bin_1.out &
R CMD BATCH --vanilla '--args rare.degree=-0.85 h.degree=0 pt=0.5 mis.specified=0 rand.seed=6' Main_RCT_Binary.R bin_2.out &
R CMD BATCH --vanilla '--args rare.degree=0 h.degree=0 pt=0.5 mis.specified=1 rand.seed=7' Main_RCT_Binary.R bin_3.out &
R CMD BATCH --vanilla '--args rare.degree=-0.85 h.degree=0 pt=0.5 mis.specified=1 rand.seed=8' Main_RCT_Binary.R bin_4.out &
R CMD BATCH --vanilla '--args rare.degree=0 h.degree=0 pt=0.7 mis.specified=0 rand.seed=9' Main_RCT_Binary.R bin_5.out &
R CMD BATCH --vanilla '--args rare.degree=0 h.degree=0.75 pt=0.5 mis.specified=0 rand.seed=10' Main_RCT_Binary.R bin_6.out &
R CMD BATCH --vanilla '--args rare.degree=-1.386 h.degree=0 pt=0.5 mis.specified=0 rand.seed=11' Main_RCT_Binary.R bin_7.out &
R CMD BATCH --vanilla '--args rare.degree=-2.197 h.degree=0 pt=0.5 mis.specified=0 rand.seed=12' Main_RCT_Binary.R bin_8.out &
