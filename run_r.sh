#!/bin/bash

echo "running combined rate"
Rscript cross_val_combined_rate.R  >cross_val_combined_rate.txt
echo "running female rate"
Rscript cross_val_female_rate.R   >cross_val_female_rate.txt   
echo "running male rate"
Rscript cross_val_male_rate.R >cross_val_male_rate.txt
echo "running combined  ratio"
Rscript cross_val_combined_ratio.R  >cross_val_combined_ratio.txt  
echo "running female ratio"
Rscript cross_val_female_ratio.R  >cross_val_female_ratio.txt  
echo "running male ratio"
Rscript cross_val_male_ratio.R >cross_val_male_ratio.txt
echo "done"
