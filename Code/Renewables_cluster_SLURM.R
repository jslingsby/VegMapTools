##############################################################################
######## Bits of code to run the Cape Point climate estimate analysis on the Davis cluster (farm)
##############################################################################

###send to home/ on farm
#Spatial data
scp -r /Users/jasper/Documents/GIS/VegToolsRaw/Rasters.zip slingsby@agri.cse.ucdavis.edu:/home/slingsby/Rasters.zip

#Just the Carbon data
scp -r /Users/jasper/Documents/GIS/VegToolsRaw/Rasters/TotalEcosystemOrganicCarbon_g_C_sq.m_DEA_CSIR_1.1.9-2015-10-06.tif slingsby@agri.cse.ucdavis.edu:/home/slingsby/Rasters/TotalEcosystemOrganicCarbon_g_C_sq.m_DEA_CSIR_1.1.9-2015-10-06.tif

scp -r /Users/jasper/Documents/GIS/VegToolsRaw/Rasters/TotalEcosystemOrganicCarbon_g_C_sq.m_DEA_CSIR_1.1.9-2015-10-06.sld slingsby@agri.cse.ucdavis.edu:/home/slingsby/Rasters/TotalEcosystemOrganicCarbon_g_C_sq.m_DEA_CSIR_1.1.9-2015-10-06.sld

#Temp code and batch script (dnorm)
scp -r /Users/jasper/GIT/VegMapTools/Code/Renewables_cluster.R slingsby@agri.cse.ucdavis.edu:/home/slingsby/Renewables_cluster.R

scp -r /Users/jasper/GIT/VegMapTools/Code/Renewables_cluster.sh slingsby@agri.cse.ucdavis.edu:/home/slingsby/Renewables_cluster.sh


###ssh into farm
ssh slingsby@agri.cse.ucdavis.edu

###set batch running
sbatch Renewables_cluster.sh

###check if job is running
squeue --jobs 10240810 (i.e. job number)

###check all jobs running
squeue

#to kill job - scancel 10230477

###Look at R output so far
cat *.Rout

###check size of directory (i.e. if output is being written)
cd to directory
du -sh
ls

###retrieve files and save locally - run from terminal
exit 

#Retrieve data 
scp -r slingsby@agri.cse.ucdavis.edu:/home/slingsby/RE.Rdata /Users/jasper/GIT/VegMapTools/Data/RE.Rdata

##delete files (once ssh’d into farm)
rm filename
rm *.txt #for wildcard delete
rmdir #for removing empty directories
rmdir -r #for recirsive removal of directory and contents