#Python single core submission script

#Run with current environment (-V) and in the current directory (-cwd)
#$ -V -cwd

#Request some time- min 15 mins - max 48 hours
#$ -l h_rt=18:00:00

#Request high memory node type
#$ -l node_type=40core-768G 

#Request some memory per core
#$ -l h_vmem=750G

#Get email at start and end of the job
#$ -m be

#Now run the job
module load anaconda
python projs.py
