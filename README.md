# Social Network Analyis
This outlines the steps we have taken to create bipartite projections using COP tweets



1. Create a subset of each COP dataset based on the normative topics extracted from BERTopic
2. Create bipartite network files containing the twitter auther username (source) and topic they interacted with (target) using the `Bipartite.ipynb` script
3. Create user projections of the bipartite network using the `projs.py` script. Due to the sizes of the files, we used the ARC HPC with `job_script1.sh`. 
