#!/bin/bash

#SBATCH -J clean_pudf                    # Job name
#SBATCH -o clean_pudf.%j.o               # Name of stdout output file (%j expands to jobId)
#SBATCH -e clean_pudf.%j.e               # Name of stderr output file (%j expands to jobId)
#SBATCH -p normal                        # Queue name, small is for <=2 nodes, normal 3+
#SBATCH -N 2                  	         # Total number of nodes requested
#SBATCH -n 34                            # Total number of tasks to run, Frontera was 56 cores/node (28 per socket)
#SBATCH -t 48:00:00            	         # Run time (hh:mm:ss)
#SBATCH -A IBN24016                      # Allocation name
#SBATCH --mail-user=emjavan@utexas.edu   # Email for notifications
#SBATCH --mail-type=all                  # Type of notifications, begin, end, fail, all

# Load newest R module on Frontera
module load tacc-apptainer

# Load launcher
module load launcher

# Configure launcher
EXECUTABLE=$TACC_LAUNCHER_DIR/init_launcher
PRUN=$TACC_LAUNCHER_DIR/paramrun
CONTROL_FILE=commands_run_estmiation_fns.txt
export LAUNCHER_JOB_FILE=commands_run_estmiation_fns.txt
export LAUNCHER_WORKDIR=`pwd`
export LAUNCHER_SCHED=interleaved

# Start launcher
$PRUN $EXECUTABLE $CONTROL_FILE
