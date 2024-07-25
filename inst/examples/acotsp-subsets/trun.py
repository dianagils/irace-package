#!/usr/bin/python3

import subprocess
import sys
import os
import re
import glob

def error(message):
    print(f"{os.popen('date').read().strip()}: error: {message}", file=sys.stderr)
    sys.exit(1)

def get_optimum_distance(instance_name, distances_file):
    try:
        with open(distances_file, 'r') as file:
            for line in file:
                if f"{instance_name}:" in line:
                    return int(line.split(':')[1].strip())
    except Exception as e:
        error(str(e))

# Path to the ACOTSP executable
EXE = "../src/ACOTSP-1.03/acotsp"

# Fixed parameters for ACOTSP
FIXED_PARAMS = " --tries 1 --tours 1000 --quiet "

DISTANCES_FILE = "../optimum-and-bestknown-RUE.txt"

CONFIG_ID = sys.argv[1]
INSTANCE_ID = sys.argv[2]
SEED = sys.argv[3]
INSTANCE = sys.argv[4]
CONFIG_PARAMS = sys.argv[5:]

STDOUT = f"c{CONFIG_ID}-{INSTANCE_ID}-{SEED}.stdout"
STDERR = f"c{CONFIG_ID}-{INSTANCE_ID}-{SEED}.stderr"

INSTANCE_NAME = os.path.split(INSTANCE)[-1]

if not os.path.exists(EXE) or not os.access(EXE, os.X_OK):
    error(f"{EXE}: not found or not executable (pwd: {os.getcwd()})")

# Build the command line for ACOTSP
command = f"{EXE} {FIXED_PARAMS} -i {INSTANCE} --seed {SEED} {' '.join(CONFIG_PARAMS)}"

# Run ACOTSP
try:
    with open(STDOUT, 'w') as stdout_file, open(STDERR, 'w') as stderr_file:
        subprocess.run(command, shell=True, stdout=stdout_file, stderr=stderr_file, check=True)
except subprocess.CalledProcessError as e:
    error(str(e))

# Check if the output file exists
if not os.path.isfile(STDOUT) or os.path.getsize(STDOUT) == 0:
    error(f"{STDOUT}: No such file or directory")

# Read the output file and extract the best objective value
try:
    with open(STDOUT, 'r') as file:
    	output = file.read()
    # Use regular expression to find the pattern 'Best [-+0-9.e]+'
    	match = re.search(r'Best ([-+0-9.e]+)', output)
    	if match:
    # Extract the second group from the match
    		cost = int(match.group(1))
    	else:
    		error("Pattern not found in the output")
except Exception as e:
    error(str(e))

#ls ../tsp-opt/
#1000.opt  1500.opt  2000.opt  2500.opt  3000.opt


# Calculate the optimum distance
#optimum_distance = get_optimum_distance(INSTANCE_NAME, DISTANCES_FILE)

# Subtract the optimum distance from the cost
#cost -= optimum_distance
#cost = 100*(abs(optimum_distance - float(cost)))/optimum_distance
# Print the result
print(float(cost))

# Clean up files
os.remove(STDOUT)
os.remove(STDERR)
for pattern in ["best.*", "stat.*", "cmp.*"]:
    files = glob.glob(pattern)
    for file in files:
        if os.path.exists(file):
            os.remove(file)
