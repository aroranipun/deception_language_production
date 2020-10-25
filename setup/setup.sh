#!/bin/bash

### This script will create a virtual environment based on the requirements.txt located in the setup directory of a the project folder

# Create a virtual environment in the current directory
py -m venv venv

# Activate the virtual environment
if [ -f ./venv/bin/activate ];then
    source ./venv/bin/activate
else
    source ./venv/Scripts/activate
fi

# Upgrade pip 
python -m pip install --upgrade pip

# install from setup/requirements.txt if present
if [ -f ./setup/requirements.txt ]; then
    pip install -r ./setup/requirements.txt
fi

# Install spacy model
#if ![ -f ./venv/Lib/site-packages/en_core_web_sm ]; then
#     python -m spacy download en_core_web_sm
#fi


# Get the finale base directory name, used for naming the Jupyter Kernel
DIR_BASE=$(basename "$PWD")


# Install ipykernel to be able to set up the kernels we need
pip3 install ipykernel

# We will try to install Jupyter Notebook and Jupyterlab, too. (and any dependencies)
pip3 install jupyter notebook jupyterlab

# Install a Jupyter kernel named to correspond with the current directory name
# Note: if there is a name conflict, this kernel will overwrite the existing one
python3 -m ipykernel install --user --name=venv_$DIR_BASE

echo "🆗 Success! Your virtual environment has been set up, and a Jupyter Kernel has been loaded, named $DIR_BASE."
echo "Here us a list of all your `jupyter kernelspec list`"
echo "Keep calm and 🐍 on."
