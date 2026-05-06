# ============================================================
# Script 01: Data Loading and Structure Check
# Project: Publication Trajectories of I/O Psychology Faculty
# Author: Soup
# Date: May 2026
# ============================================================

library(tidyverse)
library(readxl)
library(tidyLPA)

# Load data
df <- read_excel("/Users/soupornoghosh/Desktop/IMT 600/OpenAlex/Faculty Details.xlsx")

# Check structure
dim(df)
names(df)