#!/bin/bash
###############################################################################
# Fix issues with the source data.
###############################################################################

if ! command -v sed &> /dev/null
then
    echo "ERROR: sed could not be found!"
    exit 1
fi

DATA_DIR="/Documents/data/sustainable_landscapes_brazil/Forest_Inventory_Brazil_2007"

FILE1=${DATA_DIR}/data/FN_A01_2015_Inventory.csv
if [ -f "$FILE1" ]; then
        sed -i 's/;/,/g' ${FILE1}
    else 
        echo "ERROR: File not found: $FILE1"
        exit 1
fi

FILE2=${DATA_DIR}/data/SFX_A03_2012_Inventory.csv
if [ -f "$FILE2" ]; then
        sed -i 's/;/,/g' ${FILE2}
    else 
        echo "ERROR: File not found: $FILE2"
        exit 1
fi

exit 0
