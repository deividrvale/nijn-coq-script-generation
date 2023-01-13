#!/bin/bash
# Terminal colors
BLUE='\033[1;34m'
PURPLE='\033[1;35m'
RED='\033[1;31m'
GREEN='\033[1;32m'
NC='\033[0m'

EXP_PATH="./experiments/"
CERT_PATH="./$EXP_PATH/ho_poly_certificates"

# Delete old certificates
rm -rf ./$EXP_PATH/$CERT_PATH
rm -rf .nia.cache

# Create new certificate folder
mkdir -p $CERT_PATH

# Execute onijn on experiment data and generate the corresponding Coq certificates.
FILES="./$EXP_PATH/ho_poly/*"
for f in $FILES
do
    g=${f::${#f}-6}
    dune exec -- onijn $f -o $g.v
    mv $g.v "$CERT_PATH"
done

FILES="./$CERT_PATH/*.v"
fail=0
timeout=0
success=0
total=0

for f in $FILES
do
    ext=${f: -2}
    if [ $ext == ".v" ]
    then
    total=$((total+1))
    echo "Compiling proof script: $f"
    timeout 60s time coqc $f

    err=$?

    if [ $err == "0" ]
    then
        success=$((success+1))
        printf "${GREEN}success${NC}\n"
    else
        if [ $err == "1" ]
        then
            fail=$((fail+1))
            printf "${RED}fail${NC}\n"
            else
                if [ $err == "124" ]
                then
                    timeout=$((timeout+1))
                    printf "${PURPLE}timeout${NC}\n"
            fi
        fi
    fi
    fi
done

echo "Results"
echo "------------------------------------------------------------------------"
printf "${GREEN}Success:${NC} $success\n"
printf "${RED}Fail:${NC} $fail\n"
printf "${PURPLE}Timeout:${NC} $timeout\n"
printf "${BLUE}Total:${NC} $total\n"
echo "------------------------------------------------------------------------"
