#!/bin/bash
seq 1 1000 | parallel -j 100 Rscript FINAL_CODE.R {}

