import numpy as np
import matplotlib.pyplot as plt
import pandas as pd 

import os
os.environ['CUDA_VISIBLE_DEVICES'] = '1'

print("start")

pfPath = "./2020dataPackage/pf/"

pf = pd.read_csv(pfPath+"fpopl_seoul_06.csv")
del pf['etl_dt']
del pf['etl_date']
pf_H = pf['adstrd_code']
pf_HArray = pf_H.to_numpy()



for indexNumber, Hcode in zip(range(0,len(pf_HArray)), pf_HArray):
    temp = Hcode
    temp = str(temp)
    temp = temp[0:4]
    pf_HArray[indexNumber] = int(temp)

pf['adstrd_code']=pf_HArray



pf.to_csv("./slicH_202006.csv")


print("end")