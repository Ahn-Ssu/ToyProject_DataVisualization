import numpy as np
import matplotlib.pyplot as plt
import pandas as pd 

import os
os.environ['CUDA_VISIBLE_DEVICES'] = '2'

print("start")


pfPath = "./2020dataPackage/pf/"

H_dataSet = pd.read_csv(pfPath+"slicH.csv", encoding = 'CP949')
targetPF = pd.read_csv(pfPath+"slicH_202006.csv",encoding='ISO-8859-1')

pf_H = targetPF['adstrd_code']
pf_HArray = pf_H.to_numpy()

H_code, H_province, H_city = H_dataSet['행정동코드'],H_dataSet['시도명'],H_dataSet['시군구명']
province_info = []
city_info = []
nowLocation = 0 
for H in pf_HArray:
    for indexNumber in range(0,len(H_code)):
        if H == H_code[indexNumber]:
            province_info.append(H_province[indexNumber])
            city_info.append(H_city[indexNumber])
            break
    if (nowLocation % 5000)==0 :
        print("now = {:>10}, | current ratio ==> {:>5}".format(nowLocation, round(nowLocation/8760715,2)))
        
    nowLocation = nowLocation + 1
    
print("append end")

targetPF['province'] = province_info
targetPF['city'] = city_info


targetPF.to_csv("./concatH_202006.csv", encoding='ms949')

print("end")